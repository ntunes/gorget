use std::fs;
use std::path::Path;

// ── Trace event types ────────────────────────────────────────

#[derive(Debug)]
pub enum TraceEvent {
    TestStart { name: String },
    TestEnd { name: String, status: String, duration_ms: i64 },
    Call { function: String, args: String, depth: i32 },
    Return { function: String, value: String, depth: i32 },
    Loop { kind: String, detail: String, depth: i32 },
    Stmt { kind: String, name: String, value: String, depth: i32 },
    Branch { kind: String, depth: i32 },
}

// ── Tree structure ───────────────────────────────────────────

#[derive(Debug)]
struct TraceNode {
    event: TraceEvent,
    children: Vec<TraceNode>,
}

// ── Trace parser (hand-rolled, no serde) ─────────────────────

/// Extract a JSON string value for a given key from a line.
/// e.g. extract_str(line, "name") on `"name":"foo"` returns Some("foo").
fn extract_str(line: &str, key: &str) -> Option<String> {
    let needle = format!("\"{}\":\"", key);
    let start = line.find(&needle)? + needle.len();
    let rest = &line[start..];
    // Find the closing quote, handling escaped quotes
    let mut end = 0;
    let bytes = rest.as_bytes();
    while end < bytes.len() {
        if bytes[end] == b'\\' {
            end += 2; // skip escaped char
        } else if bytes[end] == b'"' {
            break;
        } else {
            end += 1;
        }
    }
    Some(unescape_json_str(&rest[..end]))
}

/// Extract a JSON integer value for a given key.
fn extract_int(line: &str, key: &str) -> Option<i64> {
    // Try "key": (with colon and possible space)
    let needle = format!("\"{}\":", key);
    let start = line.find(&needle)? + needle.len();
    let rest = line[start..].trim_start();
    // Parse digits (possibly negative)
    let end = rest.find(|c: char| !c.is_ascii_digit() && c != '-').unwrap_or(rest.len());
    rest[..end].parse().ok()
}

/// Extract the args object as a raw string representation.
fn extract_args(line: &str) -> String {
    let needle = "\"args\":{";
    let Some(start) = line.find(needle) else { return String::new() };
    let start = start + needle.len();
    let rest = &line[start..];
    // Find matching closing brace (simple — args don't contain nested objects)
    let end = rest.find('}').unwrap_or(rest.len());
    let raw = &rest[..end];
    // Convert JSON key-value pairs to readable format: "a":1,"b":"hi" -> a=1, b="hi"
    if raw.is_empty() {
        return String::new();
    }
    let mut result = String::new();
    let mut chars = raw.chars().peekable();
    while chars.peek().is_some() {
        // Skip leading quote for key
        if chars.peek() == Some(&'"') {
            chars.next();
        }
        // Read key
        let key: String = chars.by_ref().take_while(|&c| c != '"').collect();
        // Skip colon
        if chars.peek() == Some(&':') {
            chars.next();
        }
        if !result.is_empty() {
            result.push_str(", ");
        }
        result.push_str(&key);
        result.push('=');
        // Read value until comma or end
        let mut val = String::new();
        let mut in_str = false;
        while let Some(&c) = chars.peek() {
            if c == '"' {
                in_str = !in_str;
                val.push(c);
                chars.next();
            } else if c == '\\' && in_str {
                val.push(c);
                chars.next();
                if let Some(&esc) = chars.peek() {
                    val.push(esc);
                    chars.next();
                }
            } else if c == ',' && !in_str {
                chars.next();
                break;
            } else {
                val.push(c);
                chars.next();
            }
        }
        result.push_str(&val);
    }
    result
}

/// Extract the value field (can be a number, string, bool, or null).
fn extract_value(line: &str) -> String {
    let needle = "\"value\":";
    let Some(start) = line.find(needle) else { return String::new() };
    let start = start + needle.len();
    let rest = &line[start..];
    if rest.starts_with('"') {
        // String value
        extract_str(line, "value").unwrap_or_default()
    } else if rest.starts_with("null") {
        "void".to_string()
    } else if rest.starts_with("true") {
        "true".to_string()
    } else if rest.starts_with("false") {
        "false".to_string()
    } else {
        // Numeric
        let end = rest.find(|c: char| !c.is_ascii_digit() && c != '-' && c != '.').unwrap_or(rest.len());
        rest[..end].to_string()
    }
}

fn unescape_json_str(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(other) => { result.push('\\'); result.push(other); }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}

fn parse_trace_line(line: &str) -> Option<TraceEvent> {
    let line = line.trim();
    if line.is_empty() || !line.starts_with('{') {
        return None;
    }

    if line.contains("\"type\":\"test_start\"") {
        let name = extract_str(line, "name")?;
        Some(TraceEvent::TestStart { name })
    } else if line.contains("\"type\":\"test_end\"") {
        let name = extract_str(line, "name")?;
        let status = extract_str(line, "status").unwrap_or_else(|| "unknown".to_string());
        let duration_ms = extract_int(line, "duration_ms").unwrap_or(0);
        Some(TraceEvent::TestEnd { name, status, duration_ms })
    } else if line.contains("\"type\":\"call\"") {
        let function = extract_str(line, "fn")?;
        let args = extract_args(line);
        let depth = extract_int(line, "depth").unwrap_or(0) as i32;
        Some(TraceEvent::Call { function, args, depth })
    } else if line.contains("\"type\":\"return\"") {
        let function = extract_str(line, "fn")?;
        let value = extract_value(line);
        let depth = extract_int(line, "depth").unwrap_or(0) as i32;
        Some(TraceEvent::Return { function, value, depth })
    } else if line.contains("\"type\":\"loop\"") {
        let kind = extract_str(line, "kind").unwrap_or_else(|| "loop".to_string());
        let depth = extract_int(line, "depth").unwrap_or(0) as i32;
        let detail = if kind == "for" {
            let var = extract_str(line, "var").unwrap_or_default();
            let value = extract_value(line);
            format!("{var}={value}")
        } else {
            let iter = extract_int(line, "iter").unwrap_or(0);
            format!("iter {iter}")
        };
        Some(TraceEvent::Loop { kind, detail, depth })
    } else if line.contains("\"type\":\"stmt\"") {
        let kind = extract_str(line, "kind").unwrap_or_else(|| "stmt".to_string());
        let name = extract_str(line, "name").unwrap_or_default();
        let value = extract_value(line);
        let depth = extract_int(line, "depth").unwrap_or(0) as i32;
        Some(TraceEvent::Stmt { kind, name, value, depth })
    } else if line.contains("\"type\":\"branch\"") {
        let kind = extract_str(line, "kind").unwrap_or_else(|| "if".to_string());
        let depth = extract_int(line, "depth").unwrap_or(0) as i32;
        Some(TraceEvent::Branch { kind, depth })
    } else {
        None
    }
}

fn parse_trace_file(path: &Path) -> Result<Vec<TraceEvent>, String> {
    let content = fs::read_to_string(path)
        .map_err(|e| format!("Cannot read trace file '{}': {e}", path.display()))?;
    let events: Vec<TraceEvent> = content.lines().filter_map(parse_trace_line).collect();
    Ok(events)
}

// ── Tree builder ─────────────────────────────────────────────

/// Get the depth of an event (returns -1 for events without depth).
fn event_depth(event: &TraceEvent) -> i32 {
    match event {
        TraceEvent::Call { depth, .. }
        | TraceEvent::Return { depth, .. }
        | TraceEvent::Loop { depth, .. }
        | TraceEvent::Stmt { depth, .. }
        | TraceEvent::Branch { depth, .. } => *depth,
        _ => -1,
    }
}

/// Build a tree of TraceNodes from a flat list of events.
///
/// Call/Return pairs form parent-child relationships: everything between
/// a Call and its matching Return becomes a child of the Call node.
/// Loop events become expandable nodes: events until the next Loop at the
/// same depth (or end of call scope) become children.
fn build_tree(events: Vec<TraceEvent>) -> Vec<TraceNode> {
    // Stack frames: (opener event or None for root, depth, is_loop, children)
    let mut stack: Vec<(Option<TraceEvent>, i32, bool, Vec<TraceNode>)> =
        vec![(None, -1, false, Vec::new())];

    for event in events {
        let ed = event_depth(&event);
        let is_loop_event = matches!(&event, TraceEvent::Loop { .. });

        // Close loop frames that should end before this event.
        // A loop frame closes when:
        //   - A new Loop arrives at the same depth (new iteration)
        //   - Any event arrives at a lower depth (left the loop scope)
        while stack.len() > 1 {
            let (_, frame_depth, is_loop, _) = stack.last().unwrap();
            if !is_loop {
                break;
            }
            let should_close =
                ed < *frame_depth || (ed == *frame_depth && is_loop_event);
            if !should_close {
                break;
            }
            let (opener, _, _, children) = stack.pop().unwrap();
            if let Some(ev) = opener {
                stack.last_mut().unwrap().3.push(TraceNode {
                    event: ev,
                    children,
                });
            }
        }

        // Process the event itself.
        match &event {
            TraceEvent::Call { .. } => {
                let d = ed;
                stack.push((Some(event), d, false, Vec::new()));
            }
            TraceEvent::Return { .. } => {
                if stack.len() > 1 {
                    let (opener, _, _, mut children) = stack.pop().unwrap();
                    // Add Return as last child of the Call
                    children.push(TraceNode {
                        event,
                        children: Vec::new(),
                    });
                    if let Some(call_ev) = opener {
                        stack.last_mut().unwrap().3.push(TraceNode {
                            event: call_ev,
                            children,
                        });
                    }
                } else {
                    // Orphan return — add as leaf
                    stack.last_mut().unwrap().3.push(TraceNode {
                        event,
                        children: Vec::new(),
                    });
                }
            }
            TraceEvent::Loop { .. } => {
                let d = ed;
                stack.push((Some(event), d, true, Vec::new()));
            }
            _ => {
                stack.last_mut().unwrap().3.push(TraceNode {
                    event,
                    children: Vec::new(),
                });
            }
        }
    }

    // Drain remaining unclosed frames.
    while stack.len() > 1 {
        let (opener, _, _, children) = stack.pop().unwrap();
        if let Some(ev) = opener {
            stack.last_mut().unwrap().3.push(TraceNode {
                event: ev,
                children,
            });
        }
    }

    stack.pop().map(|(_, _, _, c)| c).unwrap_or_default()
}

// ── Report builder ───────────────────────────────────────────

struct TestResult {
    name: String,
    status: String,
    duration_ms: i64,
    tree: Vec<TraceNode>,
}

struct ReportData {
    tests: Vec<TestResult>,
    total_passed: usize,
    total_failed: usize,
    total_duration_ms: i64,
}

fn build_report(events: Vec<TraceEvent>) -> ReportData {
    let mut tests = Vec::new();
    let mut current_name: Option<String> = None;
    let mut current_events: Vec<TraceEvent> = Vec::new();

    for event in events {
        match &event {
            TraceEvent::TestStart { name } => {
                current_name = Some(name.clone());
                current_events.clear();
            }
            TraceEvent::TestEnd { name, status, duration_ms } => {
                let tree = build_tree(std::mem::take(&mut current_events));
                tests.push(TestResult {
                    name: name.clone(),
                    status: status.clone(),
                    duration_ms: *duration_ms,
                    tree,
                });
                current_name = None;
            }
            _ => {
                if current_name.is_some() {
                    current_events.push(event);
                }
            }
        }
    }

    let total_passed = tests.iter().filter(|t| t.status == "pass").count();
    let total_failed = tests.iter().filter(|t| t.status != "pass").count();
    let total_duration_ms = tests.iter().map(|t| t.duration_ms).sum();

    ReportData {
        tests,
        total_passed,
        total_failed,
        total_duration_ms,
    }
}

// ── HTML generator ───────────────────────────────────────────

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}

/// Render the label for a single event (without tree chrome).
fn render_node_label(event: &TraceEvent) -> String {
    match event {
        TraceEvent::Call { function, args, .. } => {
            let args_display = if args.is_empty() {
                "()".to_string()
            } else {
                format!("({})", html_escape(args))
            };
            format!(
                "<span class=\"event-icon call-icon\">&#x2192;</span> \
                 <span class=\"fn-name\">{}</span>{}",
                html_escape(function),
                args_display,
            )
        }
        TraceEvent::Return { function, value, .. } => {
            let val_display = if value == "void" || value.is_empty() {
                String::new()
            } else {
                format!(" <span class=\"ret-value\">&rarr; {}</span>", html_escape(value))
            };
            format!(
                "<span class=\"event-icon return-icon\">&#x2190;</span> \
                 <span class=\"fn-name\">{}</span>{}",
                html_escape(function),
                val_display,
            )
        }
        TraceEvent::Loop { kind, detail, .. } => {
            format!(
                "<span class=\"event-icon loop-icon\">&#x21BB;</span> \
                 <span class=\"loop-kw\">{}</span> {}",
                html_escape(kind),
                html_escape(detail),
            )
        }
        TraceEvent::Stmt { kind, name, value, .. } => {
            if kind == "assert" {
                "<span class=\"event-icon stmt-icon\">&#x2714;</span> \
                 <span class=\"stmt-kw\">assert</span>"
                    .to_string()
            } else if kind == "let" {
                format!(
                    "<span class=\"event-icon stmt-icon\">=</span> \
                     <span class=\"stmt-kw\">let</span> \
                     <span class=\"var-name\">{}</span> = \
                     <span class=\"lit-value\">{}</span>",
                    html_escape(name),
                    html_escape(value),
                )
            } else {
                // assign
                format!(
                    "<span class=\"event-icon stmt-icon\">=</span> \
                     <span class=\"var-name\">{}</span> = \
                     <span class=\"lit-value\">{}</span>",
                    html_escape(name),
                    html_escape(value),
                )
            }
        }
        TraceEvent::Branch { kind, .. } => {
            format!(
                "<span class=\"event-icon branch-icon\">&#x2387;</span> \
                 <span class=\"branch-kw\">{}</span>",
                html_escape(kind),
            )
        }
        _ => String::new(),
    }
}

/// Recursively render a tree of TraceNodes as HTML.
/// `id_counter` is used to generate unique IDs for expandable nodes.
fn render_tree_html(nodes: &[TraceNode], id_counter: &mut usize) -> String {
    let mut html = String::new();
    for node in nodes {
        let has_children = !node.children.is_empty();
        let is_expandable = has_children
            && matches!(
                &node.event,
                TraceEvent::Call { .. } | TraceEvent::Loop { .. }
            );

        if is_expandable {
            let id = *id_counter;
            *id_counter += 1;
            html.push_str(&format!(
                "<div class=\"tree-row expandable\" onclick=\"ttoggle({id})\">\
                 <span class=\"tree-toggle\" id=\"tbtn-{id}\">&#x25B6;</span> \
                 {}</div>\n",
                render_node_label(&node.event),
            ));
            html.push_str(&format!(
                "<div class=\"tree-children\" id=\"tc-{id}\">\n",
            ));
            html.push_str(&render_tree_html(&node.children, id_counter));
            html.push_str("</div>\n");
        } else {
            html.push_str(&format!(
                "<div class=\"tree-row leaf\">{}</div>\n",
                render_node_label(&node.event),
            ));
        }
    }
    html
}

pub fn generate_html_report(trace_path: &Path, output_path: &Path) -> Result<(), String> {
    let events = parse_trace_file(trace_path)?;
    let report = build_report(events);

    let pass_pct = if report.tests.is_empty() {
        0.0
    } else {
        (report.total_passed as f64 / report.tests.len() as f64) * 100.0
    };

    let mut html = String::new();

    // Header
    html.push_str("<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n");
    html.push_str("<title>Gorget Test Report</title>\n");
    html.push_str("<style>\n");
    html.push_str(REPORT_CSS);
    html.push_str("</style>\n</head>\n<body>\n");

    // Summary
    html.push_str("<div class=\"report-header\">\n");
    html.push_str("<h1>Test Report</h1>\n");
    html.push_str(&format!(
        "<div class=\"summary\">\
         <span class=\"pass-count\">{} passed</span>, \
         <span class=\"fail-count\">{} failed</span> \
         &mdash; {:.0}% &mdash; {}ms total</div>\n",
        report.total_passed, report.total_failed, pass_pct, report.total_duration_ms,
    ));

    // Pass rate bar
    html.push_str("<div class=\"bar-container\">\n");
    html.push_str(&format!(
        "<div class=\"bar-fill{}\" style=\"width:{:.1}%\"></div>\n",
        if report.total_failed > 0 { " bar-has-fail" } else { "" },
        pass_pct,
    ));
    html.push_str("</div>\n");
    html.push_str("</div>\n");

    // Global tree-node counter for unique IDs
    let mut tree_id_counter: usize = 0;

    // Test table
    html.push_str("<div class=\"test-list\">\n");
    for (i, test) in report.tests.iter().enumerate() {
        let is_pass = test.status == "pass";
        let status_class = if is_pass { "status-pass" } else { "status-fail" };
        let status_label = if is_pass { "PASS" } else { "FAIL" };
        let status_icon = if is_pass { "&#x2713;" } else { "&#x2717;" };
        let has_events = !test.tree.is_empty();

        html.push_str(&format!(
            "<div class=\"test-row {}\">\n",
            status_class,
        ));
        html.push_str(&format!(
            "<div class=\"test-header\" {}>\n",
            if has_events { format!("onclick=\"toggle({i})\"") } else { String::new() },
        ));
        html.push_str(&format!(
            "<span class=\"status-badge {status_class}\">{status_icon} {status_label}</span> \
             <span class=\"test-name\">{}</span> \
             <span class=\"test-duration\">{}ms</span>",
            html_escape(&test.name),
            test.duration_ms,
        ));
        if has_events {
            html.push_str(&format!(
                " <span class=\"expand-btn\" id=\"btn-{i}\">&#x25B6;</span>",
            ));
        }
        html.push_str("\n</div>\n");

        // Expandable trace detail (tree-structured)
        if has_events {
            html.push_str(&format!(
                "<div class=\"trace-detail\" id=\"detail-{i}\">\n",
            ));
            html.push_str(&render_tree_html(&test.tree, &mut tree_id_counter));
            html.push_str("</div>\n");
        }

        html.push_str("</div>\n");
    }
    html.push_str("</div>\n");

    // JavaScript
    html.push_str("<script>\n");
    html.push_str(REPORT_JS);
    html.push_str("</script>\n");
    html.push_str("</body>\n</html>\n");

    fs::write(output_path, &html)
        .map_err(|e| format!("Cannot write report '{}': {e}", output_path.display()))?;

    Ok(())
}

// ── Inline CSS ───────────────────────────────────────────────

const REPORT_CSS: &str = r#"
* { margin: 0; padding: 0; box-sizing: border-box; }
body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    background: #1a1a2e; color: #e0e0e0; padding: 24px; max-width: 900px; margin: 0 auto;
}
h1 { font-size: 1.5rem; margin-bottom: 8px; color: #fff; }
.report-header { margin-bottom: 24px; }
.summary { font-size: 1.1rem; margin-bottom: 12px; }
.pass-count { color: #4caf50; font-weight: bold; }
.fail-count { color: #f44336; font-weight: bold; }
.bar-container {
    background: #333; border-radius: 4px; height: 8px; overflow: hidden;
}
.bar-fill {
    background: #4caf50; height: 100%; border-radius: 4px; transition: width 0.3s;
}
.bar-has-fail { background: linear-gradient(90deg, #4caf50 0%, #4caf50 100%); }
.test-list { display: flex; flex-direction: column; gap: 2px; }
.test-row {
    background: #16213e; border-radius: 6px; overflow: hidden;
    border-left: 4px solid transparent;
}
.test-row.status-pass { border-left-color: #4caf50; }
.test-row.status-fail { border-left-color: #f44336; }
.test-header {
    display: flex; align-items: center; gap: 12px; padding: 10px 16px;
    cursor: pointer; user-select: none;
}
.test-header:hover { background: #1a2744; }
.status-badge {
    font-size: 0.75rem; font-weight: bold; padding: 2px 8px;
    border-radius: 3px; min-width: 60px; text-align: center; display: inline-block;
}
.status-badge.status-pass { background: #1b3a1b; color: #4caf50; }
.status-badge.status-fail { background: #3a1b1b; color: #f44336; }
.test-name { flex: 1; }
.test-duration { color: #888; font-size: 0.85rem; font-variant-numeric: tabular-nums; }
.expand-btn {
    color: #666; font-size: 0.7rem; transition: transform 0.2s; display: inline-block;
}
.expand-btn.open { transform: rotate(90deg); }
.trace-detail {
    display: none; padding: 8px 16px 12px 32px; background: #0f1629;
    border-top: 1px solid #222; font-family: "SFMono-Regular", Consolas, monospace;
    font-size: 0.8rem; line-height: 1.6;
}
.trace-detail.open { display: block; }
/* Tree structure */
.tree-row { white-space: nowrap; padding: 1px 0; }
.tree-row.expandable { cursor: pointer; }
.tree-row.expandable:hover { background: rgba(255,255,255,0.03); }
.tree-row.leaf { padding-left: 18px; }
.tree-toggle {
    display: inline-block; width: 14px; font-size: 0.6rem; color: #666;
    transition: transform 0.15s; text-align: center; margin-right: 2px;
}
.tree-toggle.open { transform: rotate(90deg); }
.tree-children { display: none; padding-left: 20px; }
.tree-children.open { display: block; }
.event-icon { display: inline-block; width: 16px; text-align: center; }
.call-icon { color: #64b5f6; }
.return-icon { color: #81c784; }
.loop-icon { color: #ffb74d; }
.stmt-icon { color: #90a4ae; }
.branch-icon { color: #ba68c8; }
.fn-name { color: #ce93d8; }
.ret-value { color: #aed581; }
.loop-kw { color: #ffb74d; font-weight: bold; }
.stmt-kw { color: #90a4ae; }
.var-name { color: #80cbc4; }
.lit-value { color: #fff176; }
.branch-kw { color: #ba68c8; font-weight: bold; }
"#;

// ── Inline JS ────────────────────────────────────────────────

const REPORT_JS: &str = r#"
function toggle(i) {
    var detail = document.getElementById('detail-' + i);
    var btn = document.getElementById('btn-' + i);
    if (detail) {
        detail.classList.toggle('open');
        if (btn) btn.classList.toggle('open');
    }
}
function ttoggle(id) {
    var el = document.getElementById('tc-' + id);
    var btn = document.getElementById('tbtn-' + id);
    if (el) {
        el.classList.toggle('open');
        if (btn) btn.classList.toggle('open');
    }
    event.stopPropagation();
}
"#;

// ── Unit tests ───────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_test_start() {
        let line = r#"{"type":"test_start","name":"addition works"}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::TestStart { name } => assert_eq!(name, "addition works"),
            _ => panic!("expected TestStart"),
        }
    }

    #[test]
    fn parse_test_end() {
        let line = r#"{"type":"test_end","name":"addition works","status":"pass","duration_ms":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::TestEnd { name, status, duration_ms } => {
                assert_eq!(name, "addition works");
                assert_eq!(status, "pass");
                assert_eq!(duration_ms, 0);
            }
            _ => panic!("expected TestEnd"),
        }
    }

    #[test]
    fn parse_call_event() {
        let line = r#"{"type":"call","fn":"factorial","args":{"n":3},"depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Call { function, args, depth } => {
                assert_eq!(function, "factorial");
                assert!(args.contains("n=3"));
                assert_eq!(depth, 0);
            }
            _ => panic!("expected Call"),
        }
    }

    #[test]
    fn parse_return_event() {
        let line = r#"{"type":"return","fn":"factorial","value":6,"depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Return { function, value, depth } => {
                assert_eq!(function, "factorial");
                assert_eq!(value, "6");
                assert_eq!(depth, 0);
            }
            _ => panic!("expected Return"),
        }
    }

    #[test]
    fn parse_loop_while() {
        let line = r#"{"type":"loop","kind":"while","iter":5,"depth":1}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Loop { kind, detail, depth } => {
                assert_eq!(kind, "while");
                assert_eq!(detail, "iter 5");
                assert_eq!(depth, 1);
            }
            _ => panic!("expected Loop"),
        }
    }

    #[test]
    fn parse_loop_for() {
        let line = r#"{"type":"loop","kind":"for","var":"i","value":3,"depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Loop { kind, detail, depth } => {
                assert_eq!(kind, "for");
                assert_eq!(detail, "i=3");
                assert_eq!(depth, 0);
            }
            _ => panic!("expected Loop"),
        }
    }

    #[test]
    fn parse_return_null() {
        let line = r#"{"type":"return","fn":"main","value":null,"depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Return { value, .. } => assert_eq!(value, "void"),
            _ => panic!("expected Return"),
        }
    }

    #[test]
    fn parse_return_string() {
        let line = r#"{"type":"return","fn":"greet","value":"hello","depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Return { value, .. } => assert_eq!(value, "hello"),
            _ => panic!("expected Return"),
        }
    }

    #[test]
    fn parse_stmt_event() {
        let line = r#"{"type":"stmt","kind":"let","name":"x","value":42,"depth":1}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Stmt { kind, name, value, depth } => {
                assert_eq!(kind, "let");
                assert_eq!(name, "x");
                assert_eq!(value, "42");
                assert_eq!(depth, 1);
            }
            _ => panic!("expected Stmt"),
        }
    }

    #[test]
    fn parse_stmt_assign() {
        let line = r#"{"type":"stmt","kind":"assign","name":"total","value":15,"depth":1}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Stmt { kind, name, value, depth } => {
                assert_eq!(kind, "assign");
                assert_eq!(name, "total");
                assert_eq!(value, "15");
                assert_eq!(depth, 1);
            }
            _ => panic!("expected Stmt"),
        }
    }

    #[test]
    fn parse_stmt_assert() {
        let line = r#"{"type":"stmt","kind":"assert","depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Stmt { kind, depth, .. } => {
                assert_eq!(kind, "assert");
                assert_eq!(depth, 0);
            }
            _ => panic!("expected Stmt"),
        }
    }

    #[test]
    fn parse_branch_event() {
        let line = r#"{"type":"branch","kind":"if","depth":1}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Branch { kind, depth } => {
                assert_eq!(kind, "if");
                assert_eq!(depth, 1);
            }
            _ => panic!("expected Branch"),
        }
    }

    #[test]
    fn parse_branch_else() {
        let line = r#"{"type":"branch","kind":"else","depth":2}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Branch { kind, depth } => {
                assert_eq!(kind, "else");
                assert_eq!(depth, 2);
            }
            _ => panic!("expected Branch"),
        }
    }

    #[test]
    fn build_tree_call_return_pair() {
        let events = vec![
            TraceEvent::Call { function: "add".into(), args: "a=1".into(), depth: 0 },
            TraceEvent::Stmt { kind: "let".into(), name: "r".into(), value: "3".into(), depth: 1 },
            TraceEvent::Return { function: "add".into(), value: "3".into(), depth: 0 },
        ];
        let tree = build_tree(events);
        // Should produce one Call node with two children: Stmt + Return
        assert_eq!(tree.len(), 1);
        assert!(matches!(&tree[0].event, TraceEvent::Call { function, .. } if function == "add"));
        assert_eq!(tree[0].children.len(), 2);
        assert!(matches!(&tree[0].children[0].event, TraceEvent::Stmt { kind, .. } if kind == "let"));
        assert!(matches!(&tree[0].children[1].event, TraceEvent::Return { .. }));
    }

    #[test]
    fn build_tree_nested_calls() {
        let events = vec![
            TraceEvent::Call { function: "outer".into(), args: String::new(), depth: 0 },
            TraceEvent::Call { function: "inner".into(), args: String::new(), depth: 1 },
            TraceEvent::Return { function: "inner".into(), value: "1".into(), depth: 1 },
            TraceEvent::Return { function: "outer".into(), value: "2".into(), depth: 0 },
        ];
        let tree = build_tree(events);
        assert_eq!(tree.len(), 1); // outer
        assert_eq!(tree[0].children.len(), 2); // inner call node + outer return
        assert!(matches!(&tree[0].children[0].event, TraceEvent::Call { function, .. } if function == "inner"));
        assert_eq!(tree[0].children[0].children.len(), 1); // inner return
    }

    #[test]
    fn build_tree_loop_groups_children() {
        let events = vec![
            TraceEvent::Call { function: "sum".into(), args: String::new(), depth: 0 },
            TraceEvent::Stmt { kind: "let".into(), name: "t".into(), value: "0".into(), depth: 1 },
            TraceEvent::Loop { kind: "for".into(), detail: "i=1".into(), depth: 1 },
            TraceEvent::Stmt { kind: "assign".into(), name: "t".into(), value: "1".into(), depth: 1 },
            TraceEvent::Loop { kind: "for".into(), detail: "i=2".into(), depth: 1 },
            TraceEvent::Stmt { kind: "assign".into(), name: "t".into(), value: "3".into(), depth: 1 },
            TraceEvent::Return { function: "sum".into(), value: "3".into(), depth: 0 },
        ];
        let tree = build_tree(events);
        // Should be: Call(sum) with children: [let, Loop(i=1, [assign]), Loop(i=2, [assign]), Return]
        assert_eq!(tree.len(), 1);
        let call_children = &tree[0].children;
        assert_eq!(call_children.len(), 4); // let + loop1 + loop2 + return
        // Loop 1 should have one child (the assign)
        assert!(matches!(&call_children[1].event, TraceEvent::Loop { detail, .. } if detail == "i=1"));
        assert_eq!(call_children[1].children.len(), 1);
        // Loop 2 should have one child (the assign)
        assert!(matches!(&call_children[2].event, TraceEvent::Loop { detail, .. } if detail == "i=2"));
        assert_eq!(call_children[2].children.len(), 1);
    }

    #[test]
    fn build_tree_flat_stmts() {
        // Events without calls should remain flat
        let events = vec![
            TraceEvent::Stmt { kind: "assert".into(), name: String::new(), value: String::new(), depth: 0 },
            TraceEvent::Stmt { kind: "assert".into(), name: String::new(), value: String::new(), depth: 0 },
        ];
        let tree = build_tree(events);
        assert_eq!(tree.len(), 2);
        assert!(tree[0].children.is_empty());
        assert!(tree[1].children.is_empty());
    }

    #[test]
    fn build_report_groups_by_test() {
        let events = vec![
            TraceEvent::TestStart { name: "test_a".to_string() },
            TraceEvent::Call { function: "add".to_string(), args: "a=1, b=2".to_string(), depth: 0 },
            TraceEvent::Return { function: "add".to_string(), value: "3".to_string(), depth: 0 },
            TraceEvent::TestEnd { name: "test_a".to_string(), status: "pass".to_string(), duration_ms: 1 },
            TraceEvent::TestStart { name: "test_b".to_string() },
            TraceEvent::TestEnd { name: "test_b".to_string(), status: "fail".to_string(), duration_ms: 2 },
        ];
        let report = build_report(events);
        assert_eq!(report.tests.len(), 2);
        assert_eq!(report.total_passed, 1);
        assert_eq!(report.total_failed, 1);
        assert_eq!(report.total_duration_ms, 3);
        assert_eq!(report.tests[0].tree.len(), 1); // one Call node (with Return as child)
        assert_eq!(report.tests[1].tree.len(), 0);
    }

    #[test]
    fn extract_args_parses_correctly() {
        let line = r#"{"type":"call","fn":"add","args":{"a":1,"b":2},"depth":0}"#;
        let args = extract_args(line);
        assert!(args.contains("a=1"));
        assert!(args.contains("b=2"));
    }

    #[test]
    fn extract_args_with_string_value() {
        let line = r#"{"type":"call","fn":"greet","args":{"name":"Alice"},"depth":0}"#;
        let args = extract_args(line);
        assert!(args.contains("name="));
        assert!(args.contains("Alice"));
    }

    #[test]
    fn html_escape_works() {
        assert_eq!(html_escape("<b>test</b>"), "&lt;b&gt;test&lt;/b&gt;");
        assert_eq!(html_escape("a & b"), "a &amp; b");
    }
}
