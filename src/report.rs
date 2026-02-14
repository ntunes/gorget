use std::fs;
use std::path::Path;
use serde_json::Value;

// ── Trace event types ────────────────────────────────────────

#[derive(Debug)]
pub enum TraceEvent {
    TestStart { name: String },
    TestEnd { name: String, status: String, duration_ms: i64 },
    Call { function: String, args: Value, depth: i32 },
    Return { function: String, depth: i32 },
    Loop { kind: String, detail: String, depth: i32 },
    StmtStart { src: String, vars: Value, depth: i32 },
    StmtEnd { depth: i32 },
    Branch { kind: String, depth: i32, src: String, vars: Value },
}

// ── Tree structure ───────────────────────────────────────────

#[derive(Debug)]
struct TraceNode {
    event: TraceEvent,
    children: Vec<TraceNode>,
}

// ── Trace parser ─────────────────────────────────────────────

/// Format a JSON value for display: null → "void", strings quoted, numbers/bools as-is.
fn format_json_value(v: &Value) -> String {
    match v {
        Value::Null => "void".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Number(n) => n.to_string(),
        Value::String(s) => format!("\"{s}\""),
        _ => v.to_string(),
    }
}

/// Format a JSON object as comma-separated key=value pairs for display.
/// e.g. {"a":1,"b":"hi"} → "a=1, b=\"hi\""
fn format_object_as_pairs(v: &Value) -> String {
    let Some(obj) = v.as_object() else { return String::new() };
    obj.iter()
        .map(|(k, v)| format!("{k}={}", format_json_value(v)))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Substitute variable names with their runtime values in source text.
/// Uses word-boundary matching to avoid replacing inside longer identifiers.
/// Returns `Some(substituted)` if any substitution occurred, `None` otherwise.
fn substitute_vars(src: &str, vars: &Value) -> Option<String> {
    let obj = match vars.as_object() {
        Some(m) if !m.is_empty() => m,
        _ => return None,
    };

    let pairs: Vec<(&str, String)> = obj
        .iter()
        .map(|(k, v)| (k.as_str(), format_json_value(v)))
        .collect();

    let mut result = src.to_string();
    let mut changed = false;

    for (name, value) in &pairs {
        let mut new_result = String::with_capacity(result.len());
        let mut chars = result.char_indices().peekable();

        while let Some((i, c)) = chars.next() {
            if result[i..].starts_with(name) {
                // Check word boundary before
                let before_ok = i == 0 || {
                    let prev = result[..i].chars().next_back().unwrap();
                    !prev.is_alphanumeric() && prev != '_'
                };
                // Check word boundary after
                let after_pos = i + name.len();
                let after_ok = after_pos >= result.len() || {
                    let next = result[after_pos..].chars().next().unwrap();
                    !next.is_alphanumeric() && next != '_'
                };

                if before_ok && after_ok {
                    new_result.push_str(value);
                    // Skip remaining chars of the matched name
                    while chars.peek().is_some_and(|&(j, _)| j < after_pos) {
                        chars.next();
                    }
                    changed = true;
                    continue;
                }
            }
            new_result.push(c);
        }
        result = new_result;
    }

    if changed { Some(result) } else { None }
}

fn parse_trace_line(line: &str) -> Option<TraceEvent> {
    let line = line.trim();
    if line.is_empty() || !line.starts_with('{') {
        return None;
    }

    let obj: Value = serde_json::from_str(line).ok()?;
    let typ = obj["type"].as_str()?;

    match typ {
        "test_start" => {
            let name = obj["name"].as_str()?.to_string();
            Some(TraceEvent::TestStart { name })
        }
        "test_end" => {
            let name = obj["name"].as_str()?.to_string();
            let status = obj["status"].as_str().unwrap_or("unknown").to_string();
            let duration_ms = obj["duration_ms"].as_i64().unwrap_or(0);
            Some(TraceEvent::TestEnd { name, status, duration_ms })
        }
        "call" => {
            let function = obj["fn"].as_str()?.to_string();
            let args = obj.get("args").cloned().unwrap_or(Value::Null);
            let depth = obj["depth"].as_i64().unwrap_or(0) as i32;
            Some(TraceEvent::Call { function, args, depth })
        }
        "return" => {
            let function = obj["fn"].as_str()?.to_string();
            let depth = obj["depth"].as_i64().unwrap_or(0) as i32;
            Some(TraceEvent::Return { function, depth })
        }
        "loop" => {
            let kind = obj["kind"].as_str().unwrap_or("loop").to_string();
            let depth = obj["depth"].as_i64().unwrap_or(0) as i32;
            let detail = if kind == "for" {
                let var = obj["var"].as_str().unwrap_or_default();
                let value = format_json_value(&obj["value"]);
                format!("{var}={value}")
            } else {
                let iter = obj["iter"].as_i64().unwrap_or(0);
                format!("iter {iter}")
            };
            Some(TraceEvent::Loop { kind, detail, depth })
        }
        "stmt_start" | "assert_start" => {
            let src = obj["src"].as_str().unwrap_or_default().to_string();
            let vars = obj.get("vars").cloned().unwrap_or(Value::Null);
            let depth = obj["depth"].as_i64().unwrap_or(0) as i32;
            Some(TraceEvent::StmtStart { src, vars, depth })
        }
        "stmt_end" | "assert_end" => {
            let depth = obj["depth"].as_i64().unwrap_or(0) as i32;
            Some(TraceEvent::StmtEnd { depth })
        }
        "branch" => {
            let kind = obj["kind"].as_str().unwrap_or("if").to_string();
            let depth = obj["depth"].as_i64().unwrap_or(0) as i32;
            let src = obj["src"].as_str().unwrap_or_default().to_string();
            let vars = obj.get("vars").cloned().unwrap_or(Value::Null);
            Some(TraceEvent::Branch { kind, depth, src, vars })
        }
        _ => None,
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
        | TraceEvent::StmtStart { depth, .. }
        | TraceEvent::StmtEnd { depth, .. }
        | TraceEvent::Branch { depth, .. } => *depth,
        _ => -1,
    }
}

/// Frame kind for the tree builder stack.
#[derive(Clone, Copy, PartialEq)]
enum FrameKind {
    /// Normal frame (Call, StmtStart) — closed explicitly by Return/StmtEnd.
    Normal,
    /// Loop iteration — auto-closed by next Loop at same depth or lower-depth event.
    Loop,
    /// Branch (if/elif/else) — auto-closed by next Branch at same depth or lower-depth event.
    Branch,
}

/// Build a tree of TraceNodes from a flat list of events.
///
/// Call/Return pairs form parent-child relationships: everything between
/// a Call and its matching Return becomes a child of the Call node.
/// Loop events become expandable nodes: events until the next Loop at the
/// same depth (or end of call scope) become children.
/// Branch events also become parent nodes: statements inside the taken
/// branch become children of the Branch node.
fn build_tree(events: Vec<TraceEvent>) -> Vec<TraceNode> {
    // Stack frames: (opener event or None for root, depth, kind, children)
    let mut stack: Vec<(Option<TraceEvent>, i32, FrameKind, Vec<TraceNode>)> =
        vec![(None, -1, FrameKind::Normal, Vec::new())];

    for event in events {
        let ed = event_depth(&event);
        let is_loop_event = matches!(&event, TraceEvent::Loop { .. });
        let is_branch_event = matches!(&event, TraceEvent::Branch { .. });

        // Close auto-closing frames (Loop, Branch) that should end before this event.
        // A Loop frame closes when a new Loop arrives at the same depth.
        // A Branch frame closes when a new Branch arrives at the same depth.
        // Both close when any event arrives at a lower depth.
        while stack.len() > 1 {
            let (_, frame_depth, kind, _) = stack.last().unwrap();
            let should_close = match kind {
                FrameKind::Normal => false,
                FrameKind::Loop => {
                    ed < *frame_depth || (ed == *frame_depth && is_loop_event)
                }
                FrameKind::Branch => {
                    ed < *frame_depth || (ed == *frame_depth && is_branch_event)
                }
            };
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
            TraceEvent::Call { .. } | TraceEvent::StmtStart { .. } => {
                let d = ed;
                stack.push((Some(event), d, FrameKind::Normal, Vec::new()));
            }
            TraceEvent::Return { .. } | TraceEvent::StmtEnd { .. } => {
                if stack.len() > 1 {
                    let (opener, _, _, children) = stack.pop().unwrap();
                    if let Some(opener_ev) = opener {
                        stack.last_mut().unwrap().3.push(TraceNode {
                            event: opener_ev,
                            children,
                        });
                    }
                }
            }
            TraceEvent::Loop { .. } => {
                let d = ed;
                stack.push((Some(event), d, FrameKind::Loop, Vec::new()));
            }
            TraceEvent::Branch { .. } => {
                let d = ed;
                stack.push((Some(event), d, FrameKind::Branch, Vec::new()));
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
            let is_empty = args.is_null()
                || args.as_object().map_or(true, |m| m.is_empty());
            let args_display = if is_empty {
                "()".to_string()
            } else {
                format!("({})", html_escape(&format_object_as_pairs(args)))
            };
            format!(
                "<span class=\"fn-name\">{}</span>{}",
                html_escape(function),
                args_display,
            )
        }
        TraceEvent::Return { .. } => String::new(), // structural only, not rendered
        TraceEvent::Loop { kind, detail, .. } => {
            format!(
                "<span class=\"loop-kw\">{}</span> {}",
                html_escape(kind),
                html_escape(detail),
            )
        }
        TraceEvent::StmtStart { src, .. } => {
            format!(
                "<span class=\"source-text\">{}</span>",
                html_escape(src),
            )
        }
        TraceEvent::StmtEnd { .. } => String::new(), // structural only
        TraceEvent::Branch { kind, src, .. } => {
            if !src.is_empty() {
                format!(
                    "<span class=\"source-text\">{}</span>",
                    html_escape(src),
                )
            } else {
                format!(
                    "<span class=\"branch-kw\">{}</span>",
                    html_escape(kind),
                )
            }
        }
        _ => String::new(),
    }
}

/// Compute variable substitution text for a node, if applicable.
fn node_substitution(event: &TraceEvent) -> Option<String> {
    match event {
        TraceEvent::StmtStart { src, vars, .. } => substitute_vars(src, vars),
        TraceEvent::Branch { src, vars, .. } => substitute_vars(src, vars),
        _ => None,
    }
}

/// Recursively render a tree of TraceNodes as HTML.
/// `id_counter` is used to generate unique IDs for expandable nodes.
/// `depth` controls character-based indentation (2ch per level).
fn render_tree_html(nodes: &[TraceNode], id_counter: &mut usize, depth: usize) -> String {
    let mut html = String::new();
    let pad = depth * 2;          // ch units for expandable rows (toggle occupies 2ch)
    let label_pad = pad + 3;      // ch units for leaf rows / subst text (toggle 2ch + space 1ch)

    for node in nodes {
        let has_children = !node.children.is_empty();
        let subst = node_substitution(&node.event);

        // A node is expandable if it has children OR a variable substitution
        let is_expandable = (has_children || subst.is_some())
            && matches!(
                &node.event,
                TraceEvent::Call { .. }
                    | TraceEvent::Loop { .. }
                    | TraceEvent::StmtStart { .. }
                    | TraceEvent::Branch { .. }
            );

        if is_expandable {
            let id = *id_counter;
            *id_counter += 1;
            html.push_str(&format!(
                "<div class=\"tree-row expandable\" style=\"padding-left:{pad}ch\" onclick=\"ttoggle({id})\">\
                 <span class=\"tree-toggle\" id=\"tbtn-{id}\">&#x25B6;</span> \
                 {}</div>\n",
                render_node_label(&node.event),
            ));
            html.push_str(&format!(
                "<div class=\"tree-children\" id=\"tc-{id}\">\n",
            ));
            if let Some(ref subst_text) = subst {
                html.push_str(&format!(
                    "<div class=\"subst-text\" style=\"padding-left:{label_pad}ch\">{}</div>\n",
                    html_escape(subst_text),
                ));
            }
            html.push_str(&render_tree_html(&node.children, id_counter, depth + 1));
            html.push_str("</div>\n");
        } else {
            html.push_str(&format!(
                "<div class=\"tree-row leaf\" style=\"padding-left:{label_pad}ch\">{}</div>\n",
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
            html.push_str(&render_tree_html(&test.tree, &mut tree_id_counter, 0));
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
.tree-row.leaf { }
.tree-toggle {
    display: inline-block; width: 2ch; color: #666;
    transition: transform 0.15s; text-align: center;
}
.tree-toggle.open { transform: rotate(90deg); }
.tree-children { display: none; }
.tree-children.open { display: block; }
.fn-name { color: #ce93d8; }
.loop-kw { color: #ffb74d; font-weight: bold; }
.branch-kw { color: #ba68c8; font-weight: bold; }
.source-text { color: #e0e0e0; font-family: "SFMono-Regular", Consolas, monospace; }
.subst-text {
    font-style: italic; color: #666; padding: 1px 0;
    font-family: "SFMono-Regular", Consolas, monospace;
}
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
    use serde_json::json;

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
                assert_eq!(args, json!({"n": 3}));
                assert_eq!(depth, 0);
            }
            _ => panic!("expected Call"),
        }
    }

    #[test]
    fn parse_return_event() {
        let line = r#"{"type":"return","fn":"factorial","depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Return { function, depth } => {
                assert_eq!(function, "factorial");
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
    fn parse_return_structural() {
        // Return events are now structural — just fn name and depth
        let line = r#"{"type":"return","fn":"main","depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        assert!(matches!(event, TraceEvent::Return { function, depth } if function == "main" && depth == 0));
    }

    #[test]
    fn parse_stmt_start_event() {
        let line = r#"{"type":"stmt_start","src":"auto x = f(1)","depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::StmtStart { src, depth, .. } => {
                assert_eq!(src, "auto x = f(1)");
                assert_eq!(depth, 0);
            }
            _ => panic!("expected StmtStart"),
        }
    }

    #[test]
    fn parse_stmt_end_event() {
        let line = r#"{"type":"stmt_end","depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        assert!(matches!(event, TraceEvent::StmtEnd { depth: 0 }));
    }

    #[test]
    fn parse_branch_event() {
        let line = r#"{"type":"branch","kind":"if","depth":1}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Branch { kind, depth, src, .. } => {
                assert_eq!(kind, "if");
                assert_eq!(depth, 1);
                assert_eq!(src, "");
            }
            _ => panic!("expected Branch"),
        }
    }

    #[test]
    fn parse_branch_with_src() {
        let line = r#"{"type":"branch","kind":"if","src":"if n <= 1","depth":1}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Branch { kind, depth, src, .. } => {
                assert_eq!(kind, "if");
                assert_eq!(src, "if n <= 1");
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
            TraceEvent::Branch { kind, depth, .. } => {
                assert_eq!(kind, "else");
                assert_eq!(depth, 2);
            }
            _ => panic!("expected Branch"),
        }
    }

    #[test]
    fn parse_assert_start_as_stmt_start() {
        // Legacy assert_start events are parsed as StmtStart
        let line = r#"{"type":"assert_start","src":"assert factorial(5) == 120","depth":0}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::StmtStart { src, depth, .. } => {
                assert_eq!(src, "assert factorial(5) == 120");
                assert_eq!(depth, 0);
            }
            _ => panic!("expected StmtStart"),
        }
    }

    #[test]
    fn build_tree_call_return_pair() {
        let events = vec![
            TraceEvent::Call { function: "add".into(), args: json!({"a": 1}), depth: 0 },
            TraceEvent::StmtStart { src: "auto r = 3".into(), vars: Value::Null, depth: 1 },
            TraceEvent::StmtEnd { depth: 1 },
            TraceEvent::Return { function: "add".into(), depth: 0 },
        ];
        let tree = build_tree(events);
        // Should produce one Call node with one StmtStart child
        assert_eq!(tree.len(), 1);
        assert!(matches!(&tree[0].event, TraceEvent::Call { function, .. } if function == "add"));
        assert_eq!(tree[0].children.len(), 1);
        assert!(matches!(&tree[0].children[0].event, TraceEvent::StmtStart { .. }));
    }

    #[test]
    fn build_tree_nested_calls() {
        let events = vec![
            TraceEvent::Call { function: "outer".into(), args: Value::Null, depth: 0 },
            TraceEvent::Call { function: "inner".into(), args: Value::Null, depth: 1 },
            TraceEvent::Return { function: "inner".into(), depth: 1 },
            TraceEvent::Return { function: "outer".into(), depth: 0 },
        ];
        let tree = build_tree(events);
        assert_eq!(tree.len(), 1); // outer
        assert_eq!(tree[0].children.len(), 1); // inner call node
        assert!(matches!(&tree[0].children[0].event, TraceEvent::Call { function, .. } if function == "inner"));
    }

    #[test]
    fn build_tree_loop_groups_children() {
        let events = vec![
            TraceEvent::Call { function: "sum".into(), args: Value::Null, depth: 0 },
            TraceEvent::StmtStart { src: "auto t = 0".into(), vars: Value::Null, depth: 1 },
            TraceEvent::StmtEnd { depth: 1 },
            TraceEvent::Loop { kind: "for".into(), detail: "i=1".into(), depth: 1 },
            TraceEvent::StmtStart { src: "t = t + i".into(), vars: Value::Null, depth: 1 },
            TraceEvent::StmtEnd { depth: 1 },
            TraceEvent::Loop { kind: "for".into(), detail: "i=2".into(), depth: 1 },
            TraceEvent::StmtStart { src: "t = t + i".into(), vars: Value::Null, depth: 1 },
            TraceEvent::StmtEnd { depth: 1 },
            TraceEvent::Return { function: "sum".into(), depth: 0 },
        ];
        let tree = build_tree(events);
        assert_eq!(tree.len(), 1);
        let call_children = &tree[0].children;
        assert_eq!(call_children.len(), 3); // let + loop1 + loop2
        // Loop 1 should have one child (the assign)
        assert!(matches!(&call_children[1].event, TraceEvent::Loop { detail, .. } if detail == "i=1"));
        assert_eq!(call_children[1].children.len(), 1);
        // Loop 2 should have one child (the assign)
        assert!(matches!(&call_children[2].event, TraceEvent::Loop { detail, .. } if detail == "i=2"));
        assert_eq!(call_children[2].children.len(), 1);
    }

    #[test]
    fn build_tree_flat_stmts() {
        // stmt_start/stmt_end pairs without inner calls produce leaf nodes
        let events = vec![
            TraceEvent::StmtStart { src: "assert 1 == 1".into(), vars: Value::Null, depth: 0 },
            TraceEvent::StmtEnd { depth: 0 },
            TraceEvent::StmtStart { src: "assert 2 == 2".into(), vars: Value::Null, depth: 0 },
            TraceEvent::StmtEnd { depth: 0 },
        ];
        let tree = build_tree(events);
        assert_eq!(tree.len(), 2);
        assert!(tree[0].children.is_empty());
        assert!(tree[1].children.is_empty());
    }

    #[test]
    fn build_tree_stmt_brackets_call() {
        // stmt_start/stmt_end should bracket a call, producing an expandable tree node
        let events = vec![
            TraceEvent::StmtStart { src: "auto x = add(1, 2)".into(), vars: Value::Null, depth: 0 },
            TraceEvent::Call { function: "add".into(), args: json!({"a": 1, "b": 2}), depth: 1 },
            TraceEvent::Return { function: "add".into(), depth: 1 },
            TraceEvent::StmtEnd { depth: 0 },
        ];
        let tree = build_tree(events);
        assert_eq!(tree.len(), 1); // one StmtStart node
        assert!(matches!(&tree[0].event, TraceEvent::StmtStart { src, .. } if src == "auto x = add(1, 2)"));
        // Children: Call node (Return closes it, so it appears as a child)
        assert_eq!(tree[0].children.len(), 1);
        assert!(matches!(&tree[0].children[0].event, TraceEvent::Call { .. }));
    }

    #[test]
    fn build_report_groups_by_test() {
        let events = vec![
            TraceEvent::TestStart { name: "test_a".to_string() },
            TraceEvent::Call { function: "add".to_string(), args: json!({"a": 1, "b": 2}), depth: 0 },
            TraceEvent::Return { function: "add".to_string(), depth: 0 },
            TraceEvent::TestEnd { name: "test_a".to_string(), status: "pass".to_string(), duration_ms: 1 },
            TraceEvent::TestStart { name: "test_b".to_string() },
            TraceEvent::TestEnd { name: "test_b".to_string(), status: "fail".to_string(), duration_ms: 2 },
        ];
        let report = build_report(events);
        assert_eq!(report.tests.len(), 2);
        assert_eq!(report.total_passed, 1);
        assert_eq!(report.total_failed, 1);
        assert_eq!(report.total_duration_ms, 3);
        assert_eq!(report.tests[0].tree.len(), 1); // one Call node
        assert_eq!(report.tests[1].tree.len(), 0);
    }

    #[test]
    fn html_escape_works() {
        assert_eq!(html_escape("<b>test</b>"), "&lt;b&gt;test&lt;/b&gt;");
        assert_eq!(html_escape("a & b"), "a &amp; b");
    }

    #[test]
    fn parse_stmt_start_with_vars() {
        let line = r#"{"type":"stmt_start","src":"return fibonacci(n - 1) + fibonacci(n - 2)","vars":{"n":5},"depth":1}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::StmtStart { src, vars, depth } => {
                assert_eq!(src, "return fibonacci(n - 1) + fibonacci(n - 2)");
                assert_eq!(vars, json!({"n": 5}));
                assert_eq!(depth, 1);
            }
            _ => panic!("expected StmtStart"),
        }
    }

    #[test]
    fn parse_branch_with_vars() {
        let line = r#"{"type":"branch","kind":"if","src":"if n <= 1","vars":{"n":5},"depth":2}"#;
        let event = parse_trace_line(line).unwrap();
        match event {
            TraceEvent::Branch { kind, src, vars, depth } => {
                assert_eq!(kind, "if");
                assert_eq!(src, "if n <= 1");
                assert_eq!(vars, json!({"n": 5}));
                assert_eq!(depth, 2);
            }
            _ => panic!("expected Branch"),
        }
    }

    #[test]
    fn substitute_simple() {
        let result = substitute_vars("return fibonacci(n - 1) + fibonacci(n - 2)", &json!({"n": 5}));
        assert_eq!(result, Some("return fibonacci(5 - 1) + fibonacci(5 - 2)".to_string()));
    }

    #[test]
    fn substitute_word_boundary() {
        // Should NOT replace "n" inside "name" or "fn"
        let result = substitute_vars("name + fn(n)", &json!({"n": 3}));
        assert_eq!(result, Some("name + fn(3)".to_string()));
    }

    #[test]
    fn substitute_no_change() {
        // Variable not present in source
        let result = substitute_vars("return 42", &json!({"x": 10}));
        assert_eq!(result, None);
    }

    #[test]
    fn substitute_multiple_vars() {
        let result = substitute_vars("a + b", &json!({"a": 1, "b": 2}));
        assert_eq!(result, Some("1 + 2".to_string()));
    }

    #[test]
    fn substitute_bool_var() {
        let result = substitute_vars("if flag", &json!({"flag": true}));
        assert_eq!(result, Some("if true".to_string()));
    }

    #[test]
    fn build_tree_stmt_with_subst() {
        // A StmtStart with vars but no children should still be expandable in the rendered HTML
        let events = vec![
            TraceEvent::StmtStart { src: "return n + 1".into(), vars: json!({"n": 5}), depth: 0 },
            TraceEvent::StmtEnd { depth: 0 },
        ];
        let tree = build_tree(events);
        assert_eq!(tree.len(), 1);
        // Render it and verify it's expandable (has subst-text)
        let mut counter = 0;
        let html = render_tree_html(&tree, &mut counter, 0);
        assert!(html.contains("subst-text"), "should contain substituted text div");
        assert!(html.contains("return 5 + 1"), "should contain substituted value");
    }
}
