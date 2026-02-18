use std::io::{self, Write};

use crossterm::{
    cursor, execute,
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    terminal::{self, ClearType},
};

// ── Types ────────────────────────────────────────────────────

pub enum ReadLineResult {
    Line(String),
    Eof,
}

struct SlashCommand {
    name: &'static str,
    description: &'static str,
}

const COMMANDS: &[SlashCommand] = &[
    SlashCommand { name: "/run",   description: "compile and run" },
    SlashCommand { name: "/check", description: "compile and show errors" },
    SlashCommand { name: "/show",  description: "show accumulated code" },
    SlashCommand { name: "/reset", description: "clear accumulated code" },
    SlashCommand { name: "/help",  description: "show help" },
    SlashCommand { name: "/quit",  description: "exit" },
];

struct Menu {
    matches: Vec<usize>,
    selected: usize,
}

struct LineEditor {
    buf: Vec<char>,
    cursor: usize,
    prompt: &'static str,
    menu: Option<Menu>,
    show_menu: bool,
    term_width: u16,
    prev_render_lines: usize,
}

/// RAII guard that disables raw mode on drop.
struct RawModeGuard;

impl Drop for RawModeGuard {
    fn drop(&mut self) {
        let _ = terminal::disable_raw_mode();
    }
}

// ── Menu logic ───────────────────────────────────────────────

impl LineEditor {
    fn new(prompt: &'static str, show_menu: bool) -> Self {
        let (w, _) = terminal::size().unwrap_or((80, 24));
        Self {
            buf: Vec::new(),
            cursor: 0,
            prompt,
            menu: None,
            show_menu,
            term_width: w,
            prev_render_lines: 1,
        }
    }

    fn buf_str(&self) -> String {
        self.buf.iter().collect()
    }

    fn update_menu(&mut self) {
        if !self.show_menu {
            self.menu = None;
            return;
        }
        let text = self.buf_str();
        // Menu only when buffer starts with `/` and has no spaces
        if !text.starts_with('/') || text.contains(' ') {
            self.menu = None;
            return;
        }
        let matches: Vec<usize> = COMMANDS
            .iter()
            .enumerate()
            .filter(|(_, cmd)| cmd.name.starts_with(&text))
            .map(|(i, _)| i)
            .collect();
        if matches.is_empty() {
            self.menu = None;
            return;
        }
        let selected = if let Some(ref old) = self.menu {
            old.selected.min(matches.len() - 1)
        } else {
            0
        };
        self.menu = Some(Menu { matches, selected });
    }

    // ── Rendering ────────────────────────────────────────────

    fn menu_lines(&self) -> Vec<String> {
        let menu = match &self.menu {
            Some(m) => m,
            None => return Vec::new(),
        };

        // Compute column widths
        let max_name = menu.matches.iter()
            .map(|&i| COMMANDS[i].name.len())
            .max()
            .unwrap_or(0);
        let max_desc = menu.matches.iter()
            .map(|&i| COMMANDS[i].description.len())
            .max()
            .unwrap_or(0);

        // Inner width: "▸ " (2) + name + padding + " — " (3) + desc
        let inner = 2 + max_name + 3 + max_desc;
        let box_width = (inner + 4).min(self.term_width as usize); // 2 border + 2 padding

        let mut lines = Vec::new();

        // Top border
        lines.push(format!("┌{}┐", "─".repeat(box_width - 2)));

        for (idx, &cmd_idx) in menu.matches.iter().enumerate() {
            let cmd = &COMMANDS[cmd_idx];
            let marker = if idx == menu.selected { "▸" } else { " " };
            let content = format!("{marker} {:<width$} — {}", cmd.name, cmd.description, width = max_name);
            // Pad or truncate to fit
            let avail = box_width - 4; // 2 border chars + 2 padding spaces
            let display: String = if content.chars().count() > avail {
                content.chars().take(avail).collect()
            } else {
                format!("{:<width$}", content, width = avail)
            };
            lines.push(format!("│ {display} │"));
        }

        // Bottom border
        lines.push(format!("└{}┘", "─".repeat(box_width - 2)));

        lines
    }

    /// Ensure the terminal has enough rows above the cursor for the menu.
    fn ensure_menu_space(&self, menu_height: usize) {
        let mut stdout = io::stdout();
        // Print blank lines to force scrolling, then move back up
        for _ in 0..menu_height {
            let _ = execute!(stdout, cursor::MoveToColumn(0));
            let _ = stdout.write_all(b"\n");
        }
        let _ = execute!(stdout, cursor::MoveUp(menu_height as u16));
    }

    fn render(&mut self) {
        let mut stdout = io::stdout();
        let menu_lines = self.menu_lines();
        let total_lines = menu_lines.len() + 1; // menu + prompt line

        // If the render grew (menu appeared/expanded), make space
        if total_lines > self.prev_render_lines {
            let extra = total_lines - self.prev_render_lines;
            self.ensure_menu_space(extra);
        }

        // Move to the top of the previous render region
        if self.prev_render_lines > 1 {
            let _ = execute!(stdout, cursor::MoveUp((self.prev_render_lines - 1) as u16));
        }

        // Draw each line (menu lines first, then prompt)
        for (i, line) in menu_lines.iter().enumerate().chain(std::iter::once((menu_lines.len(), &String::new()))) {
            let _ = execute!(stdout, cursor::MoveToColumn(0), terminal::Clear(ClearType::CurrentLine));
            if i < menu_lines.len() {
                let _ = stdout.write_all(line.as_bytes());
            } else {
                // Prompt line
                let prompt_text = format!("{}{}", self.prompt, self.buf_str());
                let _ = stdout.write_all(prompt_text.as_bytes());
            }
            if i < total_lines - 1 {
                let _ = stdout.write_all(b"\n");
            }
        }

        // Clear any leftover lines from a previously taller render
        if self.prev_render_lines > total_lines {
            for _ in 0..(self.prev_render_lines - total_lines) {
                let _ = stdout.write_all(b"\n");
                let _ = execute!(stdout, cursor::MoveToColumn(0), terminal::Clear(ClearType::CurrentLine));
            }
            // Move back up to the prompt line
            let extra = self.prev_render_lines - total_lines;
            if extra > 0 {
                let _ = execute!(stdout, cursor::MoveUp(extra as u16));
            }
        }

        // Position cursor on the prompt line at the correct column
        let cursor_col = self.prompt.len() + self.cursor;
        let _ = execute!(stdout, cursor::MoveToColumn(cursor_col as u16));
        let _ = stdout.flush();

        self.prev_render_lines = total_lines;
    }

    // ── Key handling ─────────────────────────────────────────

    /// Returns Some(ReadLineResult) if the line is complete, None to keep reading.
    fn handle_key(&mut self, key: KeyEvent) -> Option<ReadLineResult> {
        match key.code {
            KeyCode::Char('d') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                if self.buf.is_empty() {
                    return Some(ReadLineResult::Eof);
                }
            }
            KeyCode::Char('c') if key.modifiers.contains(KeyModifiers::CONTROL) => {
                self.buf.clear();
                self.cursor = 0;
                self.menu = None;
                self.update_menu();
            }
            KeyCode::Enter => {
                if let Some(ref menu) = self.menu {
                    let cmd_idx = menu.matches[menu.selected];
                    let cmd_name = COMMANDS[cmd_idx].name;
                    self.buf = cmd_name.chars().collect();
                    self.cursor = self.buf.len();
                    self.menu = None;
                }
                // Move cursor past the rendered content and newline
                let line = self.buf_str();
                return Some(ReadLineResult::Line(line));
            }
            KeyCode::Tab => {
                if let Some(ref menu) = self.menu {
                    let cmd_idx = menu.matches[menu.selected];
                    let cmd_name = COMMANDS[cmd_idx].name;
                    self.buf = cmd_name.chars().collect();
                    self.cursor = self.buf.len();
                    self.menu = None;
                    self.update_menu();
                }
            }
            KeyCode::Esc => {
                self.menu = None;
            }
            KeyCode::Backspace => {
                if self.cursor > 0 {
                    self.cursor -= 1;
                    self.buf.remove(self.cursor);
                    self.update_menu();
                }
            }
            KeyCode::Delete => {
                if self.cursor < self.buf.len() {
                    self.buf.remove(self.cursor);
                    self.update_menu();
                }
            }
            KeyCode::Left => {
                if self.cursor > 0 {
                    self.cursor -= 1;
                }
            }
            KeyCode::Right => {
                if self.cursor < self.buf.len() {
                    self.cursor += 1;
                }
            }
            KeyCode::Home => {
                self.cursor = 0;
            }
            KeyCode::End => {
                self.cursor = self.buf.len();
            }
            KeyCode::Up => {
                if let Some(ref mut menu) = self.menu {
                    if menu.selected == 0 {
                        menu.selected = menu.matches.len() - 1;
                    } else {
                        menu.selected -= 1;
                    }
                }
            }
            KeyCode::Down => {
                if let Some(ref mut menu) = self.menu {
                    menu.selected = (menu.selected + 1) % menu.matches.len();
                }
            }
            KeyCode::Char(c) => {
                self.buf.insert(self.cursor, c);
                self.cursor += 1;
                self.update_menu();
            }
            _ => {}
        }
        None
    }
}

// ── Public API ───────────────────────────────────────────────

pub fn read_line(prompt: &'static str, show_menu: bool) -> ReadLineResult {
    let mut editor = LineEditor::new(prompt, show_menu);

    // Enter raw mode with RAII guard
    if terminal::enable_raw_mode().is_err() {
        // Fallback: cooked-mode read
        let _ = terminal::disable_raw_mode();
        let mut stdout = io::stdout();
        let _ = stdout.write_all(prompt.as_bytes());
        let _ = stdout.flush();
        let mut line = String::new();
        return match io::stdin().read_line(&mut line) {
            Ok(0) => ReadLineResult::Eof,
            Ok(_) => {
                let trimmed = line.trim_end_matches('\n').trim_end_matches('\r').to_string();
                ReadLineResult::Line(trimmed)
            }
            Err(_) => ReadLineResult::Eof,
        };
    }
    let _guard = RawModeGuard;

    // Initial render (just the prompt)
    editor.render();

    loop {
        if let Ok(Event::Key(key)) = event::read() {
            // Skip key release events (crossterm on some platforms sends both press + release)
            if key.kind != event::KeyEventKind::Press {
                continue;
            }
            if let Some(result) = editor.handle_key(key) {
                // Before returning, clear the menu area and reprint the final prompt line cleanly
                let menu_lines = editor.menu_lines();
                let total = menu_lines.len() + 1;
                {
                    let mut stdout = io::stdout();
                    // Move to top of render region
                    if editor.prev_render_lines > 1 {
                        let _ = execute!(stdout, cursor::MoveUp((editor.prev_render_lines - 1) as u16));
                    }
                    // Clear all lines
                    for i in 0..editor.prev_render_lines {
                        let _ = execute!(stdout, cursor::MoveToColumn(0), terminal::Clear(ClearType::CurrentLine));
                        if i < editor.prev_render_lines - 1 {
                            let _ = execute!(stdout, cursor::MoveDown(1));
                        }
                    }
                    // Move back to top
                    if editor.prev_render_lines > 1 {
                        let _ = execute!(stdout, cursor::MoveUp((editor.prev_render_lines - 1) as u16));
                    }
                    // Print final prompt + input on a single line
                    let _ = execute!(stdout, cursor::MoveToColumn(0));
                    let final_text = format!("{}{}\n", editor.prompt, editor.buf_str());
                    let _ = stdout.write_all(final_text.as_bytes());
                    let _ = stdout.flush();
                }
                // Raw mode disabled by _guard drop
                let _ = total; // suppress unused warning
                return result;
            }
            editor.render();
        }
    }
}
