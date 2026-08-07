pub mod doc;

use crate::lexer::token::{StringKind, StringLiteral, StringSegment};
use crate::parser::ast::*;
use crate::span::Spanned;

// ══════════════════════════════════════════════════════════════
// Emitter — indentation-aware output buffer
// ══════════════════════════════════════════════════════════════

struct Emitter {
    buf: String,
    indent: usize,
    col: usize,
    at_line_start: bool,
}

impl Emitter {
    fn new() -> Self {
        Self {
            buf: String::new(),
            indent: 0,
            col: 0,
            at_line_start: true,
        }
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        debug_assert!(self.indent > 0);
        self.indent -= 1;
    }

    fn write(&mut self, s: &str) {
        if self.at_line_start && !s.is_empty() {
            let indent_width = self.indent * 4;
            for _ in 0..self.indent {
                self.buf.push_str("    ");
            }
            self.at_line_start = false;
            self.col = indent_width;
        }
        self.buf.push_str(s);
        self.col += s.len();
    }

    /// Write pre-formatted text from the Doc renderer.
    /// The text may contain newlines with indentation already baked in.
    /// If we're at line start, prepends the emitter's base indentation first
    /// (just like `write()` does), since the Doc renderer doesn't know about it.
    fn write_preformatted(&mut self, s: &str) {
        if s.is_empty() {
            return;
        }
        if self.at_line_start {
            let indent_width = self.indent * 4;
            for _ in 0..self.indent {
                self.buf.push_str("    ");
            }
            self.col = indent_width;
        }
        self.at_line_start = false;
        self.buf.push_str(s);
        if let Some(last_nl) = s.rfind('\n') {
            self.col = s.len() - last_nl - 1;
        } else {
            self.col += s.len();
        }
    }

    fn newline(&mut self) {
        self.buf.push('\n');
        self.col = 0;
        self.at_line_start = true;
    }

    fn blank_line(&mut self) {
        // Only emit blank line if we're not already on an empty line
        if !self.buf.ends_with("\n\n") && !self.buf.is_empty() {
            if !self.buf.ends_with('\n') {
                self.buf.push('\n');
            }
            self.buf.push('\n');
            self.at_line_start = true;
        }
    }

    fn finish(self) -> String {
        self.buf
    }
}

// ══════════════════════════════════════════════════════════════
// Formatter — walks AST and emits formatted source
// ══════════════════════════════════════════════════════════════

pub struct Formatter {
    emitter: Emitter,
    comments: Vec<Spanned<String>>,
    comment_cursor: usize,
}

impl Formatter {
    pub fn new(comments: Vec<Spanned<String>>) -> Self {
        Self {
            emitter: Emitter::new(),
            comments,
            comment_cursor: 0,
        }
    }

    pub fn format(mut self, module: &Module) -> String {
        self.format_module(module);
        self.emit_remaining_comments();
        let mut result = self.emitter.finish();
        // Normalize blank lines: collapse 3+ consecutive newlines to 2 (one blank line max).
        // Single-pass: track consecutive newlines and skip extras.
        {
            let mut normalized = String::with_capacity(result.len());
            let mut consecutive_newlines = 0u32;
            for ch in result.chars() {
                if ch == '\n' {
                    consecutive_newlines += 1;
                    if consecutive_newlines <= 2 {
                        normalized.push(ch);
                    }
                } else {
                    consecutive_newlines = 0;
                    normalized.push(ch);
                }
            }
            result = normalized;
        }
        // Ensure trailing newline
        if !result.ends_with('\n') {
            result.push('\n');
        }
        result
    }

    // ── Doc IR integration ────────────────────────────────

    /// Format an AST element to a string using a temporary formatter.
    /// Used to produce string representations of elements for Doc wrapping.
    fn element_to_string(&self, f: impl FnOnce(&mut Formatter)) -> String {
        let mut fmt = Formatter::new(vec![]);
        f(&mut fmt);
        fmt.emitter.finish()
    }

    /// Render a Doc tree at the current cursor position and write it
    /// into the output buffer. The Doc handles line-break decisions.
    fn write_doc(&mut self, doc: &doc::Doc) {
        let rendered = doc::render_at(
            doc,
            doc::MAX_WIDTH,
            self.emitter.col,
            self.emitter.indent,
        );
        self.emitter.write_preformatted(&rendered);
    }

    // ── Comment interleaving ────────────────────────────────

    fn emit_comments_before(&mut self, pos: usize) {
        while self.comment_cursor < self.comments.len() {
            let c = &self.comments[self.comment_cursor];
            if c.span.start < pos {
                self.emitter.write(&c.node);
                self.emitter.newline();
                self.comment_cursor += 1;
            } else {
                break;
            }
        }
    }

    fn emit_remaining_comments(&mut self) {
        while self.comment_cursor < self.comments.len() {
            self.emitter.write(&self.comments[self.comment_cursor].node);
            self.emitter.newline();
            self.comment_cursor += 1;
        }
    }

    // ── Module ──────────────────────────────────────────────

    fn format_module(&mut self, module: &Module) {
        // Partition items into leading directives, imports, and the rest.
        let mut directives: Vec<&Spanned<Item>> = Vec::new();
        let mut imports: Vec<&Spanned<Item>> = Vec::new();
        let mut rest: Vec<&Spanned<Item>> = Vec::new();
        let mut past_imports = false;

        for item in &module.items {
            match &item.node {
                Item::Directive(_) if !past_imports => directives.push(item),
                Item::Import(_) if !past_imports => imports.push(item),
                _ => {
                    past_imports = true;
                    rest.push(item);
                }
            }
        }

        // Sort imports: std/gg first, then third-party, alphabetically within groups.
        if !imports.is_empty() {
            imports.sort_by(|a, b| {
                let path_a = import_sort_key(a);
                let path_b = import_sort_key(b);
                let is_std_a = is_std_import(&path_a);
                let is_std_b = is_std_import(&path_b);
                // std/gg imports come first
                match (is_std_a, is_std_b) {
                    (true, false) => std::cmp::Ordering::Less,
                    (false, true) => std::cmp::Ordering::Greater,
                    _ => path_a.cmp(&path_b),
                }
            });
        }

        // Emit directives.
        let mut emitted = 0;
        for item in &directives {
            if emitted > 0 {
                self.emitter.blank_line();
            }
            self.emit_comments_before(item.span.start);
            self.format_item(item);
            emitted += 1;
        }

        // Emit sorted imports.
        for item in &imports {
            if emitted > 0 {
                self.emitter.blank_line();
            }
            self.emit_comments_before(item.span.start);
            self.format_item(item);
            emitted += 1;
        }

        // Emit remaining items.
        for item in &rest {
            if emitted > 0 {
                self.emitter.blank_line();
            }
            self.emit_comments_before(item.span.start);
            self.format_item(item);
            emitted += 1;
        }
    }

    // ── Items ───────────────────────────────────────────────

    fn format_item(&mut self, item: &Spanned<Item>) {
        match &item.node {
            Item::Function(f) => self.format_function(f),
            Item::Struct(s) => self.format_struct(s),
            Item::Enum(e) => self.format_enum(e),
            Item::Trait(t) => self.format_trait(t),
            Item::Equip(e) => self.format_equip(e),
            Item::Import(i) => self.format_import(i),
            Item::TypeAlias(ta) => self.format_type_alias(ta),
            Item::Newtype(nt) => self.format_newtype(nt),
            Item::ConstDecl(cd) => self.format_const_decl(cd),
            Item::StaticDecl(sd) => self.format_static_decl(sd),
            Item::ExternBlock(eb) => self.format_extern_block(eb),
            Item::Directive(d) => {
                self.emitter.write("directive ");
                self.emitter.write(&d.name);
                if let Some(ref val) = d.value {
                    self.emitter.write("=");
                    self.emitter.write(val);
                }
                self.emitter.newline();
            }
            Item::Test(t) => self.format_test(t),
            Item::Bench(b) => self.format_bench(b),
            Item::SuiteSetup(s) => self.format_suite_setup(s),
            Item::SuiteTeardown(s) => self.format_suite_teardown(s),
            Item::MetaConst(mc) => {
                self.emitter.write("meta ");
                self.format_type(&mc.type_);
                self.emitter.write(" ");
                self.emitter.write(&mc.name.node);
                self.emitter.write(" = ");
                self.format_expr(&mc.value);
                self.emitter.newline();
            }
            Item::MetaType(mt) => {
                self.emitter.write("meta type ");
                self.emitter.write(&mt.name.node);
                self.emitter.write(" = ");
                match &mt.rhs {
                    MetaTypeRhs::Plain(t) => self.format_type(t),
                    MetaTypeRhs::Conditional { then_type, condition, else_type } => {
                        self.format_type(then_type);
                        self.emitter.write(" if ");
                        self.format_expr(condition);
                        self.emitter.write(" else ");
                        self.format_type(else_type);
                    }
                    MetaTypeRhs::Call { callee, args } => {
                        self.emitter.write(&callee.node);
                        self.emitter.write("(");
                        for (i, arg) in args.iter().enumerate() {
                            if i > 0 { self.emitter.write(", "); }
                            self.format_expr(arg);
                        }
                        self.emitter.write(")");
                    }
                }
                self.emitter.newline();
            }
            Item::MetaTypeFunc(mtf) => {
                self.emitter.write("meta type ");
                self.emitter.write(&mtf.name.node);
                self.emitter.write("(");
                for (i, p) in mtf.params.iter().enumerate() {
                    if i > 0 { self.emitter.write(", "); }
                    self.format_param(&p.node);
                }
                self.emitter.write("):");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(&mtf.body);
                self.emitter.dedent();
            }
            Item::MetaAssert(ma) => {
                self.emitter.write("meta assert ");
                self.format_expr(&ma.condition);
                if let Some(ref msg) = ma.message {
                    self.emitter.write(", ");
                    self.format_expr(msg);
                }
                self.emitter.newline();
            }
            Item::MetaIf(mi) => {
                self.emitter.write("meta if ");
                self.format_expr(&mi.condition);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                for item in &mi.then_items {
                    self.emit_comments_before(item.span.start);
                    self.format_item(item);
                }
                self.emitter.dedent();
                for (cond, items) in &mi.elif_branches {
                    self.emitter.write("elif ");
                    self.format_expr(cond);
                    self.emitter.write(":");
                    self.emitter.newline();
                    self.emitter.indent();
                    for item in items {
                        self.emit_comments_before(item.span.start);
                        self.format_item(item);
                    }
                    self.emitter.dedent();
                }
                if let Some(ref else_items) = mi.else_items {
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    for item in else_items {
                        self.emit_comments_before(item.span.start);
                        self.format_item(item);
                    }
                    self.emitter.dedent();
                }
            }
            Item::MetaLog(ml) => {
                self.emitter.write("meta log ");
                for (i, arg) in ml.args.iter().enumerate() {
                    if i > 0 { self.emitter.write(", "); }
                    self.format_expr(arg);
                }
                self.emitter.newline();
            }
            Item::Module { items, .. } => {
                for inner in items {
                    self.format_item(inner);
                }
            }
        }
    }

    fn format_test(&mut self, t: &TestDef) {
        self.format_doc_comment(&t.doc_comment);
        self.format_attributes(&t.attributes);
        self.emitter.write("test \"");
        self.emitter.write(&t.name.node);
        self.emitter.write("\":");
        self.emitter.newline();
        self.emitter.indent();
        self.format_block_stmts(&t.body);
        self.emitter.dedent();
    }

    fn format_bench(&mut self, b: &BenchDef) {
        self.format_doc_comment(&b.doc_comment);
        self.format_attributes(&b.attributes);
        self.emitter.write("bench \"");
        self.emitter.write(&b.name.node);
        self.emitter.write("\":");
        self.emitter.newline();
        self.emitter.indent();
        self.format_block_stmts(&b.body);
        self.emitter.dedent();
    }

    fn format_suite_setup(&mut self, s: &SuiteSetup) {
        self.emitter.write("suite setup:");
        self.emitter.newline();
        self.emitter.indent();
        self.format_block_stmts(&s.body);
        self.emitter.dedent();
    }

    fn format_suite_teardown(&mut self, s: &SuiteTeardown) {
        self.emitter.write("suite teardown:");
        self.emitter.newline();
        self.emitter.indent();
        self.format_block_stmts(&s.body);
        self.emitter.dedent();
    }

    fn format_doc_comment(&mut self, doc: &Option<String>) {
        if let Some(doc) = doc {
            for line in doc.lines() {
                self.emitter.write(line);
                self.emitter.newline();
            }
        }
    }

    fn format_attributes(&mut self, attrs: &[Spanned<Attribute>]) {
        for attr in attrs {
            self.emitter.write("@");
            self.emitter.write(&attr.node.name.node);
            if !attr.node.args.is_empty() {
                self.emitter.write("(");
                for (i, arg) in attr.node.args.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    match arg {
                        AttributeArg::Identifier(s) => self.emitter.write(s),
                        AttributeArg::StringLiteral(s) => {
                            self.emitter.write("\"");
                            self.emitter.write(s);
                            self.emitter.write("\"");
                        }
                        AttributeArg::KeyValue(k, v) => {
                            self.emitter.write(k);
                            self.emitter.write(" = ");
                            self.emitter.write("\"");
                            self.emitter.write(v);
                            self.emitter.write("\"");
                        }
                    }
                }
                self.emitter.write(")");
            }
            self.emitter.newline();
        }
    }

    fn format_visibility(&mut self, vis: &Visibility) {
        // Public is the default — no keyword needed.
        // Private is the opt-in keyword.
        if *vis == Visibility::Private {
            self.emitter.write("private ");
        }
    }

    fn format_function(&mut self, f: &FunctionDef) {
        self.format_doc_comment(&f.doc_comment);
        self.format_attributes(&f.attributes);
        self.format_visibility(&f.visibility);
        if matches!(f.body, FunctionBody::Extern(_)) {
            self.emitter.write("extern ");
        }
        self.format_qualifiers(&f.qualifiers);
        // type-first: `ReturnType name(params)`
        // Bare tuple return: emit `T1, T2` not `(T1, T2)` in return position
        if let Type::Tuple(types) = &f.return_type.node {
            for (i, ty) in types.iter().enumerate() {
                if i > 0 {
                    self.emitter.write(", ");
                }
                self.format_type(ty);
            }
        } else {
            self.format_type(&f.return_type);
        }
        self.emitter.write(" ");
        self.emitter.write(&f.name.node);
        if let Some(ref gp) = f.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.format_params_wrapped(&f.params);
        match &f.throws {
            ThrowsSpec::Explicit(throws) => {
                self.emitter.write(" throws ");
                self.format_type(throws);
            }
            // D29/A31 bare `!` inferred-error-set signature (`int f()!:`).
            ThrowsSpec::Inferred(_) => self.emitter.write("!"),
            ThrowsSpec::No => {}
        }
        match &f.body {
            FunctionBody::Block(block) => {
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(block);
                self.emitter.dedent();
            }
            FunctionBody::Expression(expr) => {
                self.emitter.write(": ");
                self.format_expr(expr);
                self.emitter.newline();
            }
            FunctionBody::Declaration => {
                self.emitter.newline();
            }
            FunctionBody::Extern(sym) => {
                self.emitter.write(" = \"");
                self.emitter.write(sym);
                self.emitter.write("\"");
                self.emitter.newline();
            }
        }
    }

    fn format_qualifiers(&mut self, q: &FunctionQualifiers) {
        if q.is_async {
            self.emitter.write("async ");
        }
        if q.is_const {
            self.emitter.write("const ");
        }
        if q.is_static {
            self.emitter.write("static ");
        }
        if q.is_unsafe {
            self.emitter.write("unsafe ");
        }
    }

    fn format_struct(&mut self, s: &StructDef) {
        self.format_doc_comment(&s.doc_comment);
        self.format_attributes(&s.attributes);
        self.format_visibility(&s.visibility);
        self.emitter.write("struct ");
        self.emitter.write(&s.name.node);
        if let Some(ref gp) = s.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        for field in &s.fields {
            self.emit_comments_before(field.span.start);
            if field.node.visibility == Visibility::Private {
                self.emitter.write("private ");
            }
            // type-first: `type name`
            self.format_type(&field.node.type_);
            self.emitter.write(" ");
            self.emitter.write(&field.node.name.node);
            self.emitter.newline();
        }
        self.emitter.dedent();
    }

    fn format_enum(&mut self, e: &EnumDef) {
        self.format_doc_comment(&e.doc_comment);
        self.format_attributes(&e.attributes);
        self.format_visibility(&e.visibility);
        self.emitter.write("enum ");
        self.emitter.write(&e.name.node);
        if let Some(ref gp) = e.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        for variant in &e.variants {
            self.emit_comments_before(variant.span.start);
            self.emitter.write(&variant.node.name.node);
            match &variant.node.fields {
                VariantFields::Unit => {}
                VariantFields::Tuple(types) => {
                    self.emitter.write("(");
                    for (i, ty) in types.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_type(ty);
                    }
                    self.emitter.write(")");
                }
            }
            self.emitter.newline();
        }
        self.emitter.dedent();
    }

    fn format_trait(&mut self, t: &TraitDef) {
        self.format_doc_comment(&t.doc_comment);
        self.format_attributes(&t.attributes);
        self.format_visibility(&t.visibility);
        self.emitter.write("trait ");
        self.emitter.write(&t.name.node);
        if let Some(ref gp) = t.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        if !t.extends.is_empty() {
            self.emitter.write(" extends ");
            for (i, bound) in t.extends.iter().enumerate() {
                if i > 0 {
                    // Parser consumes `&` between supertrait names
                    // (parse_trait_bound_list); emit the same so fmt
                    // round-trips.
                    self.emitter.write(" & ");
                }
                self.format_trait_bound(bound);
            }
        }
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        for (i, item) in t.items.iter().enumerate() {
            if i > 0 {
                self.emitter.blank_line();
            }
            self.emit_comments_before(item.span.start);
            match &item.node {
                TraitItem::Method(f) => self.format_function(f),
                TraitItem::AssociatedType(at) => {
                    self.emitter.write("type ");
                    self.emitter.write(&at.name.node);
                    if !at.bounds.is_empty() {
                        self.emitter.write(": ");
                        for (i, bound) in at.bounds.iter().enumerate() {
                            if i > 0 {
                                self.emitter.write(" & ");
                            }
                            self.format_trait_bound(bound);
                        }
                    }
                    if let Some(ref default) = at.default {
                        self.emitter.write(" = ");
                        self.format_type(default);
                    }
                    self.emitter.newline();
                }
            }
        }
        self.emitter.dedent();
    }

    fn format_equip(&mut self, e: &EquipBlock) {
        self.emitter.write("equip ");
        if let Some(ref gp) = e.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.format_type(&e.type_);
        if let Some(ref trait_) = e.trait_ {
            self.emitter.write(" with ");
            self.format_type(&trait_.trait_name);
        }
        if let Some(ref via) = e.via_field {
            self.emitter.write(" via ");
            self.emitter.write(&via.node);
        }
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        if e.items.is_empty() {
            self.emitter.write("pass");
            self.emitter.newline();
        } else {
            for (i, method) in e.items.iter().enumerate() {
                if i > 0 {
                    self.emitter.blank_line();
                }
                self.emit_comments_before(method.span.start);
                self.format_function(&method.node);
            }
        }
        self.emitter.dedent();
    }

    fn format_import(&mut self, i: &ImportStmt) {
        match i {
            ImportStmt::Simple { path, .. } => {
                self.emitter.write("import ");
                self.format_dotted_path(path);
                self.emitter.newline();
            }
            ImportStmt::Grouped { path, names, .. } => {
                self.emitter.write("import ");
                self.format_dotted_path(path);
                self.emitter.write(".");
                let mut sorted: Vec<&str> = names.iter().map(|n| n.node.as_str()).collect();
                sorted.sort_unstable();
                let items: Vec<doc::Doc> = sorted.iter().map(|n| doc::text(*n)).collect();
                let doc = doc::surround("{", items, "}", true);
                self.write_doc(&doc);
                self.emitter.newline();
            }
            ImportStmt::From { path, names, glob_types, wildcard, .. } => {
                self.emitter.write("from ");
                self.format_dotted_path(path);
                self.emitter.write(" import ");
                if *wildcard {
                    self.emitter.write("*");
                    self.emitter.newline();
                    return;
                }
                // Merge regular names (with optional `as` alias) and glob types
                // (with .* suffix), then sort.
                let mut sorted: Vec<String> = names
                    .iter()
                    .map(|n| match &n.alias {
                        Some(a) => format!("{} as {}", n.name.node, a.node),
                        None => n.name.node.clone(),
                    })
                    .collect();
                for gt in glob_types {
                    sorted.push(format!("{}.*", gt.node));
                }
                sorted.sort_unstable();
                // No wrapping for `from` imports — bare names on new lines
                // would be parsed as new statements in indentation-based syntax.
                for (j, name) in sorted.iter().enumerate() {
                    if j > 0 {
                        self.emitter.write(", ");
                    }
                    self.emitter.write(name);
                }
                self.emitter.newline();
            }
        }
    }

    fn format_dotted_path(&mut self, path: &[Spanned<String>]) {
        for (i, seg) in path.iter().enumerate() {
            if i > 0 {
                self.emitter.write(".");
            }
            self.emitter.write(&seg.node);
        }
    }

    fn format_type_alias(&mut self, ta: &TypeAlias) {
        self.format_visibility(&ta.visibility);
        self.emitter.write("type ");
        self.emitter.write(&ta.name.node);
        if let Some(ref gp) = ta.generic_params {
            self.format_generic_params_wrapped(gp);
        }
        self.emitter.write(" = ");
        self.format_type(&ta.type_);
        self.emitter.newline();
    }

    fn format_newtype(&mut self, nt: &NewtypeDef) {
        self.format_visibility(&nt.visibility);
        self.emitter.write("newtype ");
        self.emitter.write(&nt.name.node);
        self.emitter.write("(");
        self.format_type(&nt.inner_type);
        self.emitter.write(")");
        self.emitter.newline();
    }

    fn format_const_decl(&mut self, cd: &ConstDecl) {
        self.format_visibility(&cd.visibility);
        self.emitter.write("const ");
        self.format_type(&cd.type_);
        self.emitter.write(" ");
        self.emitter.write(&cd.name.node);
        self.emitter.write(" = ");
        self.format_expr(&cd.value);
        self.emitter.newline();
    }

    fn format_static_decl(&mut self, sd: &StaticDecl) {
        // Static globals are private-by-default (opposite of functions / structs
        // which are public-by-default). Emit `public` explicitly — `format_visibility`
        // drops it for the regular-item convention, which would silently flip
        // visibility on round-trip through `gg fmt`.
        if sd.visibility == Visibility::Public {
            self.emitter.write("public ");
        }
        self.emitter.write("static ");
        self.format_type(&sd.type_);
        self.emitter.write(" ");
        self.emitter.write(&sd.name.node);
        self.emitter.write(" = ");
        self.format_expr(&sd.value);
        self.emitter.newline();
    }

    fn format_extern_block(&mut self, eb: &ExternBlock) {
        self.emitter.write("extern");
        if let Some(ref abi) = eb.abi {
            self.emitter.write(" \"");
            self.emitter.write(&abi.node);
            self.emitter.write("\"");
        }
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        for func in &eb.items {
            self.format_function(&func.node);
        }
        self.emitter.dedent();
    }

    // ── Generics & Bounds ───────────────────────────────────

    fn format_generic_param(&mut self, param: &GenericParam) {
        match param {
            GenericParam::Type { name, bounds } => {
                for (i, tb) in bounds.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(" & ");
                    }
                    self.format_trait_bound(tb);
                }
                if !bounds.is_empty() {
                    self.emitter.write(" ");
                }
                self.emitter.write(&name.node);
            }
            GenericParam::Const { type_, name } => {
                self.emitter.write("const ");
                self.format_type(type_);
                self.emitter.write(" ");
                self.emitter.write(&name.node);
            }
        }
    }

    fn format_trait_bound(&mut self, tb: &Spanned<TraitBound>) {
        self.emitter.write(&tb.node.name.node);
        let has_args = tb.node.generic_args.as_ref().is_some_and(|a| !a.is_empty());
        let has_bindings = !tb.node.assoc_type_bindings.is_empty();
        if has_args || has_bindings {
            self.emitter.write("[");
            let mut first = true;
            if let Some(ref args) = tb.node.generic_args {
                for arg in args {
                    if !first {
                        self.emitter.write(", ");
                    }
                    self.format_type(arg);
                    first = false;
                }
            }
            for binding in &tb.node.assoc_type_bindings {
                if !first {
                    self.emitter.write(", ");
                }
                self.emitter.write(&binding.name.node);
                self.emitter.write(" = ");
                self.format_type(&binding.type_);
                first = false;
            }
            self.emitter.write("]");
        }
    }

    // ── Parameters ──────────────────────────────────────────

    /// Format a parenthesized parameter list with line-width-aware wrapping.
    /// Writes `(param1, param2)` on one line if it fits, otherwise wraps:
    /// ```text
    /// (
    ///     param1,
    ///     param2,
    /// )
    /// ```
    fn format_params_wrapped(&mut self, params: &[Spanned<Param>]) {
        let items: Vec<doc::Doc> = params.iter().map(|p| {
            doc::text(self.element_to_string(|f| f.format_param(&p.node)))
        }).collect();
        let doc = doc::surround("(", items, ")", true);
        self.write_doc(&doc);
    }

    /// Format a parenthesized call argument list with line-width-aware wrapping.
    fn format_call_args_wrapped(&mut self, args: &[Spanned<CallArg>]) {
        let items: Vec<doc::Doc> = args.iter().map(|a| {
            doc::text(self.element_to_string(|f| f.format_call_arg(&a.node)))
        }).collect();
        let doc = doc::surround("(", items, ")", true);
        self.write_doc(&doc);
    }

    /// Format a bracketed generic parameter list with line-width-aware wrapping.
    fn format_generic_params_wrapped(&mut self, gp: &Spanned<GenericParams>) {
        let items: Vec<doc::Doc> = gp.node.params.iter().map(|p| {
            doc::text(self.element_to_string(|f| f.format_generic_param(&p.node)))
        }).collect();
        let doc = doc::surround("[", items, "]", true);
        self.write_doc(&doc);
    }

    /// Format a bracketed generic argument list (types) with wrapping.
    fn format_generic_args_wrapped(&mut self, args: &[Spanned<Type>]) {
        let items: Vec<doc::Doc> = args.iter().map(|t| {
            doc::text(self.element_to_string(|f| f.format_type(t)))
        }).collect();
        let doc = doc::surround("[", items, "]", true);
        self.write_doc(&doc);
    }

    /// Format a method chain with line-width-aware wrapping.
    /// When the chain fits on one line: `items.filter(pred).map(f).collect()`
    /// When broken:
    /// ```text
    /// items
    ///     .filter(pred)
    ///     .map(f)
    ///     .collect()
    /// ```
    fn format_method_chain(&mut self, expr: &Spanned<Expr>) {
        let (root, segments) = collect_method_chain(expr);
        let root_str = self.element_to_string(|f| f.format_expr(root));

        let mut parts = Vec::with_capacity(segments.len() + 1);
        // Format each .method(args) segment as a string
        for (method, generic_args, args) in &segments {
            let seg_str = self.element_to_string(|f| {
                f.emitter.write(".");
                f.emitter.write(&method.node);
                if let Some(ga) = generic_args {
                    f.format_generic_args_wrapped(ga);
                }
                f.format_call_args_wrapped(args);
            });
            parts.push(seg_str);
        }

        // Build Doc: root + indent(softline + .method1() + softline + .method2() + ...)
        let mut inner_docs = Vec::with_capacity(parts.len() * 2);
        for part in &parts {
            inner_docs.push(doc::softline());
            inner_docs.push(doc::text(part));
        }

        let chain_doc = doc::group(doc::concat(vec![
            doc::text(root_str),
            doc::indent(doc::concat(inner_docs)),
        ]));
        self.write_doc(&chain_doc);
    }

    /// Format a binary expression with line-width-aware wrapping.
    /// Flattens chains of the same operator for clean breaking.
    /// When the expression fits: `a + b + c`
    /// When broken:
    /// ```text
    /// a
    ///     + b
    ///     + c
    /// ```
    fn format_binary_chain(
        &mut self,
        left: &Spanned<Expr>,
        op: BinaryOp,
        right: &Spanned<Expr>,
    ) {
        // Flatten same-operator chains for clean wrapping.
        let mut operands = Vec::new();
        collect_binary_operands(left, op, &mut operands);
        operands.push(right);

        let op_str = binary_op_str(op);

        // If only 2 operands (no chain), use simpler Doc
        let operand_strs: Vec<String> = operands
            .iter()
            .map(|o| self.element_to_string(|f| f.format_expr(o)))
            .collect();

        // Build: operand1 <line " op "> operand2 <line " op "> operand3 ...
        let mut docs = Vec::with_capacity(operand_strs.len() * 2);
        for (i, s) in operand_strs.iter().enumerate() {
            if i > 0 {
                // In flat mode: ` op `. In broken mode: newline + indent + `op `.
                docs.push(doc::line());
                docs.push(doc::text(format!("{op_str} ")));
            }
            docs.push(doc::text(s));
        }

        // When this chain breaks across lines, the continuation lines start with
        // the operator (`+ a`). Bare leading-operator continuations are NOT valid
        // Gorget — the parser rejects them, and a second `gg fmt` pass then drops
        // the orphaned lines, silently LOSING code on round-trip. The lexer only
        // suppresses NEWLINE/INDENT/DEDENT inside brackets (`bracket_depth > 0`,
        // src/lexer/mod.rs:22), so the multi-line form is only parser-valid when
        // wrapped in parentheses. Emit `(` / `)` via `if_break` so the parens
        // appear ONLY in broken mode (flat mode stays `a + b + c`, no noise), and
        // the wrapped form re-parses to the same bare BinaryOp → re-formats to the
        // same parenthesized shape (idempotent). Parens are semantically
        // transparent, so adding them never changes meaning. See the
        // `fmt_binary_chain_round_trips` guard in tests/integration.rs.
        let bin_doc = doc::group(doc::concat(vec![
            doc::if_break(doc::text(""), doc::text("(")),
            docs.remove(0), // first operand
            doc::indent(doc::concat(docs)),
            doc::if_break(doc::text(""), doc::text(")")),
        ]));
        self.write_doc(&bin_doc);
    }

    fn format_param(&mut self, param: &Param) {
        // self parameter (same in both modes)
        if matches!(param.type_.node, Type::SelfType) {
            match param.ownership {
                Ownership::Borrow => self.emitter.write("self"),
                Ownership::MutableBorrow => self.emitter.write("&self"),
                Ownership::Move => self.emitter.write("!self"),
            }
            return;
        }
        // type-first: `type [&|!]name`
        self.format_type(&param.type_);
        self.emitter.write(" ");
        self.format_ownership_prefix(param.ownership);
        self.emitter.write(&param.name.node);
        if let Some(ref default) = param.default {
            self.emitter.write(" = ");
            self.format_expr(default);
        }
    }

    // ── Statements ──────────────────────────────────────────

    fn format_block_stmts(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.emit_comments_before(stmt.span.start);
            self.format_stmt(stmt);
        }
    }

    fn format_elif_else_blocks(
        &mut self,
        elif_branches: &[(Spanned<Expr>, Block)],
        else_body: Option<&Block>,
    ) {
        for (cond, body) in elif_branches {
            self.emitter.write("elif ");
            self.format_expr(cond);
            self.emitter.write(":");
            self.emitter.newline();
            self.emitter.indent();
            self.format_block_stmts(body);
            self.emitter.dedent();
        }
        if let Some(else_body) = else_body {
            self.emitter.write("else:");
            self.emitter.newline();
            self.emitter.indent();
            self.format_block_stmts(else_body);
            self.emitter.dedent();
        }
    }

    fn format_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl {
                is_const,
                is_mutable,
                shared,
                type_,
                pattern,
                value,
            } => {
                if *is_const {
                    self.emitter.write("const ");
                } else if *is_mutable {
                    self.emitter.write("mutable ");
                }
                match shared {
                    SharedKind::Auto => self.emitter.write("shared "),
                    SharedKind::RwLock => self.emitter.write("shared(rwlock) "),
                    SharedKind::Atomic => self.emitter.write("shared(atomic) "),
                    SharedKind::None => {}
                }
                // type-first: `type name = expr`
                self.format_type(type_);
                self.emitter.write(" ");
                // For auto declarations with tuple patterns, emit bare (no parens):
                // `auto a, b = ...` not `auto (a, b) = ...`
                if matches!(&type_.node, Type::Inferred) {
                    if let Pattern::Tuple(pats) = &pattern.node {
                        for (i, p) in pats.iter().enumerate() {
                            if i > 0 {
                                self.emitter.write(", ");
                            }
                            self.format_pattern(p);
                        }
                    } else {
                        self.format_pattern(pattern);
                    }
                } else {
                    self.format_pattern(pattern);
                }
                self.emitter.write(" = ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::Expr(expr) => {
                self.format_expr(expr);
                self.emitter.newline();
            }
            Stmt::Assign { target, value } => {
                self.format_expr(target);
                self.emitter.write(" = ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::CompoundAssign { target, op, value } => {
                self.format_expr(target);
                self.emitter.write(" ");
                self.emitter.write(compound_op_str(*op));
                self.emitter.write(" ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::Return(None) => {
                self.emitter.write("return");
                self.emitter.newline();
            }
            Stmt::Return(Some(expr)) => {
                self.emitter.write("return ");
                // Bare tuple: emit `a, b` not `(a, b)` in return position
                if let Expr::TupleLiteral(elems) = &expr.node {
                    for (i, e) in elems.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_expr(e);
                    }
                } else {
                    self.format_expr(expr);
                }
                self.emitter.newline();
            }
            Stmt::Throw(expr) => {
                self.emitter.write("throw ");
                self.format_expr(expr);
                self.emitter.newline();
            }
            Stmt::Break => {
                self.emitter.write("break");
                self.emitter.newline();
            }
            Stmt::Continue => {
                self.emitter.write("continue");
                self.emitter.newline();
            }
            Stmt::Pass => {
                self.emitter.write("pass");
                self.emitter.newline();
            }
            Stmt::For {
                pattern,
                ownership,
                iterable,
                body,
                else_body,
            } => {
                self.emitter.write("for ");
                // Bare tuple: emit `x, y` not `(x, y)` in for-loop pattern
                if let Pattern::Tuple(pats) = &pattern.node {
                    for (i, p) in pats.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_pattern(p);
                    }
                } else {
                    self.format_pattern(pattern);
                }
                self.emitter.write(" in ");
                self.format_ownership_prefix(*ownership);
                self.format_expr(iterable);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
                if let Some(else_body) = else_body {
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
            }
            Stmt::While {
                condition,
                body,
                else_body,
            } => {
                self.emitter.write("while ");
                self.format_expr(condition);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
                if let Some(else_body) = else_body {
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
            }
            Stmt::Loop { body } => {
                self.emitter.write("loop:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::If {
                condition,
                then_body,
                elif_branches,
                else_body,
            } => {
                self.emitter.write("if ");
                self.format_expr(condition);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(then_body);
                self.emitter.dedent();
                self.format_elif_else_blocks(elif_branches, else_body.as_ref());
            }
            Stmt::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.emitter.write("match ");
                self.format_expr(scrutinee);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                for item in arms {
                    match item {
                        crate::parser::ast::MatchItem::Arm(arm) => {
                            self.format_match_arm(arm);
                        }
                        crate::parser::ast::MatchItem::MetaFor { vars, range, arm_template, .. } => {
                            self.emitter.write("meta for ");
                            let joined = vars.iter().map(|v| v.node.as_str()).collect::<Vec<_>>().join(", ");
                            self.emitter.write(&joined);
                            self.emitter.write(" in ");
                            self.format_expr(range);
                            self.emitter.write(":");
                            self.emitter.newline();
                            self.emitter.indent();
                            self.format_match_arm(arm_template);
                            self.emitter.dedent();
                        }
                    }
                }
                if let Some(else_body) = else_arm {
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
                self.emitter.dedent();
            }
            Stmt::Select { arms, else_arm } => {
                self.emitter.write("select:");
                self.emitter.newline();
                self.emitter.indent();
                for arm in arms {
                    self.emitter.write("case ");
                    match &arm.op {
                        SelectOp::Recv { type_, name, channel } => {
                            self.format_type(type_);
                            self.emitter.write(" ");
                            self.emitter.write(&name.node);
                            self.emitter.write(" = ");
                            self.format_expr(channel);
                            self.emitter.write(".recv()");
                        }
                        SelectOp::Send { channel, value } => {
                            self.format_expr(channel);
                            self.emitter.write(".send(");
                            self.format_expr(value);
                            self.emitter.write(")");
                        }
                    }
                    self.emitter.write(":");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(&arm.body);
                    self.emitter.dedent();
                }
                if let Some(else_body) = else_arm {
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
                self.emitter.dedent();
            }
            Stmt::With { bindings, body } => {
                self.emitter.write("with ");
                for (i, binding) in bindings.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_expr(&binding.expr);
                    self.emitter.write(" as ");
                    self.emitter.write(&binding.name.node);
                }
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::Unsafe { body } => {
                self.emitter.write("unsafe:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::Assert { condition, message } => {
                self.emitter.write("assert ");
                self.format_expr(condition);
                if let Some(msg) = message {
                    self.emitter.write(", ");
                    self.format_expr(msg);
                }
                self.emitter.newline();
            }
            Stmt::AssertReturn { condition, message } => {
                self.emitter.write("assert return");
                self.format_assert_return_expr(condition);
                if let Some(msg) = message {
                    self.emitter.write(", ");
                    self.format_expr(msg);
                }
                self.emitter.newline();
            }
            Stmt::Snapshot { name, value } => {
                self.emitter.write("snapshot \"");
                self.emitter.write(&name.node);
                self.emitter.write("\" ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::Item(item) => {
                let spanned = Spanned::new(*item.clone(), stmt.span);
                self.format_item(&spanned);
            }
            Stmt::MetaIf {
                condition,
                then_body,
                elif_branches,
                else_body,
                ..
            } => {
                self.emitter.write("meta if ");
                self.format_expr(condition);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(then_body);
                self.emitter.dedent();
                self.format_elif_else_blocks(elif_branches, else_body.as_ref());
            }
            Stmt::MetaFor { vars, range, body, .. } => {
                self.emitter.write("meta for ");
                let joined = vars.iter().map(|v| v.node.as_str()).collect::<Vec<_>>().join(", ");
                self.emitter.write(&joined);
                self.emitter.write(" in ");
                self.format_expr(range);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::MetaMatch { scrutinee, arms, else_arm, .. } => {
                self.emitter.write("meta match ");
                self.format_expr(scrutinee);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                for (case_expr, body) in arms {
                    self.emitter.write("case ");
                    self.format_expr(case_expr);
                    self.emitter.write(":");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(body);
                    self.emitter.dedent();
                }
                if let Some(else_body) = else_arm {
                    self.emitter.write("else:");
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(else_body);
                    self.emitter.dedent();
                }
                self.emitter.dedent();
            }
            Stmt::MetaWhile { condition, body, .. } => {
                self.emitter.write("meta while ");
                self.format_expr(condition);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::MetaConst { name, value, .. } => {
                self.emitter.write("meta const ");
                self.emitter.write(&name.node);
                self.emitter.write(" = ");
                self.format_expr(value);
                self.emitter.newline();
            }
            Stmt::MetaLog { args, .. } => {
                self.emitter.write("meta log ");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { self.emitter.write(", "); }
                    self.format_expr(arg);
                }
                self.emitter.newline();
            }
            Stmt::NamedScope { name, body } => {
                self.emitter.write(&name.node);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Stmt::OnError { body } => {
                self.emitter.write("on error:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
        }
    }

    fn format_match_arm(&mut self, arm: &MatchArm) {
        self.emitter.write("case ");
        self.format_pattern(&arm.pattern);
        if let Some(ref guard) = arm.guard {
            self.emitter.write(" if ");
            self.format_expr(guard);
        }
        self.emitter.write(":");
        // Check if the body is a Block expression (multi-line arm)
        if let Expr::Block(ref block) = arm.body.node {
            self.emitter.newline();
            self.emitter.indent();
            self.format_block_stmts(block);
            self.emitter.dedent();
        } else {
            self.emitter.write(" ");
            self.format_expr(&arm.body);
            self.emitter.newline();
        }
    }

    // ── Types ───────────────────────────────────────────────

    fn format_type(&mut self, ty: &Spanned<Type>) {
        match &ty.node {
            Type::Primitive(p) => self.emitter.write(primitive_type_str(*p)),
            Type::Named { name, generic_args } => {
                self.emitter.write(&name.node);
                if !generic_args.is_empty() {
                    self.format_generic_args_wrapped(generic_args);
                }
            }
            Type::Array { element, size } => {
                self.format_type(element);
                self.emitter.write("[");
                self.format_expr(size);
                self.emitter.write("]");
            }
            Type::Slice { element } => {
                self.format_type(element);
                self.emitter.write("[]");
            }
            Type::Tuple(types) => {
                self.emitter.write("(");
                for (i, ty) in types.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_type(ty);
                }
                self.emitter.write(")");
            }
            Type::Function {
                return_type,
                params,
                param_ownerships,
            } => {
                // D35 (docs/define-gorget/decisions.md, ratified 2026-07-26):
                // an unnamed parameter's sigil is spelled AFTER the type
                // (`int &`, `String !`) — uniform with the named form
                // (`Message &msg`) and with `Type::Ref`/`Type::Owned` above.
                self.format_type(return_type);
                self.emitter.write("(");
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_type(p);
                    if let Some(ownership) = param_ownerships.get(i) {
                        match ownership {
                            Ownership::MutableBorrow => self.emitter.write(" &"),
                            Ownership::Move => self.emitter.write(" !"),
                            Ownership::Borrow => {}
                        }
                    }
                }
                self.emitter.write(")");
            }
            Type::Ref(inner) => {
                self.format_type(inner);
                self.emitter.write(" &");
            }
            Type::Owned(inner) => {
                self.format_type(inner);
                self.emitter.write(" !");
            }
            Type::Pointer(inner) => {
                self.format_type(inner);
                self.emitter.write("*");
            }
            Type::SelfType => self.emitter.write("Self"),
            Type::Inferred => self.emitter.write("auto"),
        }
    }

    // ── Patterns ────────────────────────────────────────────

    fn format_pattern(&mut self, pat: &Spanned<Pattern>) {
        match &pat.node {
            Pattern::Wildcard => self.emitter.write("_"),
            Pattern::Literal(expr) => self.format_expr(expr),
            Pattern::Binding(name) => self.emitter.write(name),
            Pattern::Constructor { path, fields } => {
                for (i, seg) in path.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(".");
                    }
                    self.emitter.write(&seg.node);
                }
                if !fields.is_empty() {
                    self.emitter.write("(");
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_pattern(field);
                    }
                    self.emitter.write(")");
                }
            }
            Pattern::Tuple(pats) => {
                self.emitter.write("(");
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_pattern(p);
                }
                self.emitter.write(")");
            }
            Pattern::Or(alts) => {
                for (i, alt) in alts.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(" | ");
                    }
                    self.format_pattern(alt);
                }
            }
            Pattern::Rest => self.emitter.write(".."),
            Pattern::DotShorthand { variant, fields } => {
                self.emitter.write(".");
                self.emitter.write(&variant.node);
                if !fields.is_empty() {
                    self.emitter.write("(");
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_pattern(field);
                    }
                    self.emitter.write(")");
                }
            }
        }
    }

    // ── Expressions ─────────────────────────────────────────

    /// Format an expression for `assert return`, replacing `__return__` with `return`.
    fn format_assert_return_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Identifier(name) if name == "__return__" => {
                // __return__ is a parser-internal placeholder; the surrounding
                // assert-return handler emits the keyword.
            }
            Expr::BinaryOp { left, op, right } => {
                self.format_assert_return_expr(left);
                self.emitter.write(" ");
                self.emitter.write(binary_op_str(*op));
                self.emitter.write(" ");
                self.format_assert_return_expr(right);
            }
            _ => self.format_expr(expr),
        }
    }

    fn format_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::IntLiteral(n) => {
                self.emitter.write(&n.to_string());
            }
            Expr::FloatLiteral(n) => {
                let s = format!("{}", n);
                // Ensure it looks like a float
                if !s.contains('.') && !s.contains('e') && !s.contains('E') {
                    self.emitter.write(&format!("{}.0", s));
                } else {
                    self.emitter.write(&s);
                }
            }
            Expr::BoolLiteral(b) => {
                self.emitter.write(if *b { "true" } else { "false" });
            }
            Expr::StringLiteral(s, _) => {
                self.format_string_lit(s);
            }
            Expr::NoneLiteral => self.emitter.write("None"),
            Expr::Identifier(name) => self.emitter.write(name),
            Expr::SelfExpr => self.emitter.write("self"),
            Expr::Path { segments } => {
                for (i, seg) in segments.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(".");
                    }
                    self.emitter.write(&seg.node);
                }
            }
            Expr::UnaryOp { op, operand } => {
                self.emitter.write(unary_op_str(*op));
                self.format_expr(operand);
            }
            Expr::BinaryOp { left, op, right } => {
                self.format_binary_chain(left, *op, right);
            }
            Expr::Call {
                callee,
                generic_args,
                args,
            } => {
                self.format_expr(callee);
                if let Some(ga) = generic_args {
                    self.format_generic_args_wrapped(ga);
                }
                self.format_call_args_wrapped(args);
            }
            Expr::MethodCall {
                receiver,
                method,
                generic_args,
                args,
            } => {
                // Detect method chains (2+ consecutive .method() calls).
                // Flatten and wrap with Doc for line-width-aware breaking.
                let chain_len = method_chain_length(expr);
                if chain_len >= 2 {
                    self.format_method_chain(expr);
                } else {
                    self.format_expr(receiver);
                    self.emitter.write(".");
                    self.emitter.write(&method.node);
                    if let Some(ga) = generic_args {
                        self.format_generic_args_wrapped(ga);
                    }
                    self.format_call_args_wrapped(args);
                }
            }
            Expr::FieldAccess { object, field } => {
                self.format_expr(object);
                self.emitter.write(".");
                self.emitter.write(&field.node);
            }
            Expr::TupleFieldAccess { object, index } => {
                self.format_expr(object);
                self.emitter.write(".");
                self.emitter.write(&index.to_string());
            }
            Expr::Index { object, index } => {
                self.format_expr(object);
                self.emitter.write("[");
                self.format_expr(index);
                self.emitter.write("]");
            }
            Expr::Range {
                start,
                end,
                inclusive,
            } => {
                if let Some(s) = start {
                    self.format_expr(s);
                }
                self.emitter.write(if *inclusive { "..=" } else { ".." });
                if let Some(e) = end {
                    self.format_expr(e);
                }
            }
            Expr::OptionalChain { object, field } => {
                self.format_expr(object);
                self.emitter.write("?.");
                self.emitter.write(&field.node);
            }
            Expr::DefaultOp { lhs, rhs } => {
                let lhs_s = self.element_to_string(|f| f.format_expr(lhs));
                let rhs_s = self.element_to_string(|f| f.format_expr(rhs));
                let nil_doc = doc::group(doc::concat(vec![
                    doc::text(lhs_s),
                    doc::indent(doc::concat(vec![
                        doc::line(),
                        doc::text(format!("?? {rhs_s}")),
                    ])),
                ]));
                self.write_doc(&nil_doc);
            }
            Expr::Move { expr } => {
                self.emitter.write("!");
                self.format_expr(expr);
            }
            // D29: postfix error-propagation renders the `!` AFTER the inner
            // expression. No bang-space corner here: a `!=`/`==` comparison is
            // a `BinaryOp` whose arm already emits ` != `/ ` == ` with spaces,
            // so a re-rendered `f()! != b` never fuses. (The raw-text migrator
            // handles bang-space when INSERTING into un-spaced source.)
            Expr::Propagate { expr } => {
                self.format_expr(expr);
                self.emitter.write("!");
            }
            Expr::MutableBorrow { expr } => {
                self.emitter.write("&");
                self.format_expr(expr);
            }
            Expr::Deref { expr } => {
                self.emitter.write("*");
                self.format_expr(expr);
            }
            Expr::If {
                condition,
                then_branch,
                elif_branches,
                else_branch,
            } => {
                self.emitter.write("if ");
                self.format_expr(condition);
                self.emitter.write(": ");
                self.format_expr(then_branch);
                for (cond, body) in elif_branches {
                    self.emitter.write(" elif ");
                    self.format_expr(cond);
                    self.emitter.write(": ");
                    self.format_expr(body);
                }
                if let Some(else_branch) = else_branch {
                    self.emitter.write(" else: ");
                    self.format_expr(else_branch);
                }
            }
            Expr::Match {
                scrutinee,
                arms,
                else_arm,
            } => {
                self.emitter.write("match ");
                self.format_expr(scrutinee);
                self.emitter.write(":");
                self.emitter.newline();
                self.emitter.indent();
                for arm in arms {
                    self.format_match_arm(arm);
                }
                if let Some(else_arm) = else_arm {
                    self.emitter.write("else: ");
                    self.format_expr(else_arm);
                    self.emitter.newline();
                }
                self.emitter.dedent();
            }
            Expr::Block(block) => {
                // gorget-js critique #2 (2026-05-13): `throw expr` and
                // `return [expr]` parse as expression prefixes by wrapping
                // the corresponding statement in a synthetic `Expr::Block`.
                // The formatter must round-trip those as the inline
                // expression form, not as `do:\n    throw expr` — the do-
                // wrapped form breaks `fmt_idempotent` (re-parsing the
                // do-block re-wraps it, then drops the surrounding var
                // decl as the syntactic shape drifts).
                if block.stmts.len() == 1 {
                    match &block.stmts[0].node {
                        Stmt::Throw(value) => {
                            self.emitter.write("throw ");
                            self.format_expr(value);
                            return;
                        }
                        Stmt::Return(value) => {
                            self.emitter.write("return");
                            if let Some(v) = value {
                                self.emitter.write(" ");
                                self.format_expr(v);
                            }
                            return;
                        }
                        _ => {}
                    }
                }
                self.emitter.write("do:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(block);
                self.emitter.dedent();
            }
            Expr::Do { body } => {
                self.emitter.write("do:");
                self.emitter.newline();
                self.emitter.indent();
                self.format_block_stmts(body);
                self.emitter.dedent();
            }
            Expr::Closure {
                is_move,
                is_async,
                params,
                body,
            } => {
                if *is_async {
                    self.emitter.write("async ");
                }
                if *is_move {
                    self.emitter.write("!");
                }
                let items: Vec<doc::Doc> = params.iter().map(|p| {
                    doc::text(self.element_to_string(|f| f.format_closure_param(&p.node)))
                }).collect();
                let params_doc = doc::surround("(", items, ")", true);
                self.write_doc(&params_doc);
                self.emitter.write(": ");
                // Total prelude stmts injected by the parser for tuple destructuring.
                let prelude_skip: usize = params
                    .iter()
                    .filter_map(|p| p.node.destructure.as_ref().map(|b| b.len()))
                    .sum();
                if let Expr::Block(ref block) = body.node {
                    // If the only post-prelude stmt is `return expr;`, render the closure
                    // as expression-body — mirrors the parser's wrap of inline `((...)): expr`
                    // bodies into `Block { ..prelude.., Stmt::Return(Some(expr)) }`.
                    let post_prelude: Vec<&Spanned<Stmt>> =
                        block.stmts.iter().skip(prelude_skip).collect();
                    let inline_expr = if post_prelude.len() == 1 {
                        match &post_prelude[0].node {
                            Stmt::Return(Some(e)) => Some(e.clone()),
                            _ => None,
                        }
                    } else {
                        None
                    };
                    if let Some(expr) = inline_expr {
                        self.format_expr(&expr);
                    } else {
                        self.emitter.newline();
                        self.emitter.indent();
                        if prelude_skip > 0 {
                            for stmt in &post_prelude {
                                self.emit_comments_before(stmt.span.start);
                                self.format_stmt(stmt);
                            }
                        } else {
                            self.format_block_stmts(block);
                        }
                        self.emitter.dedent();
                    }
                } else {
                    self.format_expr(body);
                }
            }
            Expr::ImplicitClosure { body } => {
                // ImplicitClosure is a parser artifact wrapping `it` expressions.
                // The formatter emits the body directly — the `it` keyword inside
                // already serves as the implicit parameter marker.
                self.format_expr(body);
            }
            Expr::ListComprehension {
                expr,
                variable,
                ownership,
                iterable,
                condition,
            } => {
                let expr_s = self.element_to_string(|f| f.format_expr(expr));
                let var_s = self.element_to_string(|f| f.format_pattern(variable));
                let own_prefix = match ownership {
                    Ownership::Borrow => "",
                    Ownership::MutableBorrow => "&",
                    Ownership::Move => "!",
                };
                let iter_s = self.element_to_string(|f| f.format_expr(iterable));
                let cond_s = condition.as_ref().map(|c| {
                    self.element_to_string(|f| f.format_expr(c))
                });
                let comp_doc = build_comprehension_doc(
                    "[", &expr_s, &var_s, own_prefix, &iter_s, cond_s.as_deref(), "]",
                );
                self.write_doc(&comp_doc);
            }
            Expr::DictComprehension {
                key,
                value,
                variables,
                iterable,
                condition,
            } => {
                let kv_s = self.element_to_string(|f| {
                    f.format_expr(key);
                    f.emitter.write(": ");
                    f.format_expr(value);
                });
                let vars_s = variables.iter().map(|v| v.node.as_str())
                    .collect::<Vec<_>>().join(", ");
                let iter_s = self.element_to_string(|f| f.format_expr(iterable));
                let cond_s = condition.as_ref().map(|c| {
                    self.element_to_string(|f| f.format_expr(c))
                });
                let comp_doc = build_comprehension_doc(
                    "{", &kv_s, &vars_s, "", &iter_s, cond_s.as_deref(), "}",
                );
                self.write_doc(&comp_doc);
            }
            Expr::SetComprehension {
                expr,
                variable,
                iterable,
                condition,
            } => {
                let expr_s = self.element_to_string(|f| f.format_expr(expr));
                let iter_s = self.element_to_string(|f| f.format_expr(iterable));
                let cond_s = condition.as_ref().map(|c| {
                    self.element_to_string(|f| f.format_expr(c))
                });
                let comp_doc = build_comprehension_doc(
                    "{", &expr_s, &variable.node, "", &iter_s, cond_s.as_deref(), "}",
                );
                self.write_doc(&comp_doc);
            }
            Expr::ArrayLiteral(elems) => {
                let items: Vec<doc::Doc> = elems.iter().map(|e| {
                    doc::text(self.element_to_string(|f| f.format_expr(e)))
                }).collect();
                let doc = doc::surround("[", items, "]", true);
                self.write_doc(&doc);
            }
            Expr::TupleLiteral(elems) => {
                if elems.len() == 1 {
                    // Single-element tuples always need trailing comma
                    self.emitter.write("(");
                    self.format_expr(&elems[0]);
                    self.emitter.write(",)");
                } else {
                    let items: Vec<doc::Doc> = elems.iter().map(|e| {
                        doc::text(self.element_to_string(|f| f.format_expr(e)))
                    }).collect();
                    let doc = doc::surround("(", items, ")", true);
                    self.write_doc(&doc);
                }
            }
            Expr::DictLiteral(pairs) => {
                let items: Vec<doc::Doc> = pairs.iter().map(|(k, v)| {
                    doc::text(self.element_to_string(|f| {
                        f.format_expr(k);
                        f.emitter.write(": ");
                        f.format_expr(v);
                    }))
                }).collect();
                let doc = doc::surround("{", items, "}", true);
                self.write_doc(&doc);
            }
            Expr::StructLiteral { name, generic_args, args } => {
                self.emitter.write(&name.node);
                if let Some(ga) = generic_args {
                    self.format_generic_args_wrapped(ga);
                }
                let items: Vec<doc::Doc> = args.iter().map(|a| {
                    doc::text(self.element_to_string(|f| f.format_expr(a)))
                }).collect();
                let doc = doc::surround("(", items, ")", true);
                self.write_doc(&doc);
            }
            Expr::As { expr, type_ } => {
                self.format_expr(expr);
                self.emitter.write(" as ");
                self.format_type(type_);
            }
            Expr::Await { expr } => {
                self.format_expr(expr);
                self.emitter.write(".await()");
            }
            Expr::Spawn { expr, unchecked } => {
                self.emitter.write(if *unchecked { "spawn unchecked " } else { "spawn " });
                self.format_expr(expr);
            }
            Expr::SpawnBlocking { expr, unchecked } => {
                self.emitter.write(if *unchecked { "spawn blocking unchecked " } else { "spawn blocking " });
                self.format_expr(expr);
            }
            Expr::Is {
                expr,
                negated,
                pattern,
            } => {
                self.format_expr(expr);
                if *negated {
                    self.emitter.write(" is not ");
                } else {
                    self.emitter.write(" is ");
                }
                self.format_pattern(pattern);
            }
            Expr::It => {
                self.emitter.write("it");
            }
            Expr::DotShorthand { variant, args } => {
                self.emitter.write(".");
                self.emitter.write(&variant.node);
                if !args.is_empty() {
                    self.format_call_args_wrapped(args);
                }
            }
            Expr::MetaOpInfix { left, op_name, right } => {
                self.format_expr(left);
                self.emitter.write(&format!(" meta[{op_name}] "));
                self.format_expr(right);
            }
            Expr::MetaOpToken(op) => {
                self.emitter.write("meta ");
                self.emitter.write(binary_op_str(*op));
            }
            Expr::Rethrow { expr, error_binding, transform } => {
                self.format_expr(expr);
                if let Some((error_type, error_name)) = error_binding {
                    self.emitter.write(" rethrow (");
                    self.format_type(error_type);
                    self.emitter.write(" ");
                    self.emitter.write(&error_name.node);
                    self.emitter.write("): ");
                } else {
                    self.emitter.write(" rethrow ");
                }
                self.format_expr(transform);
            }
            Expr::Catch { expr, error_binding, recovery } => {
                self.format_expr(expr);
                self.emitter.write(" catch (");
                self.emitter.write(&error_binding.node);
                self.emitter.write("): ");
                self.format_expr(recovery);
            }
        }
    }


    fn format_ownership_prefix(&mut self, ownership: Ownership) {
        match ownership {
            Ownership::Borrow => {}
            Ownership::MutableBorrow => self.emitter.write("&"),
            Ownership::Move => self.emitter.write("!"),
        }
    }

    fn format_call_arg(&mut self, arg: &CallArg) {
        if let Some(ref name) = arg.name {
            self.emitter.write(&name.node);
            self.emitter.write(" = ");
        }
        self.format_ownership_prefix(arg.ownership);
        self.format_expr(&arg.value);
    }

    fn format_closure_param(&mut self, param: &ClosureParam) {
        // Tuple destructuring: print `(T1 x, T2 y, ...)` from the source-level metadata
        // rather than the synthesised `(T1, T2) __dp_N` form.
        if let Some(ref bindings) = param.destructure {
            self.emitter.write("(");
            for (i, b) in bindings.iter().enumerate() {
                if i > 0 {
                    self.emitter.write(", ");
                }
                self.format_type(&b.type_);
                self.emitter.write(" ");
                self.format_ownership_prefix(b.ownership);
                self.emitter.write(&b.name.node);
            }
            self.emitter.write(")");
            return;
        }
        // type-first: `[type] [&|!]name`
        if let Some(ref ty) = param.type_ {
            self.format_type(ty);
            self.emitter.write(" ");
        }
        self.format_ownership_prefix(param.ownership);
        self.emitter.write(&param.name.node);
    }

    // ── String formatting ───────────────────────────────────

    fn format_string_lit(&mut self, s: &StringLiteral) {
        match s.kind {
            StringKind::Raw => self.emitter.write("r\""),
            StringKind::Byte => self.emitter.write("b\""),
            StringKind::CStr => self.emitter.write("c\""),
            StringKind::Format => self.emitter.write("f\""),
            StringKind::MultiLine => self.emitter.write("\"\"\""),
            StringKind::Normal => self.emitter.write("\""),
        }
        for seg in &s.segments {
            match seg {
                StringSegment::Literal(text) => {
                    self.format_string_escape(text, s.kind);
                }
                StringSegment::Interpolation(expr_text, spec) => {
                    self.emitter.write("{");
                    self.emitter.write(expr_text);
                    if let Some(fmt) = spec {
                        self.emitter.write(":");
                        self.emitter.write(fmt);
                    }
                    self.emitter.write("}");
                }
            }
        }
        match s.kind {
            StringKind::MultiLine => self.emitter.write("\"\"\""),
            _ => self.emitter.write("\""),
        }
    }

    fn format_string_escape(&mut self, text: &str, kind: StringKind) {
        if kind == StringKind::Raw {
            self.emitter.write(text);
            return;
        }
        for ch in text.chars() {
            match ch {
                '\n' => self.emitter.write("\\n"),
                '\t' => self.emitter.write("\\t"),
                '\r' => self.emitter.write("\\r"),
                '\\' => self.emitter.write("\\\\"),
                '"' => self.emitter.write("\\\""),
                '\0' => self.emitter.write("\\0"),
                '{' if kind == StringKind::Format => self.emitter.write("{{"),
                '}' if kind == StringKind::Format => self.emitter.write("}}"),
                c => {
                    let mut buf = [0u8; 4];
                    self.emitter.write(c.encode_utf8(&mut buf));
                }
            }
        }
    }

}

// ══════════════════════════════════════════════════════════════
// Helper functions
// ══════════════════════════════════════════════════════════════

fn binary_op_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Rem => "%",
        BinaryOp::Mod => "mod",
        BinaryOp::Pow => "**",
        BinaryOp::AddWrap => "+%",
        BinaryOp::SubWrap => "-%",
        BinaryOp::MulWrap => "*%",
        BinaryOp::Eq => "==",
        BinaryOp::Neq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::Gt => ">",
        BinaryOp::LtEq => "<=",
        BinaryOp::GtEq => ">=",
        BinaryOp::And => "and",
        BinaryOp::Or => "or",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::In => "in",
        // D26 fallible arithmetic.
        BinaryOp::AddFallible => "+!",
        BinaryOp::SubFallible => "-!",
        BinaryOp::MulFallible => "*!",
        BinaryOp::DivFallible => "/!",
        BinaryOp::RemFallible => "%!",
        BinaryOp::ShlFallible => "<<!",
        BinaryOp::ShrFallible => ">>!",
    }
}

fn compound_op_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+=",
        BinaryOp::Sub => "-=",
        BinaryOp::Mul => "*=",
        BinaryOp::Pow => "**=",
        BinaryOp::Div => "/=",
        BinaryOp::Rem => "%=",
        BinaryOp::AddWrap => "+%=",
        BinaryOp::SubWrap => "-%=",
        BinaryOp::MulWrap => "*%=",
        BinaryOp::BitAnd => "&=",
        BinaryOp::BitOr => "|=",
        BinaryOp::BitXor => "^=",
        BinaryOp::Shl => "<<=",
        BinaryOp::Shr => ">>=",
        _ => unreachable!("no compound assignment for {:?}", op),
    }
}

fn unary_op_str(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
        UnaryOp::Not => "not ",
        UnaryOp::BitNot => "~",
    }
}

fn primitive_type_str(p: PrimitiveType) -> &'static str {
    match p {
        PrimitiveType::Int => "int",
        PrimitiveType::Int8 => "int8",
        PrimitiveType::Int16 => "int16",
        PrimitiveType::Int32 => "int32",
        PrimitiveType::Int64 => "int64",
        PrimitiveType::Uint => "uint",
        PrimitiveType::Uint8 => "uint8",
        PrimitiveType::Uint16 => "uint16",
        PrimitiveType::Uint32 => "uint32",
        PrimitiveType::Uint64 => "uint64",
        PrimitiveType::Float => "float",
        PrimitiveType::Float32 => "float32",
        PrimitiveType::Float64 => "float64",
        PrimitiveType::Bool => "bool",
        PrimitiveType::CStr => "cstr",
        PrimitiveType::StringType => "String",
        PrimitiveType::Void => "void",
    }
}

// ══════════════════════════════════════════════════════════════
// Public API
// ══════════════════════════════════════════════════════════════

pub fn format_source(source: &str) -> String {
    let mut parser = crate::parser::Parser::new(source);
    let module = parser.parse_module();
    let comments = parser.comments;
    Formatter::new(comments).format(&module)
}

// ── Import sorting helpers ──────────────────────────────────

/// Extract the dotted path from an import item for sorting purposes.
fn import_sort_key(item: &Spanned<Item>) -> String {
    match &item.node {
        Item::Import(ImportStmt::Simple { path, .. })
        | Item::Import(ImportStmt::Grouped { path, .. })
        | Item::Import(ImportStmt::From { path, .. }) => {
            path.iter().map(|s| s.node.as_str()).collect::<Vec<_>>().join(".")
        }
        _ => String::new(),
    }
}

/// Returns true if the import path starts with `std` or `gg` (standard library).
fn is_std_import(path: &str) -> bool {
    path.starts_with("std.") || path.starts_with("xtd.") || path == "std" || path == "xtd"
}

// ── Expression chain helpers ────────────────────────────────

/// Count the length of a method call chain (consecutive `.method()` calls).
/// Returns 1 for a single method call, 2+ for chains.
fn method_chain_length(expr: &Spanned<Expr>) -> usize {
    match &expr.node {
        Expr::MethodCall { receiver, .. } => 1 + method_chain_length(receiver),
        _ => 0,
    }
}

/// Collect method chain segments from outermost to innermost.
/// Returns (root_expr, vec of (method_name, generic_args, args)) from left to right.
fn collect_method_chain<'a>(
    expr: &'a Spanned<Expr>,
) -> (
    &'a Spanned<Expr>,
    Vec<(
        &'a Spanned<String>,
        &'a Option<Vec<Spanned<Type>>>,
        &'a Vec<Spanned<CallArg>>,
    )>,
) {
    let mut segments = Vec::new();
    let mut current = expr;
    loop {
        match &current.node {
            Expr::MethodCall {
                receiver,
                method,
                generic_args,
                args,
            } => {
                segments.push((method, generic_args, args));
                current = receiver;
            }
            _ => break,
        }
    }
    segments.reverse();
    (current, segments)
}

/// Flatten a left-associative binary expression chain of the same operator.
/// `a + b + c` is parsed as `(a + b) + c`. This collects `[a, b]` into `operands`
/// (the caller adds `c`).
fn collect_binary_operands<'a>(
    expr: &'a Spanned<Expr>,
    target_op: BinaryOp,
    operands: &mut Vec<&'a Spanned<Expr>>,
) {
    match &expr.node {
        Expr::BinaryOp { left, op, right } if *op == target_op => {
            collect_binary_operands(left, target_op, operands);
            operands.push(right);
        }
        _ => {
            operands.push(expr);
        }
    }
}

/// Build a Doc for a comprehension expression with line-width-aware wrapping.
/// Flat: `[expr for var in iterable if cond]`
/// Broken:
/// ```text
/// [
///     expr
///     for var in iterable
///     if cond
/// ]
/// ```
fn build_comprehension_doc(
    open: &str,
    expr_s: &str,
    var_s: &str,
    own_prefix: &str,
    iter_s: &str,
    cond_s: Option<&str>,
    close: &str,
) -> doc::Doc {
    let mut inner = vec![
        doc::text(expr_s),
        doc::line(),
        doc::text(format!("for {var_s} in {own_prefix}{iter_s}")),
    ];
    if let Some(cond) = cond_s {
        inner.push(doc::line());
        inner.push(doc::text(format!("if {cond}")));
    }

    doc::group(doc::concat(vec![
        doc::text(open),
        doc::indent(doc::concat(vec![doc::softline(), doc::concat(inner)])),
        doc::softline(),
        doc::text(close),
    ]))
}

// ══════════════════════════════════════════════════════════════
// Tests
// ══════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    fn fmt(source: &str) -> String {
        format_source(source)
    }

    #[test]
    fn test_simple_function() {
        let input = "void main():\n    pass\n";
        let output = fmt(input);
        assert_eq!(output, "void main():\n    pass\n");
    }

    #[test]
    fn test_expression_body() {
        let input = "int double(int x): x * 2\n";
        let output = fmt(input);
        assert_eq!(output, "int double(int x): x * 2\n");
    }

    #[test]
    fn test_struct() {
        let input = "struct Point:\n    float x\n    float y\n";
        let output = fmt(input);
        assert_eq!(output, "struct Point:\n    float x\n    float y\n");
    }

    #[test]
    fn test_enum() {
        let input = "enum Color:\n    Red\n    Green\n    Blue\n";
        let output = fmt(input);
        assert_eq!(output, "enum Color:\n    Red\n    Green\n    Blue\n");
    }

    #[test]
    fn test_comment_preservation() {
        let input = "# This is a comment\nvoid main():\n    pass\n";
        let output = fmt(input);
        assert!(output.contains("# This is a comment"));
    }

    #[test]
    fn test_inline_comment() {
        let input = "void main():\n    int x = 5  # inline\n";
        let output = fmt(input);
        // The inline comment should be preserved somewhere in the output
        assert!(output.contains("# inline"));
    }

    #[test]
    fn test_idempotency_simple() {
        let input = "void main():\n    int x = 42\n    print(\"{x}\")\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "Formatter is not idempotent");
    }

    #[test]
    fn test_static_public_preserved() {
        // Static globals are private-by-default, so the `public` keyword
        // is meaningful and must survive formatting — otherwise a
        // re-parse assigns Private visibility and the global stops being
        // importable from other modules.
        let input = "public static int x = 42\n";
        let output = fmt(input);
        assert_eq!(output, "public static int x = 42\n");
    }

    #[test]
    fn test_static_private_unchanged() {
        let input = "static int x = 42\n";
        let output = fmt(input);
        assert_eq!(output, "static int x = 42\n");
    }

    #[test]
    fn test_import() {
        let input = "import std.io\n";
        let output = fmt(input);
        assert_eq!(output, "import std.io\n");
    }

    #[test]
    fn test_from_import() {
        let input = "from std.fmt import Displayable\n";
        let output = fmt(input);
        assert_eq!(output, "from std.fmt import Displayable\n");
    }

    #[test]
    fn test_trait_and_equip() {
        let input = "\
trait Shape:
    float area(self)

struct Circle:
    float radius

equip Circle with Shape:
    float area(self):
        return 3.14
";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "Formatter is not idempotent for trait+equip");
    }

    #[test]
    fn test_match_stmt() {
        let input = "\
void main():
    match x:
        case 1: print(\"one\")
        case 2: print(\"two\")
        else:
            print(\"other\")
";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second, "Formatter is not idempotent for match");
    }

    #[test]
    fn test_closure() {
        let input = "void main():\n    auto add = (int a, int b): a + b\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_if_elif_else() {
        let input = "\
void main():
    if x > 0:
        pass
    elif x < 0:
        pass
    else:
        pass
";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_for_loop() {
        let input = "void main():\n    for i in 0..10:\n        print(\"{i}\")\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_type_alias() {
        let input = "type StringList = Vector[String]\n";
        let output = fmt(input);
        assert_eq!(output, "type StringList = Vector[String]\n");
    }

    #[test]
    fn test_newtype() {
        let input = "newtype UserId(int)\n";
        let output = fmt(input);
        assert_eq!(output, "newtype UserId(int)\n");
    }

    #[test]
    fn test_import_name_sorting() {
        // Names within `from` imports should be sorted alphabetically.
        let input = "from std.io import Writer, Reader, Closer\n";
        let output = fmt(input);
        assert_eq!(output, "from std.io import Closer, Reader, Writer\n");
    }

    #[test]
    fn test_import_order_sorting() {
        // std imports should come before third-party imports.
        let input = "import mylib.utils\n\nimport std.io\n\nimport xtd.log\n";
        let output = fmt(input);
        assert_eq!(output, "import std.io\n\nimport xtd.log\n\nimport mylib.utils\n");
    }

    #[test]
    fn test_method_chain_idempotent() {
        let input = "void main():\n    auto x = items.filter(pred).map(f).collect()\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_binary_expr_idempotent() {
        let input = "void main():\n    int x = a + b + c\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_binary_expr_preserves_operators() {
        let input = "void main():\n    bool x = a and b or c\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_list_comprehension_idempotent() {
        let input = "void main():\n    auto items = [x * 2 for x in range(10) if x > 0]\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_dict_comprehension_idempotent() {
        let input = "void main():\n    auto d = {k: v for k, v in items}\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }

    #[test]
    fn test_set_comprehension_idempotent() {
        let input = "void main():\n    auto s = {x for x in items if x > 0}\n";
        let first = fmt(input);
        let second = fmt(&first);
        assert_eq!(first, second);
    }
}
