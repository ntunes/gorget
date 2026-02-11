use crate::lexer::token::{StringKind, StringLit, StringSegment};
use crate::parser::ast::*;
use crate::span::Spanned;

// ══════════════════════════════════════════════════════════════
// Emitter — indentation-aware output buffer
// ══════════════════════════════════════════════════════════════

struct Emitter {
    buf: String,
    indent: usize,
    at_line_start: bool,
}

impl Emitter {
    fn new() -> Self {
        Self {
            buf: String::new(),
            indent: 0,
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
            for _ in 0..self.indent {
                self.buf.push_str("    ");
            }
            self.at_line_start = false;
        }
        self.buf.push_str(s);
    }

    fn newline(&mut self) {
        self.buf.push('\n');
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
        // Ensure trailing newline
        if !result.ends_with('\n') {
            result.push('\n');
        }
        result
    }

    // ── Comment interleaving ────────────────────────────────

    fn emit_comments_before(&mut self, pos: usize) {
        while self.comment_cursor < self.comments.len() {
            let c = &self.comments[self.comment_cursor];
            if c.span.start < pos {
                let text = c.node.clone();
                self.emitter.write(&text);
                self.emitter.newline();
                self.comment_cursor += 1;
            } else {
                break;
            }
        }
    }

    fn emit_remaining_comments(&mut self) {
        while self.comment_cursor < self.comments.len() {
            let text = self.comments[self.comment_cursor].node.clone();
            self.emitter.write(&text);
            self.emitter.newline();
            self.comment_cursor += 1;
        }
    }

    // ── Module ──────────────────────────────────────────────

    fn format_module(&mut self, module: &Module) {
        for (i, item) in module.items.iter().enumerate() {
            if i > 0 {
                self.emitter.blank_line();
            }
            self.emit_comments_before(item.span.start);
            self.format_item(item);
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
        }
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
        if *vis == Visibility::Public {
            self.emitter.write("public ");
        }
    }

    fn format_function(&mut self, f: &FunctionDef) {
        self.format_doc_comment(&f.doc_comment);
        self.format_attributes(&f.attributes);
        self.format_visibility(&f.visibility);
        self.format_qualifiers(&f.qualifiers);
        self.format_type(&f.return_type);
        self.emitter.write(" ");
        self.emitter.write(&f.name.node);
        if let Some(ref gp) = f.generic_params {
            self.format_generic_params(gp);
        }
        self.emitter.write("(");
        self.format_params(&f.params);
        self.emitter.write(")");
        if let Some(ref throws) = f.throws {
            self.emitter.write(" throws ");
            self.format_type(throws);
        }
        if let Some(ref wc) = f.where_clause {
            self.format_where_clause(wc);
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
                self.emitter.write(" = ");
                self.format_expr(expr);
                self.emitter.newline();
            }
            FunctionBody::Declaration => {
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
            self.format_generic_params(gp);
        }
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        for field in &s.fields {
            self.emit_comments_before(field.span.start);
            if field.node.visibility == Visibility::Public {
                self.emitter.write("public ");
            }
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
            self.format_generic_params(gp);
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
            self.format_generic_params(gp);
        }
        if !t.extends.is_empty() {
            self.emitter.write(" extends ");
            for (i, bound) in t.extends.iter().enumerate() {
                if i > 0 {
                    self.emitter.write(" + ");
                }
                self.format_trait_bound(bound);
            }
        }
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        for item in &t.items {
            self.emit_comments_before(item.span.start);
            match &item.node {
                TraitItem::Method(f) => self.format_function(f),
                TraitItem::AssociatedType(at) => {
                    self.emitter.write("type ");
                    self.emitter.write(&at.name.node);
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
            self.format_generic_params(gp);
        }
        self.format_type(&e.type_);
        if let Some(ref trait_) = e.trait_ {
            self.emitter.write(" with ");
            self.format_type(&trait_.trait_name);
        }
        if let Some(ref wc) = e.where_clause {
            self.format_where_clause(wc);
        }
        self.emitter.write(":");
        self.emitter.newline();
        self.emitter.indent();
        for (i, method) in e.items.iter().enumerate() {
            if i > 0 {
                self.emitter.blank_line();
            }
            self.emit_comments_before(method.span.start);
            self.format_function(&method.node);
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
                self.emitter.write(".{");
                for (j, name) in names.iter().enumerate() {
                    if j > 0 {
                        self.emitter.write(", ");
                    }
                    self.emitter.write(&name.node);
                }
                self.emitter.write("}");
                self.emitter.newline();
            }
            ImportStmt::From { path, names, .. } => {
                self.emitter.write("from ");
                self.format_dotted_path(path);
                self.emitter.write(" import ");
                for (j, name) in names.iter().enumerate() {
                    if j > 0 {
                        self.emitter.write(", ");
                    }
                    self.emitter.write(&name.node);
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
        self.emitter.write("type ");
        self.emitter.write(&ta.name.node);
        if let Some(ref gp) = ta.generic_params {
            self.format_generic_params(gp);
        }
        self.emitter.write(" = ");
        self.format_type(&ta.type_);
        self.emitter.newline();
    }

    fn format_newtype(&mut self, nt: &NewtypeDef) {
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
        self.format_visibility(&sd.visibility);
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

    fn format_generic_params(&mut self, gp: &Spanned<GenericParams>) {
        self.emitter.write("[");
        for (i, param) in gp.node.params.iter().enumerate() {
            if i > 0 {
                self.emitter.write(", ");
            }
            match &param.node {
                GenericParam::Type(name) => self.emitter.write(&name.node),
                GenericParam::Lifetime(name) => {
                    self.emitter.write("life ");
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
        self.emitter.write("]");
    }

    fn format_where_clause(&mut self, wc: &Spanned<WhereClause>) {
        self.emitter.write(" where ");
        for (i, bound) in wc.node.bounds.iter().enumerate() {
            if i > 0 {
                self.emitter.write(", ");
            }
            self.emitter.write(&bound.node.type_name.node);
            self.emitter.write(" is ");
            for (j, tb) in bound.node.bounds.iter().enumerate() {
                if j > 0 {
                    self.emitter.write(" + ");
                }
                self.format_trait_bound(tb);
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

    fn format_params(&mut self, params: &[Spanned<Param>]) {
        for (i, param) in params.iter().enumerate() {
            if i > 0 {
                self.emitter.write(", ");
            }
            self.format_param(&param.node);
        }
    }

    fn format_param(&mut self, param: &Param) {
        if param.is_live {
            self.emitter.write("live ");
        }
        // self parameter
        if matches!(param.type_.node, Type::SelfType) {
            match param.ownership {
                Ownership::Borrow => self.emitter.write("self"),
                Ownership::MutableBorrow => self.emitter.write("&self"),
                Ownership::Move => self.emitter.write("!self"),
            }
            return;
        }
        self.format_type(&param.type_);
        self.emitter.write(" ");
        match param.ownership {
            Ownership::Borrow => {}
            Ownership::MutableBorrow => self.emitter.write("&"),
            Ownership::Move => self.emitter.write("!"),
        }
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

    fn format_stmt(&mut self, stmt: &Spanned<Stmt>) {
        match &stmt.node {
            Stmt::VarDecl {
                is_const,
                type_,
                pattern,
                value,
            } => {
                if *is_const {
                    self.emitter.write("const ");
                }
                self.format_type(type_);
                self.emitter.write(" ");
                self.format_pattern(pattern);
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
                self.format_expr(expr);
                self.emitter.newline();
            }
            Stmt::Throw(expr) => {
                self.emitter.write("throw ");
                self.format_expr(expr);
                self.emitter.newline();
            }
            Stmt::Break(None) => {
                self.emitter.write("break");
                self.emitter.newline();
            }
            Stmt::Break(Some(expr)) => {
                self.emitter.write("break ");
                self.format_expr(expr);
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
                self.format_pattern(pattern);
                self.emitter.write(" in ");
                match ownership {
                    Ownership::Borrow => {}
                    Ownership::MutableBorrow => self.emitter.write("&"),
                    Ownership::Move => self.emitter.write("!"),
                }
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
                for arm in arms {
                    self.format_match_arm(arm);
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
            Stmt::Item(item) => {
                let spanned = Spanned::new(*item.clone(), stmt.span);
                self.format_item(&spanned);
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
                    self.emitter.write("[");
                    for (i, arg) in generic_args.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_type(arg);
                    }
                    self.emitter.write("]");
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
            } => {
                self.format_type(return_type);
                self.emitter.write("(");
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_type(p);
                }
                self.emitter.write(")");
            }
            Type::Dynamic { trait_ } => {
                self.emitter.write("dynamic ");
                self.format_type(trait_);
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
        }
    }

    // ── Expressions ─────────────────────────────────────────

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
            Expr::CharLiteral(c) => {
                self.emitter.write("'");
                self.format_char_escape(*c);
                self.emitter.write("'");
            }
            Expr::StringLiteral(s) => {
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
                self.format_expr(left);
                self.emitter.write(" ");
                self.emitter.write(binary_op_str(*op));
                self.emitter.write(" ");
                self.format_expr(right);
            }
            Expr::Call {
                callee,
                generic_args,
                args,
            } => {
                self.format_expr(callee);
                if let Some(ga) = generic_args {
                    self.emitter.write("[");
                    for (i, ty) in ga.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_type(ty);
                    }
                    self.emitter.write("]");
                }
                self.emitter.write("(");
                self.format_call_args(args);
                self.emitter.write(")");
            }
            Expr::MethodCall {
                receiver,
                method,
                generic_args,
                args,
            } => {
                self.format_expr(receiver);
                self.emitter.write(".");
                self.emitter.write(&method.node);
                if let Some(ga) = generic_args {
                    self.emitter.write("[");
                    for (i, ty) in ga.iter().enumerate() {
                        if i > 0 {
                            self.emitter.write(", ");
                        }
                        self.format_type(ty);
                    }
                    self.emitter.write("]");
                }
                self.emitter.write("(");
                self.format_call_args(args);
                self.emitter.write(")");
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
            Expr::NilCoalescing { lhs, rhs } => {
                self.format_expr(lhs);
                self.emitter.write(" ?? ");
                self.format_expr(rhs);
            }
            Expr::Try { expr } => {
                self.format_expr(expr);
                self.emitter.write("?");
            }
            Expr::Move { expr } => {
                self.emitter.write("!");
                self.format_expr(expr);
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
                self.emitter.write("(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_closure_param(&param.node);
                }
                self.emitter.write("): ");
                // Check if closure body is a multi-line block
                if let Expr::Block(ref block) = body.node {
                    self.emitter.newline();
                    self.emitter.indent();
                    self.format_block_stmts(block);
                    self.emitter.dedent();
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
                self.emitter.write("[");
                self.format_expr(expr);
                self.emitter.write(" for ");
                self.format_pattern(variable);
                self.emitter.write(" in ");
                match ownership {
                    Ownership::Borrow => {}
                    Ownership::MutableBorrow => self.emitter.write("&"),
                    Ownership::Move => self.emitter.write("!"),
                }
                self.format_expr(iterable);
                if let Some(cond) = condition {
                    self.emitter.write(" if ");
                    self.format_expr(cond);
                }
                self.emitter.write("]");
            }
            Expr::DictComprehension {
                key,
                value,
                variables,
                iterable,
                condition,
            } => {
                self.emitter.write("{");
                self.format_expr(key);
                self.emitter.write(": ");
                self.format_expr(value);
                self.emitter.write(" for ");
                for (i, var) in variables.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.emitter.write(&var.node);
                }
                self.emitter.write(" in ");
                self.format_expr(iterable);
                if let Some(cond) = condition {
                    self.emitter.write(" if ");
                    self.format_expr(cond);
                }
                self.emitter.write("}");
            }
            Expr::SetComprehension {
                expr,
                variable,
                iterable,
                condition,
            } => {
                self.emitter.write("{");
                self.format_expr(expr);
                self.emitter.write(" for ");
                self.emitter.write(&variable.node);
                self.emitter.write(" in ");
                self.format_expr(iterable);
                if let Some(cond) = condition {
                    self.emitter.write(" if ");
                    self.format_expr(cond);
                }
                self.emitter.write("}");
            }
            Expr::ArrayLiteral(elems) => {
                self.emitter.write("[");
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_expr(e);
                }
                self.emitter.write("]");
            }
            Expr::TupleLiteral(elems) => {
                self.emitter.write("(");
                for (i, e) in elems.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_expr(e);
                }
                // Single-element tuples need trailing comma
                if elems.len() == 1 {
                    self.emitter.write(",");
                }
                self.emitter.write(")");
            }
            Expr::StructLiteral { name, args } => {
                self.emitter.write(&name.node);
                self.emitter.write("(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.emitter.write(", ");
                    }
                    self.format_expr(arg);
                }
                self.emitter.write(")");
            }
            Expr::As { expr, type_ } => {
                self.format_expr(expr);
                self.emitter.write(" as ");
                self.format_type(type_);
            }
            Expr::Await { expr } => {
                self.emitter.write("await ");
                self.format_expr(expr);
            }
            Expr::Spawn { expr } => {
                self.emitter.write("spawn ");
                self.format_expr(expr);
            }
            Expr::TryCapture { expr } => {
                self.emitter.write("try ");
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
        }
    }

    fn format_call_args(&mut self, args: &[Spanned<CallArg>]) {
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                self.emitter.write(", ");
            }
            if let Some(ref name) = arg.node.name {
                self.emitter.write(&name.node);
                self.emitter.write(" = ");
            }
            match arg.node.ownership {
                Ownership::Borrow => {}
                Ownership::MutableBorrow => self.emitter.write("&"),
                Ownership::Move => self.emitter.write("!"),
            }
            self.format_expr(&arg.node.value);
        }
    }

    fn format_closure_param(&mut self, param: &ClosureParam) {
        if let Some(ref ty) = param.type_ {
            self.format_type(ty);
            self.emitter.write(" ");
        }
        match param.ownership {
            Ownership::Borrow => {}
            Ownership::MutableBorrow => self.emitter.write("&"),
            Ownership::Move => self.emitter.write("!"),
        }
        self.emitter.write(&param.name.node);
    }

    // ── String formatting ───────────────────────────────────

    fn format_string_lit(&mut self, s: &StringLit) {
        match s.kind {
            StringKind::Raw => self.emitter.write("r\""),
            StringKind::Byte => self.emitter.write("b\""),
            StringKind::MultiLine => self.emitter.write("\"\"\""),
            StringKind::Normal => self.emitter.write("\""),
        }
        for seg in &s.segments {
            match seg {
                StringSegment::Literal(text) => {
                    self.format_string_escape(text, s.kind);
                }
                StringSegment::Interpolation(expr_text) => {
                    self.emitter.write("{");
                    self.emitter.write(expr_text);
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
                c => {
                    let mut buf = [0u8; 4];
                    self.emitter.write(c.encode_utf8(&mut buf));
                }
            }
        }
    }

    fn format_char_escape(&mut self, c: char) {
        match c {
            '\n' => self.emitter.write("\\n"),
            '\t' => self.emitter.write("\\t"),
            '\r' => self.emitter.write("\\r"),
            '\\' => self.emitter.write("\\\\"),
            '\'' => self.emitter.write("\\'"),
            '\0' => self.emitter.write("\\0"),
            c => {
                let mut buf = [0u8; 4];
                self.emitter.write(c.encode_utf8(&mut buf));
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
        BinaryOp::Mod => "%",
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
        BinaryOp::In => "in",
    }
}

fn compound_op_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+=",
        BinaryOp::Sub => "-=",
        BinaryOp::Mul => "*=",
        BinaryOp::Div => "/=",
        BinaryOp::Mod => "%=",
        BinaryOp::AddWrap => "+%=",
        BinaryOp::SubWrap => "-%=",
        BinaryOp::MulWrap => "*%=",
        _ => "=",
    }
}

fn unary_op_str(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "-",
        UnaryOp::Not => "not ",
        UnaryOp::Deref => "*",
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
        PrimitiveType::Char => "char",
        PrimitiveType::Str => "str",
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
        let input = "int double(int x) = x * 2\n";
        let output = fmt(input);
        assert_eq!(output, "int double(int x) = x * 2\n");
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
}
