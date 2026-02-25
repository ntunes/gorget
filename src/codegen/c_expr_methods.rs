/// Collection/type method dispatch codegen: Vector, Map, Set, String, Box,
/// Option, Result, File, HTTP, Socket, Cipher method calls and helpers.
use crate::parser::ast::{Expr, Type};
use crate::span::Spanned;

use super::c_mangle;
use super::c_types;
use super::c_expr::sort_comparator_for_type;
use super::CodegenContext;

impl CodegenContext<'_> {
    /// Generate code for Vector method calls.
    pub(super) fn gen_vector_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let elem_type = self.infer_vector_elem_type(receiver);

        // For methods that need &recv, wrap non-lvalue receivers in a temp
        let recv_ref = if needs_temp {
            format!("({{ __typeof__({recv}) __recv = {recv}; &__recv; }})")
        } else {
            format!("&{recv}")
        };

        match method_name {
            "push" => {
                let arg_expr = &args[0].node.value;
                let arg = self.gen_expr(arg_expr);
                let source_will_be_zeroed = self.source_is_safe_for_push(&arg_expr.node);
                let clone_code = if !source_will_be_zeroed {
                    self.compute_push_clone_code(&elem_type, "__push_val")
                } else {
                    None
                };
                if let Some(clone) = clone_code {
                    format!("({{ {elem_type} __push_val = {arg}; {clone} gorget_array_push({recv_ref}, &__push_val); }})")
                } else {
                    format!("({{ {elem_type} __push_val = {arg}; gorget_array_push({recv_ref}, &__push_val); }})")
                }
            }
            "len" => {
                format!("(int64_t)gorget_array_len({recv_ref})")
            }
            "get" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let opt = self.register_generic("Option", &[elem_type.clone()], super::GenericInstanceKind::Enum);
                let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                let ctor_none = c_mangle::mangle_variant(&opt, "None");
                let clone_code = self.compute_push_clone_code(&elem_type, "__gc_elem");
                if let Some(clone) = &clone_code {
                    self.deep_cloned_expr = true;
                    format!(
                        "({{ int64_t __gi = {idx}; GorgetArray __gr_src = {recv}; {opt} __gr; \
                        if (__gi >= 0 && (size_t)__gi < __gr_src.len) \
                        {{ {elem_type} __gc_elem = GORGET_ARRAY_AT({elem_type}, __gr_src, __gi); \
                        {clone} __gr = {ctor_some}(__gc_elem); }} \
                        else {{ __gr = {ctor_none}(); }} __gr; }})"
                    )
                } else {
                    format!(
                        "({{ int64_t __gi = {idx}; GorgetArray __gr_src = {recv}; {opt} __gr; \
                        if (__gi >= 0 && (size_t)__gi < __gr_src.len) \
                        {{ __gr = {ctor_some}(GORGET_ARRAY_AT({elem_type}, __gr_src, __gi)); }} \
                        else {{ __gr = {ctor_none}(); }} __gr; }})"
                    )
                }
            }
            "pop" => {
                let opt = self.register_generic("Option", &[elem_type.clone()], super::GenericInstanceKind::Enum);
                let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                let ctor_none = c_mangle::mangle_variant(&opt, "None");
                if needs_temp {
                    format!("({{ __typeof__({recv}) __recv = {recv}; {opt} __pr; \
                    if (__recv.len > 0) {{ __pr = {ctor_some}(GORGET_ARRAY_AT({elem_type}, __recv, __recv.len - 1)); __recv.len--; }} \
                    else {{ __pr = {ctor_none}(); }} __pr; }})")
                } else {
                    format!("({{ {opt} __pr; \
                    if ({recv}.len > 0) {{ __pr = {ctor_some}(GORGET_ARRAY_AT({elem_type}, {recv}, {recv}.len - 1)); {recv}.len--; }} \
                    else {{ __pr = {ctor_none}(); }} __pr; }})")
                }
            }
            "set" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let val_expr = &args[1].node.value;
                let val = self.gen_expr(val_expr);
                let source_will_be_zeroed = self.source_is_safe_for_push(&val_expr.node);
                let clone_code = if !source_will_be_zeroed {
                    self.compute_push_clone_code(&elem_type, "__set_val")
                } else {
                    None
                };
                let elem_type_id = self.resolve_expr_type_id(receiver).and_then(|tid| {
                    match self.types.get(tid) {
                        crate::semantic::types::ResolvedType::Generic(_, args) if !args.is_empty() => Some(args[0]),
                        crate::semantic::types::ResolvedType::Array(elem_tid, _) => Some(*elem_tid),
                        _ => None,
                    }
                });
                let predrop_code = self.compute_predrop_code(&elem_type, "__old_val", elem_type_id);
                if predrop_code.is_some() || clone_code.is_some() {
                    let predrop = predrop_code.map(|code| format!(
                        "if ((size_t)__set_idx < {recv}.len) {{ \
                         {elem_type} __old_val = GORGET_ARRAY_AT({elem_type}, {recv}, __set_idx); {code} }}"
                    )).unwrap_or_default();
                    let clone = clone_code.unwrap_or_default();
                    format!("({{ int64_t __set_idx = {idx}; {elem_type} __set_val = {val}; {clone} \
                             {predrop} gorget_array_set({recv_ref}, __set_idx, &__set_val); }})")
                } else {
                    format!("({{ {elem_type} __set_val = {val}; gorget_array_set({recv_ref}, {idx}, &__set_val); }})")
                }
            }
            "remove" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let opt = self.register_generic("Option", &[elem_type.clone()], super::GenericInstanceKind::Enum);
                let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                let ctor_none = c_mangle::mangle_variant(&opt, "None");
                if needs_temp {
                    format!("({{ __typeof__({recv}) __recv = {recv}; int64_t __ri = {idx}; {opt} __rr; \
                    if (__ri >= 0 && (size_t)__ri < __recv.len) \
                    {{ __rr = {ctor_some}(GORGET_ARRAY_AT({elem_type}, __recv, __ri)); gorget_array_remove(&__recv, __ri); }} \
                    else {{ __rr = {ctor_none}(); }} __rr; }})")
                } else {
                    format!("({{ int64_t __ri = {idx}; {opt} __rr; \
                    if (__ri >= 0 && (size_t)__ri < {recv}.len) \
                    {{ __rr = {ctor_some}(GORGET_ARRAY_AT({elem_type}, {recv}, __ri)); gorget_array_remove(&{recv}, __ri); }} \
                    else {{ __rr = {ctor_none}(); }} __rr; }})")
                }
            }
            "clear" => {
                format!("gorget_array_clear({recv_ref})")
            }
            "is_empty" => {
                format!("(gorget_array_len({recv_ref}) == 0)")
            }
            "contains" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {elem_type} __needle = {arg}; gorget_array_contains({recv_ref}, &__needle, sizeof({elem_type})); }})")
            }
            "reserve" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("gorget_array_reserve({recv_ref}, (size_t)({arg}))")
            }
            "sort" => {
                let cmp = sort_comparator_for_type(&elem_type);
                format!("({{ qsort({recv}.data, {recv}.len, sizeof({elem_type}), {cmp}); }})")
            }
            "sorted" => {
                let cmp = sort_comparator_for_type(&elem_type);
                format!(
                    "({{ GorgetArray __sorted_src = {recv}; \
                    GorgetArray __sorted_dst = gorget_array_new(sizeof({elem_type})); \
                    if (__sorted_src.len > 0) {{ \
                        __sorted_dst.data = GORGET_ALLOC(__sorted_src.len * sizeof({elem_type})); \
                        memcpy(__sorted_dst.data, __sorted_src.data, __sorted_src.len * sizeof({elem_type})); \
                        __sorted_dst.len = __sorted_src.len; \
                        __sorted_dst.cap = __sorted_src.len; \
                    }} \
                    qsort(__sorted_dst.data, __sorted_dst.len, sizeof({elem_type}), {cmp}); \
                    __sorted_dst; }})"
                )
            }
            "reverse" => {
                format!("gorget_array_reverse({recv_ref})")
            }
            "index_of" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let opt = self.register_generic("Option", &["int64_t".to_string()], super::GenericInstanceKind::Enum);
                let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                let ctor_none = c_mangle::mangle_variant(&opt, "None");
                format!("({{ {elem_type} __needle = {arg}; int64_t __idx = gorget_array_index_of({recv_ref}, &__needle); \
                __idx >= 0 ? {ctor_some}(__idx) : {ctor_none}(); }})")
            }
            "insert" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let val = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {elem_type} __ins_val = {val}; gorget_array_insert({recv_ref}, (size_t)({idx}), &__ins_val); }})")
            }
            "extend" => {
                let arg = self.gen_expr(&args[0].node.value);
                format!("({{ GorgetArray __ext_src = {arg}; gorget_array_extend({recv_ref}, &__ext_src); }})")
            }
            "slice" => {
                let start = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let end = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("gorget_array_slice({recv_ref}, {start}, {end})")
            }
            "any" => {
                self.closure_return_type_hint = Some("bool".to_string());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetArray __any_src = {recv}; \
                    bool __any_result = false; \
                    for (size_t __any_i = 0; __any_i < __any_src.len; __any_i++) {{ \
                        {elem_type} __any_elem = GORGET_ARRAY_AT({elem_type}, __any_src, __any_i); \
                        if ({closure_fn}(__any_elem)) {{ __any_result = true; break; }} \
                    }} \
                    __any_result; }})"
                )
            }
            "all" => {
                self.closure_return_type_hint = Some("bool".to_string());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetArray __all_src = {recv}; \
                    bool __all_result = true; \
                    for (size_t __all_i = 0; __all_i < __all_src.len; __all_i++) {{ \
                        {elem_type} __all_elem = GORGET_ARRAY_AT({elem_type}, __all_src, __all_i); \
                        if (!{closure_fn}(__all_elem)) {{ __all_result = false; break; }} \
                    }} \
                    __all_result; }})"
                )
            }
            "filter" => {
                self.closure_return_type_hint = Some("bool".to_string());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetArray __filt_src = {recv}; \
                    GorgetArray __filt_result = gorget_array_new(sizeof({elem_type})); \
                    for (size_t __filt_i = 0; __filt_i < __filt_src.len; __filt_i++) {{ \
                        {elem_type} __filt_elem = GORGET_ARRAY_AT({elem_type}, __filt_src, __filt_i); \
                        if ({closure_fn}(__filt_elem)) {{ gorget_array_push(&__filt_result, &__filt_elem); }} \
                    }} \
                    __filt_result; }})"
                )
            }
            "map" => {
                let body_c_type = self.infer_closure_body_c_type(&args[0].node.value);
                self.closure_return_type_hint = Some(body_c_type.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetArray __map_src = {recv}; \
                    GorgetArray __map_result = gorget_array_new(sizeof({body_c_type})); \
                    for (size_t __map_i = 0; __map_i < __map_src.len; __map_i++) {{ \
                        {elem_type} __map_elem = GORGET_ARRAY_AT({elem_type}, __map_src, __map_i); \
                        {body_c_type} __map_out = {closure_fn}(__map_elem); \
                        gorget_array_push(&__map_result, &__map_out); \
                    }} \
                    __map_result; }})"
                )
            }
            "fold" => {
                let init = self.gen_expr(&args[0].node.value);
                let init_c_type = self.infer_c_type_from_expr(&args[0].node.value.node);
                self.closure_return_type_hint = Some(init_c_type.clone());
                let closure_fn = self.gen_expr(&args[1].node.value);
                format!(
                    "({{ GorgetArray __fold_src = {recv}; \
                    {init_c_type} __fold_acc = {init}; \
                    for (size_t __fold_i = 0; __fold_i < __fold_src.len; __fold_i++) {{ \
                        {elem_type} __fold_elem = GORGET_ARRAY_AT({elem_type}, __fold_src, __fold_i); \
                        __fold_acc = {closure_fn}(__fold_acc, __fold_elem); \
                    }} \
                    __fold_acc; }})"
                )
            }
            "reduce" => {
                self.closure_return_type_hint = Some(elem_type.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetArray __red_src = {recv}; \
                    if (__red_src.len == 0) gorget_panic(\"reduce() called on empty array\"); \
                    {elem_type} __red_acc = GORGET_ARRAY_AT({elem_type}, __red_src, 0); \
                    for (size_t __red_i = 1; __red_i < __red_src.len; __red_i++) {{ \
                        {elem_type} __red_elem = GORGET_ARRAY_AT({elem_type}, __red_src, __red_i); \
                        __red_acc = {closure_fn}(__red_acc, __red_elem); \
                    }} \
                    __red_acc; }})"
                )
            }
            _ => {
                // Unknown method — fall through to normal dispatch
                format!("/* unknown Vector method {method_name} */ 0")
            }
        }
    }

    /// Generate code for Iterator adapter method calls (collect, filter, map, fold).
    /// Uses the while/next/break pattern from for-loop iterator codegen.
    pub(super) fn gen_iterator_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        type_name: &str,
    ) -> String {
        // Get the element C type from the Iterator[T] impl
        let elem_c_type = self.get_iterator_elem_c_type(receiver);

        // Register Option[elem_c_type] so the typedef is emitted
        let option_mangled = self.register_generic(
            "Option",
            &[elem_c_type.clone()],
            super::GenericInstanceKind::Enum,
        );
        let tag_none = super::c_mangle::mangle_tag(&option_mangled, "None");
        let iterator_trait_args = self.lookup_trait_type_args(type_name, "Iterator");
        let next_fn = super::c_mangle::mangle_trait_method("Iterator", type_name, "next", &iterator_trait_args);

        match method_name {
            "collect" => {
                format!(
                    "({{ __typeof__({recv}) __it_recv = {recv}; \
                    GorgetArray __it_result = gorget_array_new(sizeof({elem_c_type})); \
                    while (1) {{ \
                        {option_mangled} __it_next = {next_fn}(&__it_recv); \
                        if (__it_next.tag == {tag_none}) break; \
                        {elem_c_type} __it_elem = __it_next.data.Some._0; \
                        gorget_array_push(&__it_result, &__it_elem); \
                    }} \
                    __it_result; }})"
                )
            }
            "filter" => {
                self.closure_return_type_hint = Some("bool".to_string());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ __typeof__({recv}) __it_recv = {recv}; \
                    GorgetArray __it_result = gorget_array_new(sizeof({elem_c_type})); \
                    while (1) {{ \
                        {option_mangled} __it_next = {next_fn}(&__it_recv); \
                        if (__it_next.tag == {tag_none}) break; \
                        {elem_c_type} __it_elem = __it_next.data.Some._0; \
                        if ({closure_fn}(__it_elem)) {{ gorget_array_push(&__it_result, &__it_elem); }} \
                    }} \
                    __it_result; }})"
                )
            }
            "map" => {
                let body_c_type = self.infer_closure_body_c_type(&args[0].node.value);
                self.closure_return_type_hint = Some(body_c_type.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ __typeof__({recv}) __it_recv = {recv}; \
                    GorgetArray __it_result = gorget_array_new(sizeof({body_c_type})); \
                    while (1) {{ \
                        {option_mangled} __it_next = {next_fn}(&__it_recv); \
                        if (__it_next.tag == {tag_none}) break; \
                        {elem_c_type} __it_elem = __it_next.data.Some._0; \
                        {body_c_type} __it_out = {closure_fn}(__it_elem); \
                        gorget_array_push(&__it_result, &__it_out); \
                    }} \
                    __it_result; }})"
                )
            }
            "fold" => {
                let init = self.gen_expr(&args[0].node.value);
                let init_c_type = self.infer_c_type_from_expr(&args[0].node.value.node);
                self.closure_return_type_hint = Some(init_c_type.clone());
                let closure_fn = self.gen_expr(&args[1].node.value);
                format!(
                    "({{ __typeof__({recv}) __it_recv = {recv}; \
                    {init_c_type} __it_acc = {init}; \
                    while (1) {{ \
                        {option_mangled} __it_next = {next_fn}(&__it_recv); \
                        if (__it_next.tag == {tag_none}) break; \
                        {elem_c_type} __it_elem = __it_next.data.Some._0; \
                        __it_acc = {closure_fn}(__it_acc, __it_elem); \
                    }} \
                    __it_acc; }})"
                )
            }
            _ => format!("/* unknown Iterator method {method_name} */ 0"),
        }
    }

    /// Generate code for Map/Dict method calls.
    pub(super) fn gen_map_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let ordered = self.is_ordered_map_expr(receiver);
        let mangled = self.infer_map_mangled(receiver);
        let recv_ref = if needs_temp {
            format!("({{ __typeof__({recv}) __recv = {recv}; &__recv; }})")
        } else {
            format!("&{recv}")
        };

        // Helper: generate a for-loop header + body-index for ordered vs unordered maps.
        // For ordered: `for (oi = 0; oi < src.order_len; oi++) { idx = src.order[oi]; if states[idx]!=1 continue; ...`
        // For unordered: `for (idx = 0; idx < src.cap; idx++) { if states[idx]!=1 continue; ...`
        let iter_loop = |prefix: &str, src: &str, ordered: bool| -> (String, String) {
            if ordered {
                let oi = format!("__{prefix}_oi");
                let idx = format!("__{prefix}_i");
                let head = format!(
                    "for (size_t {oi} = 0; {oi} < {src}.order_len; {oi}++) {{ \
                     size_t {idx} = {src}.order[{oi}]; \
                     if ({src}.states[{idx}] != 1) continue; "
                );
                (head, idx)
            } else {
                let idx = format!("__{prefix}_i");
                let head = format!(
                    "for (size_t {idx} = 0; {idx} < {src}.cap; {idx}++) {{ \
                     if ({src}.states[{idx}] != 1) continue; "
                );
                (head, idx)
            }
        };

        match method_name {
            "put" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let val_expr = &args[1].node.value;
                let val = self.gen_expr(val_expr);
                let (key_type, val_type) = self.infer_map_kv_types(receiver);
                let source_will_be_zeroed = self.source_is_safe_for_push(&val_expr.node);
                let clone_code = if !source_will_be_zeroed {
                    self.compute_push_clone_code(&val_type, "__put_val")
                } else {
                    None
                };
                let val_type_id = self.resolve_expr_type_id(receiver).and_then(|tid| {
                    match self.types.get(tid) {
                        crate::semantic::types::ResolvedType::Generic(_, args) if args.len() >= 2 => Some(args[1]),
                        _ => None,
                    }
                });
                let predrop_code = self.compute_predrop_code(&val_type, "(*__old_vp)", val_type_id);
                if predrop_code.is_some() || clone_code.is_some() {
                    let predrop = predrop_code.map(|code| format!(
                        "{{ {val_type}* __old_vp = {mangled}__get_ptr({recv_ref}, __put_key); \
                         if (__old_vp) {{ {code} }} }}"
                    )).unwrap_or_default();
                    let clone = clone_code.unwrap_or_default();
                    format!("({{ {key_type} __put_key = {key}; {val_type} __put_val = {val}; {clone} \
                             {predrop} {mangled}__put({recv_ref}, __put_key, __put_val); }})")
                } else {
                    format!("{mangled}__put({recv_ref}, {key}, {val})")
                }
            }
            "get" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let (_key_type, val_type) = self.infer_map_kv_types(receiver);
                let opt = self.register_generic("Option", &[val_type.clone()], super::GenericInstanceKind::Enum);
                let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                let ctor_none = c_mangle::mangle_variant(&opt, "None");
                let clone_code = self.compute_push_clone_code(&val_type, "__gc_val");
                if let Some(clone) = &clone_code {
                    self.deep_cloned_expr = true;
                    format!(
                        "({{ {val_type}* __gp = {mangled}__get_ptr({recv_ref}, {key}); {opt} __gr; \
                        if (__gp) {{ {val_type} __gc_val = *__gp; {clone} __gr = {ctor_some}(__gc_val); }} \
                        else {{ __gr = {ctor_none}(); }} __gr; }})"
                    )
                } else {
                    format!(
                        "({{ {val_type}* __gp = {mangled}__get_ptr({recv_ref}, {key}); \
                        __gp ? {ctor_some}(*__gp) : {ctor_none}(); }})"
                    )
                }
            }
            "contains" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("{mangled}__contains({recv_ref}, {key})")
            }
            "len" => {
                format!("(int64_t){recv}.count")
            }
            "remove" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("{mangled}__remove({recv_ref}, {key})")
            }
            "clear" => {
                format!("{mangled}__clear({recv_ref})")
            }
            "is_empty" => {
                format!("({recv}.count == 0)")
            }
            "keys" => {
                let (key_type, _val_type) = self.infer_map_kv_types(receiver);
                let (loop_head, idx) = iter_loop("dk", "__dk_src", ordered);
                format!(
                    "({{ {mangled} __dk_src = {recv}; \
                    GorgetArray __dk_result = gorget_array_new(sizeof({key_type})); \
                    {loop_head}\
                        {key_type} __dk_k = __dk_src.keys[{idx}]; \
                        gorget_array_push(&__dk_result, &__dk_k); \
                    }} \
                    __dk_result; }})"
                )
            }
            "values" => {
                let (_key_type, val_type) = self.infer_map_kv_types(receiver);
                let (loop_head, idx) = iter_loop("dv", "__dv_src", ordered);
                format!(
                    "({{ {mangled} __dv_src = {recv}; \
                    GorgetArray __dv_result = gorget_array_new(sizeof({val_type})); \
                    {loop_head}\
                        {val_type} __dv_v = __dv_src.values[{idx}]; \
                        gorget_array_push(&__dv_result, &__dv_v); \
                    }} \
                    __dv_result; }})"
                )
            }
            "items" => {
                let (key_type, val_type) = self.infer_map_kv_types(receiver);
                let tuple_name = self.register_tuple_typedef(&[key_type.clone(), val_type.clone()]);
                let (loop_head, idx) = iter_loop("di", "__di_src", ordered);
                format!(
                    "({{ {mangled} __di_src = {recv}; \
                    GorgetArray __di_result = gorget_array_new(sizeof({tuple_name})); \
                    {loop_head}\
                        {tuple_name} __di_item = ({tuple_name}){{ __di_src.keys[{idx}], __di_src.values[{idx}] }}; \
                        gorget_array_push(&__di_result, &__di_item); \
                    }} \
                    __di_result; }})"
                )
            }
            "update" => {
                let arg = self.gen_expr(&args[0].node.value);
                let (key_type, val_type) = self.infer_map_kv_types(receiver);
                let (loop_head, idx) = iter_loop("du", "__du_src", ordered);
                format!(
                    "({{ {mangled}* __du_dst = {recv_ref}; {mangled} __du_src = {arg}; \
                    {loop_head}\
                        {key_type} __du_k = __du_src.keys[{idx}]; \
                        {val_type} __du_v = __du_src.values[{idx}]; \
                        {mangled}__put(__du_dst, __du_k, __du_v); \
                    }} }})"
                )
            }
            "get_or" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let default = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let (_key_type, val_type) = self.infer_map_kv_types(receiver);
                format!(
                    "({{ {val_type}* __dgo_ptr = {mangled}__get_ptr({recv_ref}, {key}); __dgo_ptr ? *__dgo_ptr : {default}; }})"
                )
            }
            "get_or_put" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let default = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let (key_type, val_type) = self.infer_map_kv_types(receiver);
                format!(
                    "({{ {val_type}* __gop_ptr = {mangled}__get_ptr({recv_ref}, {key}); \
                    if (!__gop_ptr) {{ {key_type} __gop_k = {key}; {val_type} __gop_v = {default}; \
                    {mangled}__put({recv_ref}, __gop_k, __gop_v); \
                    __gop_ptr = {mangled}__get_ptr({recv_ref}, __gop_k); }} \
                    *__gop_ptr; }})"
                )
            }
            "filter" => {
                let (key_type, val_type) = self.infer_map_kv_types(receiver);
                self.closure_return_type_hint = Some("bool".to_string());
                let closure_fn = self.gen_expr(&args[0].node.value);
                let (loop_head, idx) = iter_loop("dfilt", "__dfilt_src", ordered);
                format!(
                    "({{ {mangled} __dfilt_src = {recv}; \
                    {mangled} __dfilt_result = {mangled}__new(); \
                    {loop_head}\
                        {key_type} __dfilt_k = __dfilt_src.keys[{idx}]; \
                        {val_type} __dfilt_v = __dfilt_src.values[{idx}]; \
                        if ({closure_fn}(__dfilt_k, __dfilt_v)) {{ {mangled}__put(&__dfilt_result, __dfilt_k, __dfilt_v); }} \
                    }} \
                    __dfilt_result; }})"
                )
            }
            "fold" => {
                let (key_type, val_type) = self.infer_map_kv_types(receiver);
                let init = self.gen_expr(&args[0].node.value);
                let init_c_type = self.infer_c_type_from_expr(&args[0].node.value.node);
                self.closure_return_type_hint = Some(init_c_type.clone());
                let closure_fn = self.gen_expr(&args[1].node.value);
                let (loop_head, idx) = iter_loop("dfold", "__dfold_src", ordered);
                format!(
                    "({{ {mangled} __dfold_src = {recv}; \
                    {init_c_type} __dfold_acc = {init}; \
                    {loop_head}\
                        {key_type} __dfold_k = __dfold_src.keys[{idx}]; \
                        {val_type} __dfold_v = __dfold_src.values[{idx}]; \
                        __dfold_acc = {closure_fn}(__dfold_acc, __dfold_k, __dfold_v); \
                    }} \
                    __dfold_acc; }})"
                )
            }
            _ => {
                format!("/* unknown Dict method {method_name} */ 0")
            }
        }
    }

    /// Generate code for Set method calls.
    pub(super) fn gen_set_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let recv_ref = if needs_temp {
            format!("({{ __typeof__({recv}) __recv = {recv}; &__recv; }})")
        } else {
            format!("&{recv}")
        };

        match method_name {
            "add" => {
                let elem = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let elem_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                format!("({{ {elem_type} __e = {elem}; gorget_set_add({recv_ref}, &__e); }})")
            }
            "contains" => {
                let elem = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let elem_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                format!("({{ {elem_type} __e = {elem}; gorget_set_contains({recv_ref}, &__e); }})")
            }
            "len" => {
                format!("(int64_t)gorget_set_len({recv_ref})")
            }
            "remove" => {
                let elem = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let elem_type = args.first()
                    .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                    .unwrap_or_else(|| "int64_t".to_string());
                format!("({{ {elem_type} __e = {elem}; gorget_set_remove({recv_ref}, &__e); }})")
            }
            "clear" => {
                format!("gorget_set_clear({recv_ref})")
            }
            "is_empty" => {
                format!("(gorget_set_len({recv_ref}) == 0)")
            }
            "union" => {
                let elem_type = self.infer_vector_elem_type(receiver);
                let arg = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetSet __su_self = {recv}; GorgetSet __su_other = {arg}; \
                    GorgetSet __su_result = gorget_set_new(sizeof({elem_type})); \
                    for (size_t __su_i = 0; __su_i < __su_self.cap; __su_i++) {{ \
                        if (__su_self.states[__su_i] != 1) continue; \
                        {elem_type} __su_elem = *({elem_type}*)((char*)__su_self.keys + __su_i * __su_self.key_size); \
                        gorget_set_add(&__su_result, &__su_elem); \
                    }} \
                    for (size_t __su_j = 0; __su_j < __su_other.cap; __su_j++) {{ \
                        if (__su_other.states[__su_j] != 1) continue; \
                        {elem_type} __su_elem2 = *({elem_type}*)((char*)__su_other.keys + __su_j * __su_other.key_size); \
                        gorget_set_add(&__su_result, &__su_elem2); \
                    }} \
                    __su_result; }})"
                )
            }
            "intersection" => {
                let elem_type = self.infer_vector_elem_type(receiver);
                let arg = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetSet __si_self = {recv}; GorgetSet __si_other = {arg}; \
                    GorgetSet __si_result = gorget_set_new(sizeof({elem_type})); \
                    for (size_t __si_i = 0; __si_i < __si_self.cap; __si_i++) {{ \
                        if (__si_self.states[__si_i] != 1) continue; \
                        {elem_type} __si_elem = *({elem_type}*)((char*)__si_self.keys + __si_i * __si_self.key_size); \
                        if (gorget_set_contains(&__si_other, &__si_elem)) {{ gorget_set_add(&__si_result, &__si_elem); }} \
                    }} \
                    __si_result; }})"
                )
            }
            "difference" => {
                let elem_type = self.infer_vector_elem_type(receiver);
                let arg = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetSet __sd_self = {recv}; GorgetSet __sd_other = {arg}; \
                    GorgetSet __sd_result = gorget_set_new(sizeof({elem_type})); \
                    for (size_t __sd_i = 0; __sd_i < __sd_self.cap; __sd_i++) {{ \
                        if (__sd_self.states[__sd_i] != 1) continue; \
                        {elem_type} __sd_elem = *({elem_type}*)((char*)__sd_self.keys + __sd_i * __sd_self.key_size); \
                        if (!gorget_set_contains(&__sd_other, &__sd_elem)) {{ gorget_set_add(&__sd_result, &__sd_elem); }} \
                    }} \
                    __sd_result; }})"
                )
            }
            "is_subset" => {
                let elem_type = self.infer_vector_elem_type(receiver);
                let arg = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetSet __ssub_self = {recv}; GorgetSet __ssub_other = {arg}; \
                    bool __ssub_result = true; \
                    for (size_t __ssub_i = 0; __ssub_i < __ssub_self.cap; __ssub_i++) {{ \
                        if (__ssub_self.states[__ssub_i] != 1) continue; \
                        {elem_type} __ssub_elem = *({elem_type}*)((char*)__ssub_self.keys + __ssub_i * __ssub_self.key_size); \
                        if (!gorget_set_contains(&__ssub_other, &__ssub_elem)) {{ __ssub_result = false; break; }} \
                    }} \
                    __ssub_result; }})"
                )
            }
            "is_superset" => {
                let elem_type = self.infer_vector_elem_type(receiver);
                let arg = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetSet __ssup_self = {recv}; GorgetSet __ssup_other = {arg}; \
                    bool __ssup_result = true; \
                    for (size_t __ssup_i = 0; __ssup_i < __ssup_other.cap; __ssup_i++) {{ \
                        if (__ssup_other.states[__ssup_i] != 1) continue; \
                        {elem_type} __ssup_elem = *({elem_type}*)((char*)__ssup_other.keys + __ssup_i * __ssup_other.key_size); \
                        if (!gorget_set_contains(&__ssup_self, &__ssup_elem)) {{ __ssup_result = false; break; }} \
                    }} \
                    __ssup_result; }})"
                )
            }
            "filter" => {
                // Set elem type is the first generic type arg, same as Vector
                let elem_type = self.infer_vector_elem_type(receiver);
                self.closure_return_type_hint = Some("bool".to_string());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ GorgetSet __sfilt_src = {recv}; \
                    GorgetSet __sfilt_result = gorget_set_new(sizeof({elem_type})); \
                    for (size_t __sfilt_i = 0; __sfilt_i < __sfilt_src.cap; __sfilt_i++) {{ \
                        if (__sfilt_src.states[__sfilt_i] != 1) continue; \
                        {elem_type} __sfilt_elem = *({elem_type}*)((char*)__sfilt_src.keys + __sfilt_i * __sfilt_src.key_size); \
                        if ({closure_fn}(__sfilt_elem)) {{ gorget_set_add(&__sfilt_result, &__sfilt_elem); }} \
                    }} \
                    __sfilt_result; }})"
                )
            }
            "fold" => {
                let elem_type = self.infer_vector_elem_type(receiver);
                let init = self.gen_expr(&args[0].node.value);
                let init_c_type = self.infer_c_type_from_expr(&args[0].node.value.node);
                self.closure_return_type_hint = Some(init_c_type.clone());
                let closure_fn = self.gen_expr(&args[1].node.value);
                format!(
                    "({{ GorgetSet __sfold_src = {recv}; \
                    {init_c_type} __sfold_acc = {init}; \
                    for (size_t __sfold_i = 0; __sfold_i < __sfold_src.cap; __sfold_i++) {{ \
                        if (__sfold_src.states[__sfold_i] != 1) continue; \
                        {elem_type} __sfold_elem = *({elem_type}*)((char*)__sfold_src.keys + __sfold_i * __sfold_src.key_size); \
                        __sfold_acc = {closure_fn}(__sfold_acc, __sfold_elem); \
                    }} \
                    __sfold_acc; }})"
                )
            }
            _ => {
                format!("/* unknown Set method {method_name} */ 0")
            }
        }
    }

    /// Generate code for String method calls.
    pub(super) fn gen_string_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        needs_temp: bool,
        is_owned: bool,
    ) -> Option<String> {
        // String-specific methods (only on owned String / GorgetString)
        if is_owned {
            match method_name {
                "push" | "push_line" => {
                    let arg = args.first()
                        .map(|a| self.gen_expr(&a.node.value))
                        .unwrap_or_else(|| "\"\"".to_string());
                    let arg_type = args.first()
                        .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                        .unwrap_or_default();
                    let func = match (method_name, arg_type.as_str()) {
                        ("push", "char") => "gorget_string_push_char",
                        ("push", "int64_t" | "int8_t" | "int16_t" | "int32_t"
                              | "uint64_t" | "uint8_t" | "uint16_t" | "uint32_t") => "gorget_string_push_int",
                        ("push", "double" | "float") => "gorget_string_push_float",
                        ("push", "bool") => "gorget_string_push_bool",
                        ("push_line", "char") => "gorget_string_push_line_char",
                        ("push_line", "int64_t" | "int8_t" | "int16_t" | "int32_t"
                                    | "uint64_t" | "uint8_t" | "uint16_t" | "uint32_t") => "gorget_string_push_line_int",
                        ("push_line", "double" | "float") => "gorget_string_push_line_float",
                        ("push_line", "bool") => "gorget_string_push_line_bool",
                        (_, "GorgetString") => {
                            let rhs = self.extract_cstr_data(&arg, "GorgetString");
                            let func = if method_name == "push_line" { "gorget_string_push_line" } else { "gorget_string_append" };
                            return Some(format!("{func}(&{recv}, {rhs})"));
                        }
                        (_, "Str") => {
                            if method_name == "push_line" {
                                return Some(format!("({{ gorget_string_append_str(&{recv}, {arg}); gorget_string_push_char(&{recv}, '\\n'); }})"));
                            }
                            return Some(format!("gorget_string_append_str(&{recv}, {arg})"));
                        }
                        ("push_line", _) => "gorget_string_push_line",
                        _ => "gorget_string_append",
                    };
                    return Some(format!("{func}(&{recv}, {arg})"));
                }
                "push_char" => {
                    let arg = args.first()
                        .map(|a| self.gen_expr(&a.node.value))
                        .unwrap_or_else(|| "'\\0'".to_string());
                    return Some(format!("gorget_string_push_char(&{recv}, {arg})"));
                }
                "str" => {
                    // Explicit coercion to str: GorgetString → Str
                    return Some(self.coerce_string_to_str(recv));
                }
                "clear" => {
                    return Some(format!("({{ {recv}.len = 0; if ({recv}.data) {recv}.data[0] = '\\0'; }})"));
                }
                "capacity" => {
                    return Some(format!("(int64_t){recv}.cap"));
                }
                _ => {} // Fall through to shared str methods
            }
        }

        // Helper: produce a Str C expression from receiver.
        // If is_owned (GorgetString), coerce to Str.
        // If needs_temp, wrap in statement expression.
        let str_recv = if is_owned {
            format!("(Str){{ .data = {recv}.data, .len = {recv}.len }}")
        } else if needs_temp {
            format!("({{ Str __s = {recv}; __s; }})")
        } else {
            recv.to_string()
        };

        match method_name {
            "len" => {
                Some(format!("gorget_str_codepoint_count({str_recv})"))
            }
            "byte_len" => {
                if is_owned {
                    Some(format!("(int64_t){recv}.len"))
                } else if needs_temp {
                    Some(format!("({{ Str __s = {recv}; (int64_t)__s.len; }})"))
                } else {
                    Some(format!("(int64_t){recv}.len"))
                }
            }
            "contains" => {
                let str_arg = self.gen_str_arg_from_call_arg(args.first());
                Some(format!("gorget_str_contains({str_recv}, {str_arg})"))
            }
            "starts_with" => {
                let str_arg = self.gen_str_arg_from_call_arg(args.first());
                Some(format!("gorget_str_starts_with({str_recv}, {str_arg})"))
            }
            "ends_with" => {
                let str_arg = self.gen_str_arg_from_call_arg(args.first());
                Some(format!("gorget_str_ends_with({str_recv}, {str_arg})"))
            }
            "is_empty" => {
                Some(format!("({recv}.len == 0)"))
            }
            // Group A: View returns — return Str (no allocation)
            "trim" => {
                Some(format!("gorget_str_trim({str_recv})"))
            }
            "strip" => {
                if let Some(arg) = args.first() {
                    let str_arg = self.gen_str_from_expr(&arg.node.value);
                    Some(format!("gorget_str_strip({str_recv}, {str_arg})"))
                } else {
                    Some(format!("gorget_str_trim({str_recv})"))
                }
            }
            "lstrip" => {
                if let Some(arg) = args.first() {
                    let str_arg = self.gen_str_from_expr(&arg.node.value);
                    Some(format!("gorget_str_lstrip({str_recv}, {str_arg})"))
                } else {
                    Some(format!("gorget_str_lstrip_ws({str_recv})"))
                }
            }
            "rstrip" => {
                if let Some(arg) = args.first() {
                    let str_arg = self.gen_str_from_expr(&arg.node.value);
                    Some(format!("gorget_str_rstrip({str_recv}, {str_arg})"))
                } else {
                    Some(format!("gorget_str_rstrip_ws({str_recv})"))
                }
            }
            "removeprefix" => {
                let str_arg = self.gen_str_arg_from_call_arg(args.first());
                Some(format!("gorget_str_removeprefix({str_recv}, {str_arg})"))
            }
            "removesuffix" => {
                let str_arg = self.gen_str_arg_from_call_arg(args.first());
                Some(format!("gorget_str_removesuffix({str_recv}, {str_arg})"))
            }
            // Group B: Allocating returns — return GorgetString directly
            "to_upper" => {
                Some(format!("gorget_str_to_upper({str_recv})"))
            }
            "to_lower" => {
                Some(format!("gorget_str_to_lower({str_recv})"))
            }
            "replace" => {
                let str_old = self.gen_str_arg_from_call_arg(args.first());
                let str_new = self.gen_str_arg_from_call_arg(args.get(1));
                Some(format!("gorget_str_replace({str_recv}, {str_old}, {str_new})"))
            }
            "repeat" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                Some(format!("gorget_str_repeat({str_recv}, {arg})"))
            }
            "join" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "gorget_array_new(sizeof(Str))".to_string());
                Some(format!("gorget_str_join({str_recv}, {arg})"))
            }
            "pad_left" => {
                let width = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let fill = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "' '".to_string());
                Some(format!("gorget_str_pad_left({str_recv}, {width}, {fill})"))
            }
            "pad_right" => {
                let width = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let fill = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "' '".to_string());
                Some(format!("gorget_str_pad_right({str_recv}, {width}, {fill})"))
            }
            "substring" => {
                let start_arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let end_arg = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                Some(format!("gorget_string_from_str(gorget_str_byte_slice({str_recv}, {start_arg}, {end_arg}))"))
            }
            // Group C: split, index_of, count — non-string returns
            "split" => {
                let str_arg = self.gen_str_arg_from_call_arg(args.first());
                Some(format!("gorget_str_split({str_recv}, {str_arg})"))
            }
            "index_of" => {
                let str_arg = self.gen_str_arg_from_call_arg(args.first());
                let opt = self.register_generic("Option", &["int64_t".to_string()], super::GenericInstanceKind::Enum);
                let ctor_some = c_mangle::mangle_variant(&opt, "Some");
                let ctor_none = c_mangle::mangle_variant(&opt, "None");
                Some(format!("({{ int64_t __si = gorget_str_index_of({str_recv}, {str_arg}); \
                __si >= 0 ? {ctor_some}(__si) : {ctor_none}(); }})"))
            }
            "count" => {
                let str_arg = self.gen_str_arg_from_call_arg(args.first());
                Some(format!("gorget_str_count({str_recv}, {str_arg})"))
            }
            // Group D: iterator methods — return GorgetArray
            "bytes" => {
                Some(format!("gorget_str_bytes({str_recv})"))
            }
            "codepoints" => {
                Some(format!("gorget_str_codepoints({str_recv})"))
            }
            "chars" => {
                Some(format!("gorget_str_chars({str_recv})"))
            }
            // Unchanged methods
            "hash" => {
                if needs_temp {
                    Some(format!("({{ Str __s = {recv}; (int64_t)__gorget_hash_str_len(__s.data, __s.len); }})"))
                } else {
                    Some(format!("(int64_t)__gorget_hash_str_len({recv}.data, {recv}.len)"))
                }
            }
            "char_at" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                if is_owned {
                    Some(format!("gorget_str_byte_at((Str){{ .data = {recv}.data, .len = {recv}.len }}, {arg})"))
                } else if needs_temp {
                    Some(format!("({{ Str __s = {recv}; gorget_str_byte_at(__s, {arg}); }})"))
                } else {
                    Some(format!("gorget_str_byte_at({recv}, {arg})"))
                }
            }
            "byte_slice" => {
                let start_arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let end_arg = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                if is_owned {
                    Some(format!("gorget_str_byte_slice((Str){{ .data = {recv}.data, .len = {recv}.len }}, {start_arg}, {end_arg})"))
                } else if needs_temp {
                    Some(format!("({{ Str __s = {recv}; gorget_str_byte_slice(__s, {start_arg}, {end_arg}); }})"))
                } else {
                    Some(format!("gorget_str_byte_slice({recv}, {start_arg}, {end_arg})"))
                }
            }
            _ => None, // Not a known string method — fall through
        }
    }

    /// Coerce a call argument expression to a Str C expression.
    fn gen_str_arg_from_call_arg(&mut self, arg: Option<&Spanned<crate::parser::ast::CallArg>>) -> String {
        if let Some(a) = arg {
            self.gen_str_from_expr(&a.node.value)
        } else {
            "gorget_str_empty()".to_string()
        }
    }

    /// Coerce an arbitrary expression to a Str C expression.
    fn gen_str_from_expr(&mut self, expr: &Spanned<Expr>) -> String {
        let c_expr = self.gen_expr(expr);
        let c_type = self.infer_c_type_from_expr(&expr.node);
        self.coerce_to_str(&c_expr, &c_type)
    }


    /// Generate code for Box built-in methods: get(), set().
    pub(super) fn gen_box_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
    ) -> String {
        match method_name {
            "get" => format!("(*{recv})"),
            "set" => {
                if let Some(arg) = args.first() {
                    let val = self.gen_expr(&arg.node.value);
                    format!("({{ *{recv} = {val}; }})")
                } else {
                    format!("/* Box.set() missing arg */")
                }
            }
            "new" => {
                // Box.new(value) as a method call (when Box is the receiver)
                if let Some(arg) = args.first() {
                    let inner = self.gen_expr(&arg.node.value);
                    let inner_type = self.box_inner_c_type(&arg.node.value);
                    format!(
                        "({{ {inner_type}* __box_tmp = ({inner_type}*)GORGET_ALLOC(sizeof({inner_type})); *__box_tmp = {inner}; __box_tmp; }})"
                    )
                } else {
                    format!("/* Box.new() missing arg */")
                }
            }
            _ => format!("/* unknown Box method: {method_name} */"),
        }
    }

    /// Generate code for File static constructor calls (open/create only).
    /// Instance methods (read_all, write, close) are handled via extern dispatch.
    pub(super) fn gen_file_method(
        &mut self,
        _recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        _needs_temp: bool,
    ) -> String {
        match method_name {
            "open" => {
                if let Some(arg) = args.first() {
                    let path_arg = self.gen_str_arg(&arg.node.value);
                    format!("gorget_file_open({path_arg}, \"r\")")
                } else {
                    format!("/* File.open() missing arg */")
                }
            }
            "create" => {
                if let Some(arg) = args.first() {
                    let path_arg = self.gen_str_arg(&arg.node.value);
                    format!("gorget_file_open({path_arg}, \"w\")")
                } else {
                    format!("/* File.create() missing arg */")
                }
            }
            _ => format!("/* unknown File static method: {method_name} */"),
        }
    }

    /// Infer the inner C type for a Box allocation.
    /// Uses `decl_type_hint` to extract T from `Box[T]`, falling back to the argument's inferred type.
    pub(super) fn box_inner_c_type(&mut self, arg_expr: &Spanned<Expr>) -> String {
        if let Some(Type::Named { name, generic_args }) = self.decl_type_hint.as_ref() {
            if name.node == "Box" && generic_args.len() == 1 {
                return c_types::ast_type_to_c(&generic_args[0].node, self.scopes);
            }
        }
        // Check if the inner expression is a variant constructor → use parent enum type
        if let Some(enum_c_type) = self.resolve_variant_parent_enum_c_type(arg_expr) {
            return enum_c_type;
        }
        // Use resolve_expr_type_id for reliable type resolution (uses resolution map)
        if let Some(type_id) = self.resolve_expr_type_id(arg_expr) {
            return self.type_id_to_c_substituted(type_id);
        }
        // Fallback: infer from the argument expression
        self.infer_c_type_from_expr(&arg_expr.node)
    }

    /// Resolve a variant constructor call to its parent enum C type name.
    /// Returns `Some("Expr")` for `Num(1)` if `Num` is a variant of `Expr`.
    pub(super) fn resolve_variant_parent_enum_c_type(&self, expr: &Spanned<Expr>) -> Option<String> {
        let callee = match &expr.node {
            Expr::Call { callee, .. } => callee,
            _ => return None,
        };
        if let Expr::Identifier(name) = &callee.node {
            let def_id = self.resolution_map
                .get(&callee.span.start)
                .filter(|did| self.scopes.get_def(**did).name == *name)
                .copied()
                .or_else(|| self.scoped_lookup(name))?;
            if self.scopes.get_def(def_id).kind == crate::semantic::scope::DefKind::Variant {
                for (enum_def_id, info) in self.enum_variants {
                    if info.variants.iter().any(|(_, vid)| *vid == def_id) {
                        return Some(self.scopes.get_def(*enum_def_id).name.clone());
                    }
                }
            }
        }
        None
    }

    /// Resolve a unit variant constructor using the decl_type_hint.
    /// Returns the monomorphized enum name if the hint matches the expected enum.
    pub(super) fn resolve_unit_variant_from_type_hint(&mut self, enum_name: &str, _variant_name: &str) -> Option<String> {
        if let Some(Type::Named { name, generic_args }) = self.decl_type_hint.as_ref() {
            if name.node == enum_name && !generic_args.is_empty() {
                let c_type_args: Vec<String> = generic_args.iter()
                    .map(|a| c_types::ast_type_to_c(&a.node, self.scopes))
                    .collect();
                let mangled = self.register_generic(enum_name, &c_type_args, super::GenericInstanceKind::Enum);
                return Some(mangled);
            }
        }
        None
    }

    /// Infer the monomorphized C type name for a generic enum receiver (e.g. `Option__int64_t`).
    pub(super) fn infer_generic_enum_mangled_name(&mut self, receiver: &Spanned<Expr>) -> String {
        if let Some(tid) = self.resolve_expr_type_id(receiver) {
            if let crate::semantic::types::ResolvedType::Generic(def_id, args) = self.types.get(tid) {
                let base = self.scopes.get_def(*def_id).name.clone();
                let c_args: Vec<String> = args.iter()
                    .map(|&a| self.type_id_to_c_substituted(a))
                    .collect();
                return c_mangle::mangle_generic(&base, &c_args);
            }
        }
        // When receiver is a collection .get() call, construct Option[T] mangled name
        // directly from the collection's element/value type.
        if let Expr::MethodCall { receiver: inner_recv, method, .. } = &receiver.node {
            let m = method.node.as_str();
            if matches!(m, "get" | "pop" | "remove" | "index_of") {
                let recv_type = self.infer_receiver_type(inner_recv);
                if matches!(m, "index_of") {
                    // index_of always returns Option[int] regardless of collection type
                    if matches!(recv_type.as_str(), "Vector" | "List" | "Array")
                        || recv_type == "const char*"
                        || recv_type == "GorgetString"
                        || recv_type == "str"
                        || recv_type == "String"
                    {
                        return c_mangle::mangle_generic("Option", &["int64_t".to_string()]);
                    }
                } else if matches!(recv_type.as_str(), "Vector" | "List" | "Array") {
                    let elem = self.infer_vector_elem_type(inner_recv);
                    return c_mangle::mangle_generic("Option", &[elem]);
                } else if m == "get" && matches!(recv_type.as_str(), "Dict" | "HashMap") {
                    let (_, val) = self.infer_map_kv_types(inner_recv);
                    return c_mangle::mangle_generic("Option", &[val]);
                }
            }
        }
        // Fallback: use the receiver C type directly
        self.infer_receiver_c_type(receiver).unwrap_or_else(|| "Option__int64_t".to_string())
    }

    /// Extract the C return type from a closure's body expression.
    pub(super) fn infer_closure_body_c_type(&mut self, arg: &Spanned<Expr>) -> String {
        if let Expr::Closure { body, .. } = &arg.node {
            return self.infer_c_type_from_expr(&body.node);
        }
        "int64_t".to_string()
    }

    /// Extract the C type args from a generic receiver expression.
    pub(super) fn infer_generic_type_args(&mut self, receiver: &Spanned<Expr>) -> Vec<String> {
        if let Some(tid) = self.resolve_expr_type_id(receiver) {
            if let crate::semantic::types::ResolvedType::Generic(_, args) = self.types.get(tid) {
                return args.iter()
                    .map(|&a| self.type_id_to_c_substituted(a))
                    .collect();
            }
        }
        vec![]
    }

    /// Generate code for Option method calls.
    pub(super) fn gen_option_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let mangled = self.infer_generic_enum_mangled_name(receiver);
        let tag_some = c_mangle::mangle_tag(&mangled, "Some");
        let tag_none = c_mangle::mangle_tag(&mangled, "None");

        match method_name {
            "unwrap" => {
                format!("({{ {mangled} __opt = {recv}; if (__opt.tag == {tag_none}) {{ fprintf(stderr, \"unwrap called on None\\n\"); exit(1); }} __opt.data.Some._0; }})")
            }
            "expect" => {
                let msg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"expect failed\"".to_string());
                format!("({{ {mangled} __opt = {recv}; if (__opt.tag == {tag_none}) {{ fprintf(stderr, \"%s\\n\", {msg}); exit(1); }} __opt.data.Some._0; }})")
            }
            "unwrap_or" => {
                let default = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {mangled} __opt = {recv}; (__opt.tag == {tag_some}) ? __opt.data.Some._0 : {default}; }})")
            }
            "is_some" => {
                if needs_temp {
                    format!("({{ {mangled} __opt = {recv}; __opt.tag == {tag_some}; }})")
                } else {
                    format!("({recv}.tag == {tag_some})")
                }
            }
            "is_none" => {
                if needs_temp {
                    format!("({{ {mangled} __opt = {recv}; __opt.tag == {tag_none}; }})")
                } else {
                    format!("({recv}.tag == {tag_none})")
                }
            }
            "map" => {
                // Determine closure body return C type for output Option type
                let body_c_type = self.infer_closure_body_c_type(&args[0].node.value);
                self.closure_return_type_hint = Some(body_c_type.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                // Register output Option type (may be same as input for same-type map)
                let output_mangled = self.register_generic("Option", &[body_c_type], super::GenericInstanceKind::Enum);
                let ctor_some = c_mangle::mangle_variant(&output_mangled, "Some");
                let ctor_none = c_mangle::mangle_variant(&output_mangled, "None");
                format!(
                    "({{ {mangled} __opt = {recv}; {output_mangled} __result; \
                    if (__opt.tag == {tag_some}) {{ __result = {ctor_some}({closure_fn}(__opt.data.Some._0)); }} \
                    else {{ __result = {ctor_none}(); }} \
                    __result; }})"
                )
            }
            "and_then" => {
                self.closure_return_type_hint = Some(mangled.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                let ctor_none = c_mangle::mangle_variant(&mangled, "None");
                format!(
                    "({{ {mangled} __opt = {recv}; {mangled} __result; \
                    if (__opt.tag == {tag_some}) {{ __result = {closure_fn}(__opt.data.Some._0); }} \
                    else {{ __result = {ctor_none}(); }} \
                    __result; }})"
                )
            }
            "or_else" => {
                self.closure_return_type_hint = Some(mangled.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ {mangled} __opt = {recv}; (__opt.tag == {tag_some}) ? __opt : {closure_fn}(); }})"
                )
            }
            "unwrap_or_else" => {
                let type_args = self.infer_generic_type_args(receiver);
                let elem_c_type = type_args.first().cloned().unwrap_or_else(|| "int64_t".to_string());
                self.closure_return_type_hint = Some(elem_c_type);
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ {mangled} __opt = {recv}; (__opt.tag == {tag_some}) ? __opt.data.Some._0 : {closure_fn}(); }})"
                )
            }
            "filter" => {
                self.closure_return_type_hint = Some("bool".to_string());
                let closure_fn = self.gen_expr(&args[0].node.value);
                let ctor_none = c_mangle::mangle_variant(&mangled, "None");
                format!(
                    "({{ {mangled} __opt = {recv}; (__opt.tag == {tag_some} && {closure_fn}(__opt.data.Some._0)) ? __opt : {ctor_none}(); }})"
                )
            }
            "or" => {
                let alt = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ {mangled} __opt = {recv}; (__opt.tag == {tag_some}) ? __opt : {alt}; }})"
                )
            }
            "flatten" => {
                let type_args = self.infer_generic_type_args(receiver);
                let inner_mangled = type_args.first().cloned()
                    .unwrap_or_else(|| "Option__int64_t".to_string());
                let inner_none = c_mangle::mangle_variant(&inner_mangled, "None");
                format!(
                    "({{ {mangled} __opt = {recv}; (__opt.tag == {tag_some}) ? __opt.data.Some._0 : {inner_none}(); }})"
                )
            }
            _ => format!("/* unknown Option method {method_name} */ 0"),
        }
    }

    /// Generate code for Result method calls.
    pub(super) fn gen_result_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        receiver: &Spanned<Expr>,
        needs_temp: bool,
    ) -> String {
        let mangled = self.infer_generic_enum_mangled_name(receiver);
        let tag_ok = c_mangle::mangle_tag(&mangled, "Ok");
        let tag_error = c_mangle::mangle_tag(&mangled, "Error");

        match method_name {
            "unwrap" => {
                format!("({{ {mangled} __res = {recv}; if (__res.tag == {tag_error}) {{ fprintf(stderr, \"unwrap called on Error\\n\"); exit(1); }} __res.data.Ok._0; }})")
            }
            "expect" => {
                let msg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"expect failed\"".to_string());
                format!("({{ {mangled} __res = {recv}; if (__res.tag == {tag_error}) {{ fprintf(stderr, \"%s\\n\", {msg}); exit(1); }} __res.data.Ok._0; }})")
            }
            "unwrap_or" => {
                let default = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {mangled} __res = {recv}; (__res.tag == {tag_ok}) ? __res.data.Ok._0 : {default}; }})")
            }
            "is_ok" => {
                if needs_temp {
                    format!("({{ {mangled} __res = {recv}; __res.tag == {tag_ok}; }})")
                } else {
                    format!("({recv}.tag == {tag_ok})")
                }
            }
            "is_err" => {
                if needs_temp {
                    format!("({{ {mangled} __res = {recv}; __res.tag == {tag_error}; }})")
                } else {
                    format!("({recv}.tag == {tag_error})")
                }
            }
            "map" => {
                let body_c_type = self.infer_closure_body_c_type(&args[0].node.value);
                self.closure_return_type_hint = Some(body_c_type.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                // Result[T,E].map() -> Result[U,E]: need E type for output
                let type_args = self.infer_generic_type_args(receiver);
                let error_c_type = type_args.get(1).cloned().unwrap_or_else(|| "int64_t".to_string());
                let output_mangled = self.register_generic("Result", &[body_c_type, error_c_type.clone()], super::GenericInstanceKind::Enum);
                let ctor_ok = c_mangle::mangle_variant(&output_mangled, "Ok");
                let ctor_error = c_mangle::mangle_variant(&output_mangled, "Error");
                format!(
                    "({{ {mangled} __res = {recv}; {output_mangled} __result; \
                    if (__res.tag == {tag_ok}) {{ __result = {ctor_ok}({closure_fn}(__res.data.Ok._0)); }} \
                    else {{ __result = {ctor_error}(__res.data.Error._0); }} \
                    __result; }})"
                )
            }
            "and_then" => {
                self.closure_return_type_hint = Some(mangled.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                let ctor_error = c_mangle::mangle_variant(&mangled, "Error");
                format!(
                    "({{ {mangled} __res = {recv}; {mangled} __result; \
                    if (__res.tag == {tag_ok}) {{ __result = {closure_fn}(__res.data.Ok._0); }} \
                    else {{ __result = {ctor_error}(__res.data.Error._0); }} \
                    __result; }})"
                )
            }
            "or_else" => {
                self.closure_return_type_hint = Some(mangled.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ {mangled} __res = {recv}; (__res.tag == {tag_ok}) ? __res : {closure_fn}(__res.data.Error._0); }})"
                )
            }
            "unwrap_or_else" => {
                let type_args = self.infer_generic_type_args(receiver);
                let ok_c_type = type_args.first().cloned().unwrap_or_else(|| "int64_t".to_string());
                self.closure_return_type_hint = Some(ok_c_type);
                let closure_fn = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ {mangled} __res = {recv}; (__res.tag == {tag_ok}) ? __res.data.Ok._0 : {closure_fn}(__res.data.Error._0); }})"
                )
            }
            "unwrap_err" => {
                format!("({{ {mangled} __res = {recv}; if (__res.tag == {tag_ok}) {{ fprintf(stderr, \"unwrap_err called on Ok\\n\"); exit(1); }} __res.data.Error._0; }})")
            }
            "map_err" => {
                let body_c_type = self.infer_closure_body_c_type(&args[0].node.value);
                self.closure_return_type_hint = Some(body_c_type.clone());
                let closure_fn = self.gen_expr(&args[0].node.value);
                let type_args = self.infer_generic_type_args(receiver);
                let ok_c_type = type_args.first().cloned().unwrap_or_else(|| "int64_t".to_string());
                let output_mangled = self.register_generic("Result", &[ok_c_type, body_c_type], super::GenericInstanceKind::Enum);
                let ctor_ok = c_mangle::mangle_variant(&output_mangled, "Ok");
                let ctor_error = c_mangle::mangle_variant(&output_mangled, "Error");
                format!(
                    "({{ {mangled} __res = {recv}; {output_mangled} __result; \
                    if (__res.tag == {tag_ok}) {{ __result = {ctor_ok}(__res.data.Ok._0); }} \
                    else {{ __result = {ctor_error}({closure_fn}(__res.data.Error._0)); }} \
                    __result; }})"
                )
            }
            "or" => {
                let alt = self.gen_expr(&args[0].node.value);
                format!(
                    "({{ {mangled} __res = {recv}; (__res.tag == {tag_ok}) ? __res : {alt}; }})"
                )
            }
            _ => format!("/* unknown Result method {method_name} */ 0"),
        }
    }
}
