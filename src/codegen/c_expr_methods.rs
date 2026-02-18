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
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {elem_type} __push_val = {arg}; gorget_array_push({recv_ref}, &__push_val); }})")
            }
            "len" => {
                format!("(int64_t)gorget_array_len({recv_ref})")
            }
            "get" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                if needs_temp {
                    format!("({{ __typeof__({recv}) __recv = {recv}; GORGET_ARRAY_AT({elem_type}, __recv, {idx}); }})")
                } else {
                    format!("GORGET_ARRAY_AT({elem_type}, {recv}, {idx})")
                }
            }
            "pop" => {
                if needs_temp {
                    format!("({{ __typeof__({recv}) __recv = {recv}; {elem_type} __popped = GORGET_ARRAY_AT({elem_type}, __recv, __recv.len - 1); __recv.len--; __popped; }})")
                } else {
                    format!("({{ {elem_type} __popped = GORGET_ARRAY_AT({elem_type}, {recv}, {recv}.len - 1); {recv}.len--; __popped; }})")
                }
            }
            "set" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let val = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("({{ {elem_type} __set_val = {val}; gorget_array_set({recv_ref}, {idx}, &__set_val); }})")
            }
            "remove" => {
                let idx = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                if needs_temp {
                    format!("({{ __typeof__({recv}) __recv = {recv}; {elem_type} __removed = GORGET_ARRAY_AT({elem_type}, __recv, {idx}); gorget_array_remove(&__recv, {idx}); __removed; }})")
                } else {
                    format!("({{ {elem_type} __removed = GORGET_ARRAY_AT({elem_type}, {recv}, {idx}); gorget_array_remove(&{recv}, {idx}); __removed; }})")
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
                        __sorted_dst.data = malloc(__sorted_src.len * sizeof({elem_type})); \
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
                format!("({{ {elem_type} __needle = {arg}; gorget_array_index_of({recv_ref}, &__needle); }})")
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
                let val = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                format!("{mangled}__put({recv_ref}, {key}, {val})")
            }
            "get" => {
                let key = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let (_key_type, val_type) = self.infer_map_kv_types(receiver);
                format!(
                    "({{ {val_type}* __gp = {mangled}__get_ptr({recv_ref}, {key}); \
                    if (!__gp) gorget_panic(\"key not found in map\"); *__gp; }})"
                )
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
                "push" => {
                    let arg = args.first()
                        .map(|a| self.gen_expr(&a.node.value))
                        .unwrap_or_else(|| "\"\"".to_string());
                    let arg_type = args.first()
                        .map(|a| self.infer_c_type_from_expr(&a.node.value.node))
                        .unwrap_or_default();
                    let rhs = if arg_type == "GorgetString" {
                        self.coerce_string_to_str(&arg)
                    } else {
                        arg
                    };
                    // recv is already .data, but we need the original var for &
                    // Since recv = "var.data", we need the base: strip ".data"
                    let base = recv.strip_suffix(".data").unwrap_or(recv);
                    return Some(format!("gorget_string_append(&{base}, {rhs})"));
                }
                "push_char" => {
                    let arg = args.first()
                        .map(|a| self.gen_expr(&a.node.value))
                        .unwrap_or_else(|| "'\\0'".to_string());
                    let base = recv.strip_suffix(".data").unwrap_or(recv);
                    return Some(format!("gorget_string_push_char(&{base}, {arg})"));
                }
                "str" => {
                    // Explicit coercion to str: .data
                    return Some(recv.to_string());
                }
                "clear" => {
                    let base = recv.strip_suffix(".data").unwrap_or(recv);
                    return Some(format!("({{ {base}.len = 0; if ({base}.data) {base}.data[0] = '\\0'; }})"));
                }
                "capacity" => {
                    let base = recv.strip_suffix(".data").unwrap_or(recv);
                    return Some(format!("(int64_t){base}.cap"));
                }
                _ => {} // Fall through to shared str methods
            }
        }
        match method_name {
            "len" => {
                if needs_temp {
                    Some(format!("({{ const char* __s = {recv}; (int64_t)strlen(__s); }})"))
                } else {
                    Some(format!("(int64_t)strlen({recv})"))
                }
            }
            "contains" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("(strstr({recv}, {arg}) != NULL)"))
            }
            "starts_with" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_starts_with({recv}, {arg})"))
            }
            "ends_with" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_ends_with({recv}, {arg})"))
            }
            "is_empty" => {
                Some(format!("(strlen({recv}) == 0)"))
            }
            "trim" => {
                Some(format!("gorget_string_new(gorget_string_trim({recv}))"))
            }
            "strip" => {
                if let Some(arg) = args.first() {
                    let arg = self.gen_expr(&arg.node.value);
                    Some(format!("gorget_string_new(gorget_string_strip({recv}, {arg}))"))
                } else {
                    Some(format!("gorget_string_new(gorget_string_trim({recv}))"))
                }
            }
            "lstrip" => {
                if let Some(arg) = args.first() {
                    let arg = self.gen_expr(&arg.node.value);
                    Some(format!("gorget_string_new(gorget_string_lstrip({recv}, {arg}))"))
                } else {
                    Some(format!("gorget_string_new(gorget_string_lstrip_ws({recv}))"))
                }
            }
            "rstrip" => {
                if let Some(arg) = args.first() {
                    let arg = self.gen_expr(&arg.node.value);
                    Some(format!("gorget_string_new(gorget_string_rstrip({recv}, {arg}))"))
                } else {
                    Some(format!("gorget_string_new(gorget_string_rstrip_ws({recv}))"))
                }
            }
            "to_upper" => {
                Some(format!("gorget_string_new(gorget_string_to_upper({recv}))"))
            }
            "to_lower" => {
                Some(format!("gorget_string_new(gorget_string_to_lower({recv}))"))
            }
            "replace" => {
                let old_arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                let new_arg = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_new(gorget_string_replace({recv}, {old_arg}, {new_arg}))"))
            }
            "split" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_split({recv}, {arg})"))
            }
            "hash" => {
                if needs_temp {
                    Some(format!("({{ const char* __s = {recv}; (int64_t)__gorget_hash_str(__s); }})"))
                } else {
                    Some(format!("(int64_t)__gorget_hash_str({recv})"))
                }
            }
            "substring" => {
                let start_arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let end_arg = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                Some(format!("gorget_string_new(gorget_string_slice({recv}, {start_arg}, {end_arg}))"))
            }
            "char_at" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                Some(format!("gorget_string_at({recv}, {arg})"))
            }
            "index_of" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_index_of({recv}, {arg})"))
            }
            "count" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_count({recv}, {arg})"))
            }
            "repeat" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                Some(format!("gorget_string_new(gorget_string_repeat({recv}, {arg}))"))
            }
            "join" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "gorget_array_new(sizeof(const char*))".to_string());
                Some(format!("gorget_string_new(gorget_string_join({recv}, {arg}))"))
            }
            "removeprefix" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_new(gorget_string_removeprefix({recv}, {arg}))"))
            }
            "removesuffix" => {
                let arg = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "\"\"".to_string());
                Some(format!("gorget_string_new(gorget_string_removesuffix({recv}, {arg}))"))
            }
            "pad_left" => {
                let width = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let fill = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "' '".to_string());
                Some(format!("gorget_string_new(gorget_string_pad_left({recv}, {width}, {fill}))"))
            }
            "pad_right" => {
                let width = args.first()
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "0".to_string());
                let fill = args.get(1)
                    .map(|a| self.gen_expr(&a.node.value))
                    .unwrap_or_else(|| "' '".to_string());
                Some(format!("gorget_string_new(gorget_string_pad_right({recv}, {width}, {fill}))"))
            }
            _ => None, // Not a known string method — fall through
        }
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
                        "({{ {inner_type}* __box_tmp = ({inner_type}*)malloc(sizeof({inner_type})); *__box_tmp = {inner}; __box_tmp; }})"
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
                    let path_arg = self.gen_expr(&arg.node.value);
                    format!("gorget_file_open({path_arg}, \"r\")")
                } else {
                    format!("/* File.open() missing arg */")
                }
            }
            "create" => {
                if let Some(arg) = args.first() {
                    let path_arg = self.gen_expr(&arg.node.value);
                    format!("gorget_file_open({path_arg}, \"w\")")
                } else {
                    format!("/* File.create() missing arg */")
                }
            }
            _ => format!("/* unknown File static method: {method_name} */"),
        }
    }

    /// Generate code for a method call on an HTTP Client receiver.
    pub(super) fn gen_http_client_method(
        &mut self,
        recv: &str,
        method_name: &str,
        args: &[Spanned<crate::parser::ast::CallArg>],
        needs_temp: bool,
    ) -> String {
        let recv_ref = if needs_temp {
            format!("({{ __typeof__({recv}) __tmp = {recv}; &__tmp; }})")
        } else {
            format!("&{recv}")
        };
        match method_name {
            // base_url, header, timeout are now handled by extern dispatch
            "get" | "post" | "put" | "delete" | "patch" | "head" => {
                let method = method_name;
                let path = if let Some(a) = args.first() { self.gen_expr(&a.node.value) } else { "\"\"".into() };
                let body = if args.len() > 1 { self.gen_expr(&args[1].node.value) } else { "\"\"".into() };
                let result_type = c_mangle::mangle_generic("Result", &["GorgetHttpResponse".into(), "const char*".into()]);
                let ok_ctor = c_mangle::mangle_variant(&result_type, "Ok");
                let err_ctor = c_mangle::mangle_variant(&result_type, "Error");
                format!(
                    "({{ GorgetHttpResponse __hr = gorget_http_client_request({recv_ref}, \"{method}\", {path}, {body}); \
                    const char* __he = gorget_http_last_error(); \
                    __he ? {err_ctor}(__he) : {ok_ctor}(__hr); }})"
                )
            }
            _ => format!("/* unknown Client method: {method_name} */"),
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
        // Use resolve_expr_type_id for reliable type resolution (uses resolution map)
        if let Some(type_id) = self.resolve_expr_type_id(arg_expr) {
            return self.type_id_to_c_substituted(type_id);
        }
        // Fallback: infer from the argument expression
        self.infer_c_type_from_expr(&arg_expr.node)
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
                    format!("{recv}.tag == {tag_some}")
                }
            }
            "is_none" => {
                if needs_temp {
                    format!("({{ {mangled} __opt = {recv}; __opt.tag == {tag_none}; }})")
                } else {
                    format!("{recv}.tag == {tag_none}")
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
                    format!("{recv}.tag == {tag_ok}")
                }
            }
            "is_err" => {
                if needs_temp {
                    format!("({{ {mangled} __res = {recv}; __res.tag == {tag_error}; }})")
                } else {
                    format!("{recv}.tag == {tag_error}")
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
            _ => format!("/* unknown Result method {method_name} */ 0"),
        }
    }
}
