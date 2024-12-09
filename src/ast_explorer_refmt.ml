open Js_of_ocaml
module RE = Reason.Reason_toolchain.RE
module ML = Reason.Reason_toolchain.ML

let syntaxerr_error_to_string err = Printexc.to_string (Syntaxerr.Error err)

let reason_error_to_string err loc =
  Printexc.to_string (Reason.Reason_errors.Reason_error (err, loc))

let location_to_js_obj (loc : Astlib.Location.t) =
  let _file, start_line, start_char = Location.get_pos_info loc.loc_start in
  let _, end_line, end_char = Location.get_pos_info loc.loc_end in
  (* The right way of handling ocaml syntax error locations. Do do this at home
     copied over from
     https://github.com/BuckleScript/bucklescript/blob/2ad2310f18567aa13030cdf32adb007d297ee717/jscomp/super_errors/super_location.ml#L73
  *)
  let normalizedRange =
    if start_char == -1 || end_char == -1 then
      (* happens sometimes. Syntax error for example *)
      None
    else if start_line = end_line && start_char >= end_char then
      (* in some errors, starting char and ending char can be the same. But
         since ending char was supposed to be exclusive, here it might end up
         smaller than the starting char if we naively did start_char + 1 to
         just the starting char and forget ending char *)
      let same_char = start_char + 1 in
      Some ((start_line, same_char), (end_line, same_char))
    else
      (* again: end_char is exclusive, so +1-1=0 *)
      Some ((start_line, start_char + 1), (end_line, end_char))
  in
  match normalizedRange with
  | None -> Js.undefined
  | Some ((start_line, start_line_start_char), (end_line, end_line_end_char)) ->
      let int_to_js i =
        i |> float_of_int |> Js.number_of_float |> Js.Unsafe.inject
      in
      Js.def
        (Js.Unsafe.obj
           [|
             ("startLine", int_to_js start_line);
             ("startLineStartChar", int_to_js start_line_start_char);
             ("endLine", int_to_js end_line);
             ("endLineEndChar", int_to_js end_line_end_char);
           |])

let throw_js_error ~throw_fn ~loc ~message =
  let js_location = location_to_js_obj loc in
  let js_error =
    Js.Unsafe.obj
      [|
        ("message", Js.Unsafe.inject (Js.string message));
        ("location", Js.Unsafe.inject js_location);
      |]
  in
  Js.Unsafe.fun_call throw_fn [| Js.Unsafe.inject js_error |]

let parse_with f code =
  let throw_fn = Js.Unsafe.js_expr "function(a) {throw a}" in
  try code |> Lexing.from_string |> f with
  | Syntaxerr.Error err ->
      let Location.{ loc_start; loc_end; loc_ghost } =
        Syntaxerr.location_of_error err
      in
      let loc : Astlib.Location.t = { loc_start; loc_end; loc_ghost } in
      throw_js_error ~throw_fn ~loc ~message:(syntaxerr_error_to_string err)
  | Reason.Reason_errors.Reason_error (err, loc) ->
      throw_js_error ~throw_fn ~loc ~message:(reason_error_to_string err loc)

let rec repr_to_yojson : Ppxlib.Pp_ast.repr -> Yojson.Basic.t = function
  | Unit -> `Null
  | Int i -> `Int i
  | String s -> `String s
  | Special s -> `String s
  | Bool b -> `Bool b
  | Char c -> `String (String.make 1 c)
  | Float f -> `Float f
  | Int32 i32 -> `Int (Int32.to_int i32)
  | Int64 i64 -> `Int (Int64.to_int i64)
  | Nativeint ni -> `Int (Nativeint.to_int ni)
  | Array l -> `List (List.map repr_to_yojson l)
  | Tuple l -> `List (List.map repr_to_yojson l)
  | List l -> `List (List.map repr_to_yojson l)
  | Record fields ->
      `Assoc (List.map (fun (k, v) -> (k, repr_to_yojson v)) fields)
  | Constr (cname, []) -> `String cname
  | Constr (cname, [ x ]) -> `Assoc [ (cname, repr_to_yojson x) ]
  | Constr (cname, l) -> `Assoc [ (cname, `List (List.map repr_to_yojson l)) ]

let json_printer fmt value =
  Yojson.Basic.pretty_print fmt (repr_to_yojson value)

let ast_string exp =
  let config =
    Ppxlib.Pp_ast.Config.make ~show_attrs:true ~show_locs:true
      ~printer:json_printer ~loc_mode:`Full ()
  in
  Format.asprintf "%a" (Ppxlib.Pp_ast.structure ~config) exp

let parse f code =
  let structure, _ = parse_with f code in
  ast_string structure

let parse_reason code = parse RE.implementation_with_comments code
let parse_ocaml code = parse ML.implementation_with_comments code
let _ = Js.export "parseReason" parse_reason
let _ = Js.export "parseOcaml" parse_ocaml
