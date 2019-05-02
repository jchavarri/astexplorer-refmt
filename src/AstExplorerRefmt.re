let log = (title, data) =>
  Js.Unsafe.fun_call(
    Js.Unsafe.js_expr("console.log"),
    [|title |> Js.string |> Js.Unsafe.inject, data |> Js.Unsafe.inject|],
  );

let locationToJsObj = (loc: Location.t) => {
  let (_file, start_line, start_char) = Location.get_pos_info(loc.loc_start);
  let (_, end_line, end_char) = Location.get_pos_info(loc.loc_end);
  /* The right way of handling ocaml syntax error locations. Do do this at home
       copied over from
       https://github.com/BuckleScript/bucklescript/blob/2ad2310f18567aa13030cdf32adb007d297ee717/jscomp/super_errors/super_location.ml#L73
     */
  let normalizedRange =
    if (start_char === (-1) || end_char === (-1)) {
      None;
          /* happens sometimes. Syntax error for example */
    } else if (start_line == end_line && start_char >= end_char) {
      /* in some errors, starting char and ending char can be the same. But
         since ending char was supposed to be exclusive, here it might end up
         smaller than the starting char if we naively did start_char + 1 to
         just the starting char and forget ending char */
      let same_char = start_char + 1;
      Some(((start_line, same_char), (end_line, same_char)));
    } else {
      /* again: end_char is exclusive, so +1-1=0 */
      Some((
        (start_line, start_char + 1),
        (end_line, end_char),
      ));
    };

  switch (normalizedRange) {
  | None => Js.undefined
  | Some((
      (start_line, start_line_start_char),
      (end_line, end_line_end_char),
    )) =>
    let intToJsFloatToAny = i =>
      i |> float_of_int |> Js.number_of_float |> Js.Unsafe.inject;

    Js.def(
      Js.Unsafe.obj([|
        ("startLine", intToJsFloatToAny(start_line)),
        ("startLineStartChar", intToJsFloatToAny(start_line_start_char)),
        ("endLine", intToJsFloatToAny(end_line)),
        ("endLineEndChar", intToJsFloatToAny(end_line_end_char)),
      |]),
    );
  };
};

let parseWith = (f, code) => {
  /* you can't throw an Error here. jsoo parses the string and turns it
     into something else */
  let throwAnything =
    Js.Unsafe.js_expr(
      "function(a) {throw new SyntaxError(JSON.stringify(a))}",
    );
  try (code |> Lexing.from_string |> f) {
  /* from ocaml and reason */
  | Syntaxerr.Error(err) =>
    let location = Syntaxerr.location_of_error(err);
    let jsLocation = locationToJsObj(location);
    Syntaxerr.report_error(Format.str_formatter, err);
    let errorString = Format.flush_str_formatter();
    let jsError =
      Js.Unsafe.obj([|
        ("message", Js.Unsafe.inject(Js.string(errorString))),
        ("location", Js.Unsafe.inject(jsLocation)),
      |]);

    Js.Unsafe.fun_call(throwAnything, [|Js.Unsafe.inject(jsError)|]);
  /* from reason */
  | Reason_syntax_util.Error(location, Syntax_error(err)) =>
    let jsLocation = locationToJsObj(location);
    let jsError =
      Js.Unsafe.obj([|
        ("message", Js.Unsafe.inject(Js.string(err))),
        ("location", Js.Unsafe.inject(jsLocation)),
      |]);

    Js.Unsafe.fun_call(throwAnything, [|Js.Unsafe.inject(jsError)|]);
  | Reason_lexer.Error(err, loc) =>
    let reportedError =
      Location.error_of_printer(loc, Reason_lexer.report_error, err);

    let jsLocation = locationToJsObj(reportedError.loc);
    let jsError =
      Js.Unsafe.obj([|
        ("message", Js.Unsafe.inject(Js.string(reportedError.msg))),
        ("location", Js.Unsafe.inject(jsLocation)),
      |]);

    Js.Unsafe.fun_call(throwAnything, [|Js.Unsafe.inject(jsError)|]);
  };
};

let parse = (implementation, code) => {
  let (structure, _comments) = code |> parseWith(implementation);
  %js
  {
    val type_ = "structure" |> Js.string;
    val structure = ReParsetree.handleStructure(structure)
  };
};

let parseReason = code => {
  parse(Reason_toolchain.RE.implementation_with_comments, code);
};

let parseOcaml = code => {
  parse(Reason_toolchain.ML.implementation_with_comments, code);
};

Js.export_all(
  [%js
    {
      val parseReason = a => a |> Js.to_string |> parseReason;
      val parseOcaml = a => a |> Js.to_string |> parseOcaml
    }
  ],
);
