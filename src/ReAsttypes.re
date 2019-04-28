open Ast_404.Asttypes;

let recFlag = r =>
  switch (r) {
  | Nonrecursive =>
    %js
    {val type_ = "Nonrecursive" |> Js.string}
  | Recursive =>
    %js
    {val type_ = "Recursive" |> Js.string}
  };

let handleArgLabel = argLabel =>
  switch (argLabel) {
  | Nolabel =>
    %js
    {val type_ = "Nonrecursive" |> Js.string}
  | Labelled(str) =>
    [%js {val type_ = "Labelled" |> Js.string; val label = str |> Js.string}]
    |> Js.Unsafe.coerce
  | Optional(str) =>
    [%js {val type_ = "Optional" |> Js.string; val label = str |> Js.string}]
    |> Js.Unsafe.coerce
  };

let stringLoc = ({txt, loc}) => [%js
  {
    val type_ = "Asttypes.loc(string)" |> Js.string;
    val txt = txt |> Js.string;
    val loc = ReLocation.handleLocation(loc)
  }
];
