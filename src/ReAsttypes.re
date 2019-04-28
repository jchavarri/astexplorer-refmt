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

let handleClosedFlag = f =>
  switch (f) {
  | Closed => "Closed"
  | Open => "Open"
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

let handleStringLoc = ({txt, loc}) => [%js
  {
    val type_ = "Asttypes.loc(string)" |> Js.string;
    val txt = txt |> Js.string;
    val loc = ReLocation.handleLocation(loc)
  }
];

let handleIdLoc = ({txt, loc}) => [%js
  {
    val type_ = "Asttypes.loc(Longident.t)" |> Js.string;
    val txt = txt |> ReLongident.handleLongident;
    val loc = ReLocation.handleLocation(loc)
  }
];
