open Ast_404.Asttypes;

let handleRecFlag = r =>
  switch (r) {
  | Nonrecursive =>
    %js
    {val type_ = "Nonrecursive" |> Js.string}
  | Recursive =>
    %js
    {val type_ = "Recursive" |> Js.string}
  };

let handleDirectionFlag = f =>
  switch (f) {
  | Upto => "Upto" |> Js.string
  | Downto => "Downto" |> Js.string
  };

let handlePrivateFlag = f =>
  switch (f) {
  | Private => "Private" |> Js.string
  | Public => "Public" |> Js.string
  };

let handleMutableFlag = f =>
  switch (f) {
  | Immutable => "Immutable" |> Js.string
  | Mutable => "Mutable" |> Js.string
  };

let handleOverrideFlag = f =>
  switch (f) {
  | Override => "Override" |> Js.string
  | Fresh => "Fresh" |> Js.string
  };

let handleClosedFlag = f =>
  switch (f) {
  | Closed => "Closed" |> Js.string
  | Open => "Open" |> Js.string
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

let handleVariance = v =>
  switch (v) {
  | Covariant => "Covariant" |> Js.string
  | Contravariant => "Contravariant" |> Js.string
  | Invariant => "Invariant" |> Js.string
  };
