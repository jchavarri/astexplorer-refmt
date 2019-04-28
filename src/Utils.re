/* TODO: Figure out a way to produce JS arrays with different types */
let unsafeFromTuple = t => Js.array(Obj.magic(t));

let handleOption = (f, opt) =>
  switch (opt) {
  | Some(o) =>
    %js
    {val type_ = "Some" |> Js.string; val payload = f(o)}
  | None => [%js {val type_ = "None" |> Js.string}] |> Js.Unsafe.coerce
  };
