open Longident;

let rec handleLongident = longident =>
  switch (longident) {
  | Lident(string) =>
    %js
    {val type_ = "Lident" |> Js.string; val li = string |> Js.string}
  | Ldot(li, string) =>
    [%js
      {
        val type_ = "Ldot" |> Js.string;
        val li = handleLongident(li);
        val s = string |> Js.string
      }
    ]
    |> Js.Unsafe.coerce
  | Lapply(li1, li2) =>
    [%js
      {
        val type_ = "Lapply" |> Js.string;
        val li1 = handleLongident(li1);
        val li2 = handleLongident(li2)
      }
    ]
    |> Js.Unsafe.coerce
  };
