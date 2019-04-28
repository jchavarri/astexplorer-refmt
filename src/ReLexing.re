open Lexing;

let handlePosition = ({pos_fname, pos_lnum, pos_bol, pos_cnum}) => [%js
  {
    val type_ = "Lexing.position" |> Js.string;
    val pos_fname_ = pos_fname |> Js.string;
    val pos_lnum_ = pos_lnum;
    val pos_bol_ = pos_bol;
    val pos_cnum_ = pos_cnum
  }
];
