open Migrate_parsetree.Ast_404.Location;

let handleLocation = ({loc_start, loc_end, loc_ghost}) => [%js
  {
    val type_ = "Location.t" |> Js.string;
    val loc_start_ = ReLexing.handlePosition(loc_start);
    val loc_end_ = ReLexing.handlePosition(loc_end);
    val loc_ghost_ = loc_ghost |> Js.bool
  }
];
