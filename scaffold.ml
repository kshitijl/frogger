open Base
open Js_of_ocaml

(* ## - method will get called as soon I deref.
   `##.prop_name := ` to set a property
   ##.prop_name to read (no deref)

   - z-index if there's trouble with over/under bg
   - any time there's an updated world -> redraw
*)

module Html  = Dom_html
let document = Html.window##.document

module Position = struct
  type t =
    { x : int
    ; y : int
    } [@@deriving fields]

  let create = Fields.create
end

module Image = struct
  type t = string 

  let frog_up     = "assets/frog-icon.png"
  let frog_down   = "assets/frog-icon.png"
  let frog_left   = "assets/frog-icon.png"
  let frog_right  = "assets/frog-icon.png"
  let frog_on_log = "assets/camel.png"

  let car1_left   = "assets/frog-icon.png"
  let car1_right  = "assets/frog-icon.png"
  let car2_left   = "assets/frog-icon.png"
  let car2_right  = "assets/frog-icon.png"

  let log         = "assets/frog-icon.png"

  let skull_and_crossbones = "assets/skull.png"
end

module Display_list = struct
  module Display_command = struct
    type nonrec t = Image.t * Position.t
  end

  type t = Display_command.t list
end

module Key = struct
  type t =
    | Arrow_up
    | Arrow_down
    | Arrow_left
    | Arrow_right
end

module Event = struct
  type t =
    | Tick
    | Keypress of Key.t
end

module Board = struct

  (** Every row of the game board is one of these three kinds. *)
  module Row = struct
    type t =
      | Safe_strip
      | Road
      | River
  end

  let num_cols = 15

  (** The first and last rows are guaranteed to be [Safe_strip]s. *)
  let rows =
    let open Row in
    [ Safe_strip
    ; River
    ; River
    ; River
    ; Safe_strip
    ; Road
    ; Road
    ; Road
    ; Safe_strip
    ]
    |> List.rev
  ;;
end

