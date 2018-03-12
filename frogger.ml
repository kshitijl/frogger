open Base
open Scaffold

open Js_of_ocaml

module Direction = struct
  type t =
    | Up 
    | Down
    | Left
    | Right
end

module Frog = struct
  type t =
    { position : Position.t
    ; facing   : Direction.t
    } [@@deriving fields]

  let create = Fields.create
end

module Non_frog_character = struct
  module Kind = struct
    type t =
      | Car
      | Log
  end

  type t =
    { horizontal_speed : int
    ; position         : Position.t
    ; kind             : Kind.t
    ; image            : Image.t
    } [@@deriving fields]

  let create = Fields.create
end

module Game_state = struct
  type t =
    | Playing
    | Won
    | Dead
end

module World = struct
  type t =
    { state : Game_state.t
    ; frog  : Frog.t
    ; nfcs  : Non_frog_character.t list
    } [@@deriving fields]

  let create = Fields.create
end

let create () =
  let max_speed = 3 in
  let frog =
    let position =
      Position.create
        ~x:(Scaffold.Board.num_cols / 2)
        ~y:0
    in
    Frog.create ~position ~facing:Direction.Up
  in
  let nfcs =
    List.mapi
      Scaffold.Board.rows
      ~f:(fun idx row ->
          let make_nfc kind col =
            let horizontal_speed =
              let sign = 2 * (Random.int 1) - 1 in
              sign * (1 + Random.int max_speed)
            in
            let position         = Position.create ~x:col ~y:idx in
            let image =
              if horizontal_speed < 0
              then Image.car1_left
              else Image.car1_right
            in
            Non_frog_character.create ~kind ~horizontal_speed ~position ~image
          in
          let make_nfcs kind =
            [make_nfc kind 0] (* CR klauria: Make a few, leaving gaps between them *)
          in
          match row with
          | Safe_strip -> []
          | Road       -> make_nfcs Non_frog_character.Kind.Car
          | River      -> make_nfcs Non_frog_character.Kind.Log)
    |> List.concat
  in
  World.create ~state:(Game_state.Playing) ~frog ~nfcs
;;

let rec detect_collision (frog_pos : Position.t) nfcs = 
  let is_colliding (nfc : Non_frog_character.t) =
    if Int.(<>) frog_pos.y nfc.position.y
    then false
    else
      let width =
        match nfc.kind with
        | Car -> 1
        | Log -> 1
      in
      (nfc.position.x <= frog_pos.x) && (frog_pos.x < nfc.position.x + width)
  in
  match nfcs with
  | []          -> None
  | nfc :: rest -> if is_colliding nfc then Some nfc else detect_collision frog_pos rest
;;

let pos_is_in_river (pos : Position.t) =
  match List.nth_exn Scaffold.Board.rows pos.y with
  | Safe_strip | Road -> false
  | River             -> true
;;

let should_die frog_pos collision_result =
  let frog_is_in_river = pos_is_in_river frog_pos in
  match (collision_result : Non_frog_character.t option) with
  | Some { kind = Car; _ } -> true
  | Some { kind = Log; _ } -> false
  | None                   -> frog_is_in_river
;;

let should_win (frog_pos : Position.t) =
  Int.(=) frog_pos.y (List.length Scaffold.Board.rows)
;;

let compute_new_game_state frog_pos collision_result =
  if should_die frog_pos collision_result
  then Game_state.Dead
  else if should_win frog_pos
  then Won
  else Playing
;;

let tick (world : World.t) =
  match world.state with
  | Won | Dead -> world
  | Playing -> 
    let new_nfcs =
      List.map world.nfcs ~f:(fun nfc ->
          let new_position =
            Position.create
              ~x:((nfc.position.x + nfc.horizontal_speed) % Scaffold.Board.num_cols)
              ~y:nfc.position.y
          in
          { nfc with position = new_position })
    in
    let collision_result = detect_collision world.frog.position new_nfcs in
    let new_frog = 
      let new_frog_position =
        let dx = 
          match collision_result with
          | Some { kind = Log; horizontal_speed; _ } -> horizontal_speed
          | _                                        -> 0
        in
        Position.create ~x:(world.frog.position.x + dx) ~y:(world.frog.position.y)
      in
      { world.frog with position = new_frog_position }
    in
    let new_game_state = compute_new_game_state new_frog.position collision_result in
    World.create ~state:new_game_state ~frog:new_frog ~nfcs:new_nfcs
;;

let clamp ~min ~max x =
  if x < min then min else if x > max then max else x
;;

let handle_input (world : World.t) key =
  let num_rows = List.length Scaffold.Board.rows in
  let num_cols = Scaffold.Board.num_cols in
  match world.state with
  | Won | Dead -> world
  | Playing    ->
    let new_frog =
      let new_pos, new_dir =
        let old_pos = world.frog.position in
      match key with
      | Key.Arrow_up ->
        { old_pos with y = clamp ~min:0 ~max:num_rows (old_pos.y + 1)}, Direction.Up
      | Key.Arrow_down ->
        { old_pos with y = clamp ~min:0 ~max:num_rows (old_pos.y - 1)}, Direction.Down
      | Key.Arrow_left ->
        { old_pos with x = clamp ~min:0 ~max:num_cols (old_pos.x - 1)}, Direction.Left
      | Key.Arrow_right ->
        { old_pos with x = clamp ~min:0 ~max:num_cols (old_pos.x + 1)}, Direction.Right
      in
      Frog.create ~position:new_pos ~facing:new_dir
    in
    let new_game_state =
      let collision_result = detect_collision new_frog.position world.nfcs in
      compute_new_game_state new_frog.position collision_result
    in
    World.create ~state:new_game_state ~frog:new_frog ~nfcs:world.nfcs
;;

let draw (world : World.t) =
  let print s = Js_of_ocaml.Firebug.console##info s in
  (match world.state with
  | Won  -> print "won"
  | Dead -> print "dead"
  | _    -> ());

  let draw_frog_command =
    let frog_image = 
      match world.state with
      | Dead    -> Scaffold.Image.skull_and_crossbones
      | Won
      | Playing -> (
          match world.frog.facing with
          | Up    -> Scaffold.Image.frog_up
          | Down  -> Scaffold.Image.frog_down
          | Left  -> Scaffold.Image.frog_left
          | Right -> Scaffold.Image.frog_right)
    in
    (frog_image, world.frog.position)
  in
  let draw_nfc (nfc : Non_frog_character.t) = (nfc.image, nfc.position) in
  (List.map world.nfcs ~f:draw_nfc) @ [draw_frog_command]
;;

let handle_event world event =
  match (event : Event.t) with
  | Tick       -> tick world
  | Keypress k -> handle_input world k
;;
