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
    ; nfcs   Non_frog_character.t list
    }
end

let create rng =
  let max_speed = 3 in
  let frog =
    let position =
      Position.create
        ~x:(Scaffold.Board.num_cols / 2)
        ~y:0
    in
    Frog.create ~position ~direction:Direction.Up
  in
  let nfcs =
    List.mapi
      Scaffold.Board.rows
      ~f:(fun idx ->
          let make_nfc kind col =
            let horizontal_speed = Rng.random_int ~lower:-max_speed ~upper:max_speed in (* CR klauria: But not zero. Roll again? *)
            let position         = Position.create ~x:col ~y:idx in
            let image =
              if nfc.horizontal_speed < 0
              then nfc.left_image
              else nfc.right_image
            in
            Non_frog_character.create ~kind ~horizontal_speed ~position (* CR klauria: Image *)
          in
          let make_nfcs kind =
            [make_nfc kind 0] (* CR klauria: Make a few, leaving gaps between them *)
          in
          function
          | Safe_strip -> []
          | Road       -> make_nfcs Non_frog_character.Kind.Car
          | River      -> make_nfcs Non_frog_character.Kind.Log)
    |> List.concat
  in
  World.create ~state:(Game_state.Playing) ~frog ~nfcs
;;

let rec detect_collision frog_pos = function
  let is_colliding nfc =
    if Int.(<>) frog_pos.y nfc.position.y
    then false
    else
      let width =
        match nfc.kind with
        | Car -> 1
        | Log -> 3
      in
      (nfc.position.x <= frog_pos.x) && (frog_pos.x < nfc.position.x + width)
  in
  | []          -> None
  | nfc :: rest -> if is_colliding nfc then nfc else detect_collision frog_pos rest
;;

let pos_is_in_river pos =
  match List.nth_exn Scaffold.Board.rows pos.y with
  | Safe_strip | Road -> false
  | River             -> true
;;

let should_die frog_pos collision_result =
  let frog_is_in_river = pos_is_in_river frog_pos in
  match collision_result with
  | Some { kind = Car; _ } -> true
  | Some { kind = Log; _ } -> false
  | None                   -> frog_is_in_river
;;

let should_win frog_pos =
  Int.(=) frog_pos.y (List.length Scaffold.Board.rows)
;;

let compute_new_game_state frog_pos collision_result =
  if should_die new_frog.position collision_result
  then Dead
  else if should_win frog_pos
  then Won
  else Playing
;;

let tick world =
  match world.state with
  | Won | Dead -> world
  | Playing -> 
    let new_nfcs =
      List.map world.nfcs ~f:(fun nfc ->
          let new_position =
            Position.create ~x:(nfc.position.x + nfc.horizontal_speed) ~y:nfc.position.y
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
      { frog with position = new_position }
    in
    let new_game_state = compute_new_game_state new_frog.position collision_result in
    World.create ~state:new_game_state ~frog:new_frog ~nfcs:new_nfcs
;;

let handle_input world key =
  match world.state with
  | Won | Dead -> world
  | Playing    ->
    let new_frog =
      let new_pos, new_dir = 
      match key with
        | UpArrow -> ()
      in
      Frog.create ~position:new_pos ~facing:new_dir
    in
    let new_game_state =
      let collision_result = detect_collision new_frog world.nfcs in
      compute_new_game_state new_frog.position collision_result
    in
    World.create ~state:new_game_state ~frog:new_frog ~nfcs:world.nfcs
;;

let draw world =
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
  let draw_nfc nfc = (nfc.image, nfc.position) in
  draw_frog_command :: (List.iter world.nfcs ~f:draw_nfc)
;;
