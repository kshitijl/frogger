open Base

open Js_of_ocaml
module Html  = Dom_html
open Scaffold

let document = Html.window##.document

module Game = struct
  module Config = struct
    type t = {
      num_rows        : int
    ; num_cols        : int
    ; grid_size_in_px : int
    }

    let default =
      { num_rows = List.length Board.rows
      ; num_cols = Board.num_cols
      ; grid_size_in_px = 50
      }
  end

  module Board_dom = struct
    type t = Html.imageElement Array.t Array.t
  end

  let init_dom board_div (config : Config.t) =
    board_div##.style##.cssText    :=
      Js.string (
        Printf.sprintf "
background-image  : url(\"assets/background.png\");
background-size   : %dpx %dpx;
background-repeat : no-repeat;
"
          (config.num_cols * config.grid_size_in_px)
          (config.num_rows * config.grid_size_in_px))
    ;
    let buf = document##createDocumentFragment in
    let grid =
      Array.init config.num_rows ~f:(fun _ ->
          let row = 
            Array.init config.num_cols ~f:(fun _ ->
                let grid_square = Html.createImg document in
                grid_square##.src    := Js.string "";
                grid_square##.width  := config.grid_size_in_px;
                grid_square##.height := config.grid_size_in_px;
                Dom.appendChild buf grid_square;
                grid_square
              )
          in
          Dom.appendChild buf (Html.createBr document);
          row)
    in
    Dom.appendChild board_div buf;
    board_div##.style##.lineHeight := Js.string "0";
    grid
  ;;

  let render num_rows num_cols board_dom display_list =
    let () =
      for i = 0 to num_rows - 1 do
        for j = 0 to num_cols - 1 do
          board_dom.(num_rows - i - 1).(j)##.src := Js.string "assets/selector-square-border.png" 
        done
      done
    in
    List.iter display_list
      ~f:(fun (image, (position : Position.t)) ->
          board_dom.(num_rows - position.y - 1).(position.x)##.src := Js.string image
        )
  ;;

  module Game_impl = struct
    type 'a t =
      { init : 'a
      ; handle_event : 'a -> Event.t -> 'a
      ; draw         : 'a -> Display_list.t
      }    
  end

  let init_event_handlers (config : Config.t) board_dom (game_impl : 'a Game_impl.t) =
    let world = ref game_impl.init in
    let render some_world =
      render config.num_rows config.num_cols board_dom (game_impl.draw some_world)
    in
    render !world;
    let _ =
      Html.window##setInterval (Js.wrap_callback (fun () ->
          world := game_impl.handle_event !world Tick;
          render !world
        ))
        1000.0
    in
    Html.window##.onkeydown := (Dom.handler (fun key_event ->
        let handle_keypress which_key =
          world := game_impl.handle_event !world (Keypress which_key);
          render !world
        in
        let key = Js.Optdef.get key_event##.key (fun () -> assert false) in
        let () = 
          match Js.to_string key with
          | "ArrowUp"    -> handle_keypress Arrow_up
          | "ArrowDown"  -> handle_keypress Arrow_down
          | "ArrowLeft"  -> handle_keypress Arrow_left
          | "ArrowRight" -> handle_keypress Arrow_right
          | _            -> ()
        in
        Js._true))
  ;;

  let run board_div config game_impl =
    let board_dom = init_dom board_div config in
    init_event_handlers config board_dom game_impl
  ;;
end

let onload ~init ~handle_event ~draw _ =
  let board_div =
    Js.Opt.get (document##getElementById (Js.string "board"))
      (fun () -> assert false)
  in
  Game.run board_div Game.Config.default
    { Game.Game_impl.
      init 
    ; handle_event
    ; draw
    };
  Js._false
;;

let main ~init ~handle_event ~draw =
  Html.window##.onload := Html.handler (
      onload ~init ~handle_event ~draw)
;;


let () =
  main
    ~init:(Frogger.create ())
    ~handle_event:Frogger.handle_event
    ~draw:Frogger.draw

