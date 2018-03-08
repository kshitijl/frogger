open Base
open Js_of_ocaml
(* ## - method will get called as soon I deref.
   `##.prop_name := ` to set a property
   ##.prop_name to read (no deref)

   -keypress events should trigger a redraw.
   z-index if there's trouble with over/under bg

*)

let get_by_id id =
  Option.value_exn (
    (Js.Opt.to_option (Dom_html.document##getElementById (Js.string id))))
;;

let get_foo_div () = get_by_id "foo"
;;

let set_cell row column img_path =
    let img_id = (Int.to_string row) ^ "." ^ (Int.to_string column) in
    let img_elem = get_by_id img_id in
    img_elem##setAttribute (Js.string "src") (Js.string img_path);
;;

let render frog_row frog_col =
  List.iter [0;1] ~f:(fun row ->
      List.iter [0;1] ~f:(fun col ->
          set_cell row col ""));
  set_cell frog_row frog_col  "assets/frog-icon.png";
;;

let () =
  Dom_html.window##.onload := (Dom.handler (fun _ ->
      let foo_div = get_foo_div () in
      foo_div##.textContent := Js.Opt.return (Js.string "Hello, world!");
      Js._true
    ));
  let frog_row = ref 1 in
  let frog_col = ref 0 in
  Dom_html.window##.onkeydown := (Dom.handler (fun key_event ->
      let foo_div = get_foo_div () in
      let key = Option.value_exn (Js.Optdef.to_option (key_event##.key)) in
      let () = 
        match Js.to_string key with
        | "ArrowUp"
        | "ArrowDown"
        | "ArrowLeft"
        | "ArrowRight" -> foo_div##.textContent := Js.Opt.return key
        | _            -> ()
      in
      let () =
        match Js.to_string key with
        | "ArrowUp"   -> frog_row := if !frog_row = 0 then !frog_row else !frog_row - 1
        | "ArrowDown" -> frog_row := if !frog_row = 1 then !frog_row else !frog_row + 1
        | _ -> ()
      in
      render !frog_row !frog_col;
      Js._true));
  let _ =
    Dom_html.window##setInterval (Js.wrap_callback (fun () ->
        render !frog_row !frog_col;
        frog_col := (!frog_col + 1) % 2;
      ))
      1000.0
  in
  ()
;;
