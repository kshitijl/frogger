open Base
open Js_of_ocaml
(* ## - method will get called as soon I deref.
   `##.prop_name := ` to set a property
   ##.prop_name to read (no deref)

   put a table here ; bg here ; each cell has an id
*)

let get_by_id id =
  Option.value_exn (
    (Js.Opt.to_option (Dom_html.document##getElementById (Js.string id))))
;;

let get_foo_div () = get_by_id "foo"
;;

let () =
  Dom_html.window##.onload := (Dom.handler (fun _ ->
      let foo_div = get_foo_div () in
      foo_div##.textContent := Js.Opt.return (Js.string "Hello, world!");
      Js._true
    ));
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
      Js._true));
  let set_cell column img_path =
    let img_id = "0." ^ (Int.to_string column) in
    let img_elem = get_by_id img_id in
    img_elem##setAttribute (Js.string "src") (Js.string img_path);
  in
  let frog_col = ref 0 in
  let blank_col = ref 1 in
  let _ =
    Dom_html.window##setInterval (Js.wrap_callback (fun () ->
        set_cell !frog_col  "assets/frog-icon.png";
        set_cell !blank_col "";
        let tmp = !frog_col in
        frog_col := !blank_col;
        blank_col := tmp
      ))
      1000.0
  in
  ()
;;
