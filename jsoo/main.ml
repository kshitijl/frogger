open Js_of_ocaml
(* ## - method will get called as soon I deref.
   `##.prop_name := ` to set a property
   ##.prop_name to read (no deref)
*)

let () =
  Dom_html.window##.onload := (Dom.handler (fun _ ->
  let foo_div =
    match
      (Js.Opt.to_option (Dom_html.document##getElementById (Js.string "foo")))
    with
    | Some x -> x
    | None   -> failwith "none"
  in
  foo_div##.textContent := Js.Opt.return (Js.string "Hello, world");
  Js._true
  ))
;;
