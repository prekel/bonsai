open! Core
open! Async_kernel
open! Bonsai_web
open Bonsai.Let_syntax

module Effect = struct
  include Bonsai_web.Effect

  module Lwt_fun_arg = struct
    module Action = struct
      type 'r t = T : 'a * ('a -> 'r Lwt.t) -> 'r t
    end

    let handle (Action.T (a, f)) ~on_response =
      Lwt.async (fun () ->
          let%map.Lwt result = f a in
          on_response result)
    ;;
  end

  module Lwt_fun = Ui_effect.Define1 (Lwt_fun_arg)

  let of_lwt_fun f a = Lwt_fun.inject (T (a, f))
end

let fake_slow_capitalize_string_rpc_async =
  Effect.of_deferred_fun (fun text ->
      let rand_delay = Random.float_range 0.0 1.0 in
      let%map.Deferred () = Async_kernel.after (Time_ns.Span.of_sec rand_delay) in
      String.uppercase text)
;;

let fake_slow_capitalize_string_rpc_lwt =
  Effect.of_lwt_fun (fun text ->
      let rand_delay = Random.float_range 0.0 1.0 in
      let%map.Lwt () = Js_of_ocaml_lwt.Lwt_js.sleep rand_delay in
      String.uppercase text)
;;

let textbox =
  let%sub state = Bonsai.state [%here] (module String) ~default_model:"" in
  return
  @@ let%map text, set_text = state in
     let view =
       Vdom.Node.input
         ~attr:
           (Vdom.Attr.many
              [ Vdom.Attr.string_property "value" text
              ; Vdom.Attr.on_input (fun _ -> set_text)
              ])
         []
     in
     text, view
;;

let component fake_slow_capitalize_string_rpc =
  let%sub text, view = textbox in
  let%sub capitalized =
    Bonsai.Edge.Poll.(
      effect_on_change
        [%here]
        (module String)
        (module String)
        (Starting.initial "")
        text
        ~effect:(Value.return fake_slow_capitalize_string_rpc))
  in
  let%arr view = view
  and capitalized = capitalized in
  Vdom.Node.div [ view; Vdom.Node.text capitalized ]
;;

let supercomponent =
  let%sub co_async = component fake_slow_capitalize_string_rpc_async in
  let%sub co_lwt = component fake_slow_capitalize_string_rpc_lwt in
  let%arr co_async = co_async
  and co_lwt = co_lwt in
  Vdom.Node.div [ co_async; Vdom.Node.br (); co_lwt ]
;;

let (_ : _ Start.Handle.t) =
  Start.start Start.Result_spec.just_the_view ~bind_to_element_with_id:"app" supercomponent
;;
