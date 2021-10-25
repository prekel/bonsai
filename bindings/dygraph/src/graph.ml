[@@@js.dummy "!! This code has been generated by gen_js_api !!"]
[@@@ocaml.warning "-7-32-39"]
open! Core
open! Import
open Gen_js_api
type t = Ojs.t
let rec t_of_js : Ojs.t -> t = fun (x2 : Ojs.t) -> x2
and t_to_js : t -> Ojs.t = fun (x1 : Ojs.t) -> x1
let (create : Native_node.t -> Data.t -> Options.t -> t) =
  fun (x3 : Native_node.t) ->
    fun (x4 : Data.t) ->
      fun (x5 : Options.t) ->
        t_of_js
          (Ojs.new_obj (Ojs.get_prop_ascii Ojs.global "Dygraph")
             [|(Native_node.t_to_js x3);(Data.t_to_js x4);(Options.t_to_js x5)|])
let (destroy : t -> unit) =
  fun (x6 : t) -> ignore (Ojs.call (t_to_js x6) "destroy" [||])
let (resize : t -> unit) =
  fun (x7 : t) -> ignore (Ojs.call (t_to_js x7) "resize" [||])
let (updateOptions : t -> Update_options.t -> unit) =
  fun (x9 : t) ->
    fun (x8 : Update_options.t) ->
      ignore
        (Ojs.call (t_to_js x9) "updateOptions"
           [|(Update_options.t_to_js x8)|])
let (getArea : t -> Area.t) =
  fun (x10 : t) -> Area.t_of_js (Ojs.call (t_to_js x10) "getArea" [||])
let (isZoomed : t -> bool) =
  fun (x11 : t) -> Ojs.bool_of_js (Ojs.call (t_to_js x11) "isZoomed" [||])
let (resetZoom : t -> unit) =
  fun (x12 : t) -> ignore (Ojs.call (t_to_js x12) "resetZoom" [||])
