open! Core_kernel
open! Import
open! Bonsai_web

type ('result, 'parsed) t =
  { value : 'result
  ; set : 'parsed -> Vdom.Event.t
  }
[@@deriving fields]

val create : value:'result -> set:('parsed -> Vdom.Event.t) -> ('result, 'parsed) t
val lift : ('result, 'parsed1) t -> f:('parsed2 -> 'parsed1) -> ('result, 'parsed2) t

module With_view : sig
  type 'a t =
    { value : 'a
    ; view : Vdom.Node.t
    }
  [@@deriving fields]

  val create : value:'a -> view:Vdom.Node.t -> 'a t
end

module Same : sig
  type nonrec 'a t = ('a With_view.t, 'a) t
end

module Errorable : sig
  type nonrec ('result, 'parsed) t = ('result Or_error.t With_view.t, 'parsed) t

  module Same : sig
    type nonrec 'a t = ('a, 'a) t
  end
end
