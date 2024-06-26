
(* open Stdlib *)

type 'v values =  Int
type 'k keys =  Int

type ('k, 'v) node =
  | NodeKey of ('k) keys
  | NodeValue of ('v) values
  | NodeSibling of ('k, 'v) t
  | NodeChild of ('k, 'v) t
and ('k, 'v) t =
  | Node of ('k, 'v) node
  | Empty

type ('k, 'v) head =
  | NodeSibling of ('k, 'v) t
  | NodeChild of ('k, 'v) t
