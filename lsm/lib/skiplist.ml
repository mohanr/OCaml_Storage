
open Stdlib

let () = Random.self_init()

type 'v values =  Int
type 'k keys =  Int


type ('k, 'v) skiplist = Empty | Head of ('k, 'v) head |  Node of ('k, 'v) node
and  ('k, 'v)  node =
  { keys : 'k ;
    values : 'v;
    nodesibling :  ('k, 'v) skiplist;
    nodechild :  ('k, 'v) skiplist
  }
and ('k, 'v) head =
   { sibling :  ('k, 'v) skiplist ;
     child :  ('k, 'v) skiplist
   }
type ('k, 'v) skip_list  =
  { skpseed : int ;  (*Seed *)
    skpinternal :  ('k, 'v) head }


let empty_check (skp : ('k, 'v) skip_list) : bool =
  match skp.skpinternal with
  | { sibling = Empty; child = Empty } -> true
  | _ -> false
