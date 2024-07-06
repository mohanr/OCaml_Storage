
open Stdlib

let () = Random.self_init()

type 'v values =  Int
type 'k keys =  Int

type ('k, 'v) node =
  | Empty
  | Node of {
      keys : 'k;
      values : 'v;
      sibling : ('k, 'v) node;
      child : ('k, 'v) node;
    }
  | Head of
   { sibling :  ('k, 'v) node;
     child :  ('k, 'v) node
   }

type ('k, 'v) skip_list  =
  { skpseed : int ;  (*Seed *)
    skpinternal :  ('k, 'v) node }


let empty_check (skp : ('k, 'v) skip_list) : bool =
  match skp.skpinternal with
  | Head { sibling = Empty; child = Empty } -> true
  | _ -> false

let create_empty_skiplist : ('k, 'v) skip_list =
     let head = Head { sibling = Empty; child = Empty } in
     let seed = Random.int 1 in
     { skpseed = seed ; skpinternal = head }


let rec addlevels k v internal levels : ('k, 'v ) node =
  match levels with
        | levels when levels  > 0  -> create_level k v levels ( splitlevel internal)
        | _  -> internal
  and
    create_level k v levels (l, r) =
    match levels with
        | 1 ->  let node =  Node { keys = k; values = v; sibling = Empty; child = r} in
             Head { sibling = node ; child = r }
        | _ ->
              let head = Head { sibling = Empty; child = l } in
              let head1 = Head { sibling = Empty; child = r } in
              create_level k v (levels - 1) (head , head1)
  and
    splitlevel ( node : ('k, 'v) node ) :  ('k, 'v) node * ('k, 'v) node  =
      match node with
      | Head { sibling = _; child = _ } as head1 ->
        let head = Head { sibling = Empty; child = head1 } in
        (head, node)
       | ns ->
          let (next,r)  = splitlevel ns in
          (next ,  r)
