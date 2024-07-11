
open Skiplist

let%expect_test _=

  let () =
  let state = Random.State.make_self_init() in
  let skp =create_empty_skiplist state in
  assert ( create_empty_skiplist state = skp) in ()
