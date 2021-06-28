open Yuujinchou
open Pattern

let trie (type a) (elem : a Alcotest.testable) : a Trie.t Alcotest.testable =
  let module M = struct
    type t = a Trie.t
    let pp fmt t = Alcotest.(pp @@ list @@ pair (list string) elem) fmt (List.of_seq @@ Trie.to_seq t)
    let equal = Trie.equal (Alcotest.equal elem)
  end in
  (module M)

let cantor ~rev_path:_ x y = if x == y then x else (x + y) * (x + y + 1) / 2 + y

let of_list l =
  Trie.of_seq
    (fun ~rev_path _ _ -> failwith @@ "conflicting keys at " ^ String.concat "." @@ List.rev rev_path)
    (List.to_seq l)

let error : [`BindingNotFound of Pattern.path] Alcotest.testable =
  let module M = struct
    type t = [`BindingNotFound of Pattern.path]
    let pp fmt (`BindingNotFound path) = Format.fprintf fmt "Binding not found: %a" Action.pp_path path
    let equal (`BindingNotFound p1) (`BindingNotFound p2) = p1 = p2
  end in
  (module M)

let run_result elem = Alcotest.result (trie elem) error

let test_none_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list []))
    (Action.run ~union:cantor none (of_list [["x"], 10]))

let test_none_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list []))
    (Action.run ~union:cantor none (of_list [[], 10]))

let test_none_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor none Trie.empty)

let test_any_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run ~union:cantor any (of_list [["x"], 10]))

let test_any_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [[], 10]))
    (Action.run ~union:cantor any (of_list [[], 10]))

let test_any_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor any Trie.empty)

let test_any_phy_eq () =
  let t = of_list [[], 10] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor any t) == t)

let test_wildcard_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run ~union:cantor wildcard (of_list [["x"], 10]))

let test_wildcard_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor wildcard (of_list [[], 10]))

let test_wildcard_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor wildcard Trie.empty)

let test_wildcard_phy_eq () =
  let t = of_list [["x"], 10] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor wildcard t) == t)

let test_root_1 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor root (of_list [["x"], 10]))

let test_root_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [[], 10]))
    (Action.run ~union:cantor root (of_list [[], 10]))

let test_root_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor root Trie.empty)

let test_root_phy_eq () =
  let t = of_list [[], 10] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor root t) == t)

let test_only_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run ~union:cantor (only ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_only_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (only ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20]))

let test_only_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (only ["x"]) Trie.empty)

let test_only_phy_eq () =
  let t = of_list [["x"], 10] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor (only ["x"]) t) == t)

let test_only_subtree_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10]))
    (Action.run ~union:cantor (only_subtree ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_only_subtree_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"; "y"], 10; ["x"; "x"], 20]))
    (Action.run ~union:cantor (only_subtree ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_only_subtree_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (only_subtree ["x"]) Trie.empty)

let test_only_subtree_phy_eq () =
  let t = of_list [["x"], 10] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor (only_subtree ["x"]) t) == t)

let test_except_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 20]))
    (Action.run ~union:cantor (except ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_except_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (except ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20]))

let test_except_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (except ["x"]) Trie.empty)

let test_except_subtree_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 20]))
    (Action.run ~union:cantor (except_subtree ["x"]) (of_list [["x"], 10; ["y"], 20]))

let test_except_subtree_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 30]))
    (Action.run ~union:cantor (except_subtree ["x"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_except_subtree_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (except_subtree ["x"]) Trie.empty)

let test_in_subtree_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"; "z"], 10; ["y"], 20]))
    (Action.run ~union:cantor (in_subtree ["x"] (renaming [] ["z"])) (of_list [["x"], 10; ["y"], 20]))

let test_in_subtree_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30]))
    (Action.run ~union:cantor (in_subtree ["x"] (renaming ["x"] ["w"])) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_in_subtree_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (in_subtree ["x"] any) Trie.empty)

let test_in_subtree_phy_eq () =
  let t = of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor (in_subtree ["x"] (renaming ["x"] ["x"])) t) == t)

let test_renaming_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["z"], 10; ["y"], 20]))
    (Action.run ~union:cantor (renaming ["x"] ["z"]) (of_list [["x"], 10; ["y"], 20]))

let test_renaming_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (renaming ["x"] ["z"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_renaming_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (renaming ["x"] ["z"]) Trie.empty)

let test_renaming_phy_eq () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor (renaming ["x"; "w"] ["x"; "w"]) t) == t)

let test_renaming_subtree_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["z"], 10; ["y"], 20]))
    (Action.run ~union:cantor (renaming_subtree ["x"] ["z"]) (of_list [["x"], 10; ["y"], 20]))

let test_renaming_subtree_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["z"; "y"], 10; ["z"; "x"], 20; ["y"], 30]))
    (Action.run ~union:cantor (renaming_subtree ["x"] ["z"]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_renaming_subtree_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (renaming_subtree ["x"] ["z"]) Trie.empty)

let test_renaming_subtree_phy_eq_1 () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor (renaming_subtree ["x"] ["x"]) t) == t)

let test_renaming_subtree_phy_eq_2 () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor (renaming_subtree ["x"; "y"] ["x"; "y"]) t) == t)

let test_seq_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["w"], 10; ["y"], 20]))
    (Action.run ~union:cantor (seq [renaming ["x"] ["z"]; renaming ["z"] ["w"]]) (of_list [["x"], 10; ["y"], 20]))

let test_seq_2 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor (seq [none; any]) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_seq_3 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound []))
    (Action.run ~union:cantor (seq [none; none]) Trie.empty)

let test_seq_4 () =
  Alcotest.(check @@ run_result int) "error"
    (Error (`BindingNotFound ["x"]))
    (Action.run ~union:cantor (seq [renaming ["x"] ["z"]; only ["x"]]) (of_list [["x"], 10; ["y"], 20]))

let test_seq_phy_eq () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor (seq [seq []; seq []]) t) == t)

let test_union_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["x"], 10; ["y"], 20]))
    (Action.run ~union:cantor (union [only ["x"]; except ["x"]]) (of_list [["x"], 10; ["y"], 20]))

let test_union_2 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list []))
    (Action.run ~union:cantor (union []) (of_list [["x"], 10; ["y"], 20]))

let test_union_phy_eq () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok (Action.run ~union:cantor (union [seq []]) t) == t)

let test_filter_map_1 () =
  Alcotest.(check @@ run_result int) "ok"
    (Ok (of_list [["y"], 110]))
    (Action.run_with_hooks
       ~hooks:(fun () ~rev_prefix t ->
           Result.ok @@ Trie.filter_mapi_endo ~rev_prefix
             (fun ~rev_path:_ d -> if d > 20 then Some (d + 80) else None) t)
       ~union:cantor
       (hook ()) (of_list [["x"; "y"], 10; ["x"; "x"], 20; ["y"], 30]))

let test_filter_map_phy_eq () =
  let t = of_list [["x"; "y"], 10; ["x"; "w"], 20; ["y"], 30] in
  Alcotest.(check bool) "true"
    true
    (Result.get_ok
       (Action.run_with_hooks
          ~hooks:(fun () ~rev_prefix t ->
              Result.ok @@ Trie.filter_mapi_endo ~rev_prefix
                (fun ~rev_path:_ x -> Some x) t)
          ~union:cantor
          (hook ()) t) == t)

let () =
  let open Alcotest in
  run "Action" [
    "none", [
      test_case "none" `Quick test_none_1;
      test_case "none" `Quick test_none_2;
      test_case "none" `Quick test_none_3;
    ];
    "any", [
      test_case "any" `Quick test_any_1;
      test_case "any" `Quick test_any_2;
      test_case "any" `Quick test_any_3;
      test_case "physical equality" `Quick test_any_phy_eq;
    ];
    "wildcard", [
      test_case "wildcard" `Quick test_wildcard_1;
      test_case "wildcard" `Quick test_wildcard_2;
      test_case "wildcard" `Quick test_wildcard_3;
      test_case "physical equality" `Quick test_wildcard_phy_eq;
    ];
    "root", [
      test_case "root" `Quick test_root_1;
      test_case "root" `Quick test_root_2;
      test_case "root" `Quick test_root_3;
      test_case "physical equality" `Quick test_root_phy_eq;
    ];
    "only", [
      test_case "only" `Quick test_only_1;
      test_case "only" `Quick test_only_2;
      test_case "only" `Quick test_only_3;
      test_case "physical equality" `Quick test_only_phy_eq;
    ];
    "only_subtree", [
      test_case "only_subtree" `Quick test_only_subtree_1;
      test_case "only_subtree" `Quick test_only_subtree_2;
      test_case "only_subtree" `Quick test_only_subtree_3;
      test_case "physical equality" `Quick test_only_subtree_phy_eq;
    ];
    "except", [
      test_case "except" `Quick test_except_1;
      test_case "except" `Quick test_except_2;
      test_case "except" `Quick test_except_3;
    ];
    "except_subtree", [
      test_case "except_subtree" `Quick test_except_subtree_1;
      test_case "except_subtree" `Quick test_except_subtree_2;
      test_case "except_subtree" `Quick test_except_subtree_3;
    ];
    "in_subtree", [
      test_case "in_subtree" `Quick test_in_subtree_1;
      test_case "in_subtree" `Quick test_in_subtree_2;
      test_case "in_subtree" `Quick test_in_subtree_3;
      test_case "physical equality" `Quick test_in_subtree_phy_eq;
    ];
    "renaming", [
      test_case "renaming" `Quick test_renaming_1;
      test_case "renaming" `Quick test_renaming_2;
      test_case "renaming" `Quick test_renaming_3;
      test_case "physical equality" `Quick test_renaming_phy_eq;
    ];
    "renaming_subtree", [
      test_case "renaming_subtree" `Quick test_renaming_subtree_1;
      test_case "renaming_subtree" `Quick test_renaming_subtree_2;
      test_case "renaming_subtree" `Quick test_renaming_subtree_3;
      test_case "physical equality" `Quick test_renaming_subtree_phy_eq_1;
      test_case "physical equality" `Quick test_renaming_subtree_phy_eq_2;
    ];
    "seq", [
      test_case "seq" `Quick test_seq_1;
      test_case "seq" `Quick test_seq_2;
      test_case "seq" `Quick test_seq_3;
      test_case "seq" `Quick test_seq_4;
      test_case "physical equality" `Quick test_seq_phy_eq;
    ];
    "union", [
      test_case "union" `Quick test_union_1;
      test_case "union" `Quick test_union_2;
      test_case "physical equality" `Quick test_union_phy_eq;
    ];
    "filter_map", [
      test_case "filter_map" `Quick test_filter_map_1;
      test_case "physical equality" `Quick test_filter_map_phy_eq;
    ];
  ]
