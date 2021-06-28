open Yuujinchou

let trie (type a) (elem : a Alcotest.testable) : a Trie.t Alcotest.testable =
  let module M = struct
    type t = a Trie.t
    let pp fmt t = Alcotest.(pp @@ list @@ pair (list string) elem) fmt (List.of_seq @@ Trie.to_seq t)
    let equal = Trie.equal (Alcotest.equal elem)
  end in
  (module M)

let cantor_val x y = if x == y then x else (x + y) * (x + y + 1) / 2 + y
let cantor ~rev_path:_ x y = cantor_val x y

let of_list l =
  Trie.of_seq
    (fun ~rev_path _ _ -> failwith @@ "conflicting keys at " ^ String.concat "." @@ List.rev rev_path)
    (List.to_seq l)

let test_empty () =
  Alcotest.(check @@ trie int) "same trie" (of_list []) Trie.empty

let test_is_empty_empty () =
  Alcotest.(check bool) "empty" true (Trie.is_empty Trie.empty)

let test_is_empty_root () =
  Alcotest.(check bool) "not empty" false (Trie.is_empty @@ of_list [[], [1]])

let test_root_opt_none () =
  Alcotest.(check @@ trie int) "same trie" Trie.empty (Trie.root_opt None)

let test_root_opt_some () =
  Alcotest.(check @@ trie int) "same trie" (of_list [[], 10]) (Trie.root_opt (Some 10))

let test_prefix_1 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"], 10; ["x"; "y"; "z"], 100])
    (Trie.prefix ["x"] (of_list [[], 10; ["y"; "z"], 100]))

let test_prefix_2 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x1"; "x2"; "y"], 10; ["x1"; "x2"; "z"], 20])
    (Trie.prefix ["x1"; "x2"] (of_list [["y"], 10; ["z"], 20]))

let test_singleton_1 () =
  Alcotest.(check @@ trie int) "same trie" (of_list [[], 100]) (Trie.singleton ([], 100))

let test_singleton_2 () =
  Alcotest.(check @@ trie int) "same trie" (of_list [["x"], 100]) (Trie.singleton (["x"], 100))

let test_root () =
  Alcotest.(check @@ trie int) "same trie" (of_list [[], 100]) (Trie.root 100)

let test_equal_1 () =
  Alcotest.(check bool) "true"
    true
    (Trie.equal Alcotest.(equal int) (of_list [["x"; "y"], 100]) (of_list [["x"; "y"], 100]))

let test_equal_2 () =
  Alcotest.(check bool) "false"
    false
    (Trie.equal Alcotest.(equal int) (of_list [["x"; "y"], 100]) (of_list [["x"; "z"], 100]))

let test_equal_phy_eq () =
  let t = of_list [["x"], 1] in
  Alcotest.(check bool) "true"
    true
    (Trie.equal (fun _ _ -> failwith "not using physical equality") t t)

let test_find_subtree_1 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [[], 10; ["x"], 20])
    (Trie.find_subtree [] (of_list [[], 10; ["x"], 20]))

let test_find_subtree_2 () =
  Alcotest.(check @@ trie int) "same trie"
    Trie.empty
    (Trie.find_subtree ["y"] (of_list [[], 10; ["x"], 20]))

let test_find_singleton_1 () =
  Alcotest.(check @@ option int) "same answer"
    (Some 10)
    (Trie.find_singleton [] (of_list [[], 10; ["x"], 20]))

let test_find_singleton_2 () =
  Alcotest.(check @@ option int) "same answer"
    None
    (Trie.find_singleton ["y"] (of_list [[], 10; ["x"], 20]))

let test_find_root_1 () =
  Alcotest.(check @@ option int) "same answer"
    (Some 10)
    (Trie.find_root (of_list [[], 10; ["x"], 20]))

let test_find_root_2 () =
  Alcotest.(check @@ option int) "same answer"
    None
    (Trie.find_root (of_list [["x"], 20]))

let test_find_root_3 () =
  Alcotest.(check @@ option int) "same answer"
    None
    (Trie.find_root Trie.empty)

let test_mapi_1 () =
  Alcotest.(check @@ trie (pair (list string) int)) "same trie"
    (of_list [["x"; "y"], (["y"; "x"], 11); [], ([], 21)])
    (Trie.mapi (fun ~rev_path x -> rev_path, x + 1) (of_list [["x"; "y"], 10; [], 20]))

let test_mapi_2 () =
  Alcotest.(check @@ trie (pair (list string) int)) "same trie"
    Trie.empty
    (Trie.mapi (fun ~rev_path x -> rev_path, x + 1) Trie.empty)

let test_mapi_endo_1 () =
  Alcotest.(check @@ trie @@ pair (list string) int) "same trie"
    (of_list [["x"; "y"], (["z"; "y"; "x"], 11); [], (["w"], 21)])
    (Trie.mapi_endo (fun ~rev_path (p, x) -> (List.rev_append p rev_path, x + 1)) (of_list [["x"; "y"], (["z"], 10); [], (["w"], 20)]))

let test_mapi_endo_2 () =
  Alcotest.(check @@ trie int) "same trie"
    Trie.empty
    (Trie.mapi_endo (fun ~rev_path:_ x -> x + 1) Trie.empty)

let test_mapi_endo_phy_eq_1 () =
  let t = Trie.empty in
  Alcotest.(check bool) "true"
    true
    (Trie.mapi_endo (fun ~rev_path:_ x -> x + 2) t == t)

let test_mapi_endo_phy_eq_2 () =
  let t = of_list [["x"], 40; ["x"; "y"], 160] in
  Alcotest.(check bool) "true"
    true
    (Trie.mapi_endo (fun ~rev_path:_ x -> x) t == t)

let test_filteri_1 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"], 20; [], 40])
    (Trie.filteri (fun ~rev_path:_ x -> x > 15) (of_list [["x"; "y"], 10; ["x"], 20; [], 40]))

let test_filteri_2 () =
  Alcotest.(check @@ trie int) "same trie"
    Trie.empty
    (Trie.filteri (fun ~rev_path:_ x -> x > 100) (of_list [["x"; "y"], 10; ["x"], 20; [], 40]))

let test_filteri_3 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], 10])
    (Trie.filteri (fun ~rev_path _ -> match rev_path with "y"::_ -> true | _ -> false)
         (of_list [["x"; "y"], 10; ["x"], 20; [], 40]))

let test_filteri_4 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"], 20])
    (Trie.filteri (fun ~rev_path _ -> match rev_path with ["x"] -> true | _ -> false)
         (of_list [["x"; "y"], 10; ["x"], 20; [], 40]))

let test_filteri_phy_eq_1 () =
  let t = Trie.empty in
  Alcotest.(check bool) "true"
    true
    (Trie.filteri (fun ~rev_path:_ x -> x > 100) t == t)

let test_filteri_phy_eq_2 () =
  let t = of_list [["x"], 40; ["x"; "y"], 160] in
  Alcotest.(check bool) "true"
    true
    (Trie.filteri (fun ~rev_path:_ _ -> true) t == t)

let test_filter_mapi_1 () =
  Alcotest.(check @@ trie @@ pair (list string) int) "same trie"
    (of_list [[], ([], 30)])
    (Trie.filter_mapi (fun ~rev_path x -> if x > 10 then Some (rev_path, 30) else None) (of_list [["x"; "y"], 10; [], 20]))

let test_filter_mapi_2 () =
  Alcotest.(check @@ trie @@ pair unit int) "same trie"
    Trie.empty
    (Trie.filter_mapi (fun ~rev_path:_ _ -> None) (of_list [["x"; "y"], 10; [], 20]))

let test_filter_mapi_endo_1 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [[], 30])
    (Trie.filter_mapi_endo (fun ~rev_path:_ x -> if x > 10 then Some 30 else None) (of_list [["x"; "y"], 10; [], 20]))

let test_filter_mapi_endo_2 () =
  Alcotest.(check @@ trie int) "same trie"
    Trie.empty
    (Trie.filter_mapi_endo (fun ~rev_path:_ _ -> None) (of_list [["x"; "y"], 10; [], 20]))

let test_filter_mapi_endo_3 () =
  Alcotest.(check @@ trie (pair (list string) int)) "same trie"
    (of_list [["w"], (["b"; "w"], 30)])
    (Trie.filter_mapi_endo (fun ~rev_path (p, x) -> if x > 10 then Some (List.rev_append p rev_path, 30) else None)
       (of_list [["x"; "y"], (["a"], 10); ["w"], (["b"], 20)]))

let test_filter_mapi_endo_phy_eq_1 () =
  let t = Trie.empty in
  Alcotest.(check bool) "true"
    true
    (Trie.filter_mapi_endo (fun ~rev_path:_ _ -> None) t == t)

let test_filter_mapi_endo_phy_eq_2 () =
  let t = of_list [["x"], 40; ["x"; "y"], 160] in
  Alcotest.(check bool) "true"
    true
    (Trie.filter_mapi_endo (fun ~rev_path:_ x -> Some x) t == t)

let test_update_subtree () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], cantor_val 160 10; ["x"], 40])
    (Trie.update_subtree ["x"] (fun t -> Trie.union_singleton cantor t (["y"], 10)) (of_list [["x"], 40; ["x"; "y"], 160]))

let test_update_subtree_phy_eq () =
  let t = of_list [["x"], 40; ["x"; "y"], 160] in
  Alcotest.(check bool) "true"
    true
    (Trie.update_subtree ["x"] (fun t -> t) t == t)

let test_update_singleton_1 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], 160; ["x"], 80])
    (Trie.update_singleton ["x"] (fun t -> Some (Option.get t * 2)) (of_list [["x"], 40; ["x"; "y"], 160]))

let test_update_singleton_2 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], 160])
    (Trie.update_singleton ["x"] (fun _ -> None) (of_list [["x"], 40; ["x"; "y"], 160]))

let test_update_singleton_3 () =
  Alcotest.(check @@ trie int) "same trie"
    Trie.empty
    (Trie.update_singleton ["x"] (fun _ -> None) (of_list [["x"], 40]))

let test_update_singleton_phy_eq_1 () =
  let t = of_list [["x"], 40; ["x"; "y"], 160] in
  Alcotest.(check bool) "true"
    true
    (Trie.update_singleton ["x"] (fun x -> x) t == t)

let test_update_singleton_phy_eq_2 () =
  let t = Trie.empty in
  Alcotest.(check bool) "true"
    true
    (Trie.update_singleton ["x"] (fun x -> x) t == t)

let test_update_root_1 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], 160; [], 80])
    (Trie.update_root (fun t -> Some (Option.get t * 2)) (of_list [[], 40; ["x"; "y"], 160]))

let test_update_root_2 () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], 160])
    (Trie.update_root (fun _ -> None) (of_list [[], 40; ["x"; "y"], 160]))

let test_update_root_3 () =
  Alcotest.(check @@ trie int) "same trie"
    Trie.empty
    (Trie.update_root (fun _ -> None) (of_list [[], 40]))

let test_update_root_phy_eq_1 () =
  let t = of_list [["x"], 40; ["x"; "y"], 160] in
  Alcotest.(check bool) "true"
    true
    (Trie.update_root (fun x -> x) t == t)

let test_update_root_phy_eq_2 () =
  let t = Trie.empty in
  Alcotest.(check bool) "true"
    true
    (Trie.update_root (fun x -> x) t == t)

let test_union () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], cantor_val 10 160; [], 20; ["x"], 40])
    (Trie.union cantor (of_list [["x"; "y"], 10; [], 20]) (of_list [["x"], 40; ["x"; "y"], 160]))

let test_union_phy_eq_1 () =
  let t = of_list [["x"; "y"], 10; [], 20] in
  Alcotest.(check bool) "eq"
    true
    (Trie.union cantor t Trie.empty == t)

let test_union_phy_eq_2 () =
  let t = of_list [["x"; "y"], 10; [], 20] in
  Alcotest.(check bool) "eq"
    true
    (Trie.union cantor Trie.empty t == t)

let test_union_phy_eq_3 () =
  let t = of_list [["x"; "y"], 10; [], 20] in
  let sub, others = Trie.detach_subtree ["x"] t in
  let t' = Trie.union cantor others (Trie.prefix ["x"] sub) in
  Alcotest.(check bool) "eq"
    true
    (Trie.physically_equal (Trie.find_subtree ["x"] t') sub)

let test_union_phy_eq_4 () =
  let t = of_list [[], 10] in
  Alcotest.(check bool) "eq"
    true
    (Trie.union cantor t t == t)

let test_union_subtree () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], 10; [], 20; ["x"], cantor_val 40 80; ["x"; "x"; "y"], 1600])
    (Trie.union_subtree cantor (of_list [["x"; "y"], 10; [], 20; ["x"], 40]) (["x"], of_list [[], 80; ["x"; "y"], 1600]))

let test_union_subtree_phy_eq_1 () =
  let t = of_list [["x"; "y"], 10; [], 20] in
  Alcotest.(check bool) "eq"
    true
    (Trie.union_subtree cantor t (["x"], Trie.empty) == t)

let test_union_subtree_phy_eq_2 () =
  let t = of_list [["x"; "y"], 10; [], 20] in
  let sub, others = Trie.detach_subtree ["x"] t in
  let t' = Trie.union_subtree cantor others (["x"], sub) in
  Alcotest.(check bool) "eq"
    true
    (Trie.physically_equal (Trie.find_subtree ["x"] t') sub)

let test_union_singleton () =
  Alcotest.(check @@ trie int) "same trie"
    (of_list [["x"; "y"], cantor_val 10 160; [], 20; ["x"], 40])
    (Trie.union_singleton cantor (of_list [["x"; "y"], 10; [], 20; ["x"], 40]) (["x"; "y"], 160))

let test_union_singleton_phy_eq () =
  let ten = 10 in
  let t = of_list [["x"; "y"], ten; [], 20] in
  Alcotest.(check bool) "eq"
    true
    (Trie.union_singleton cantor t (["x"; "y"], ten) == t)

let test_detach_subtree_1 () =
  Alcotest.(check @@ pair (trie int) (trie int)) "same trie"
    (of_list [["y"], 10], of_list [[], 20])
    (Trie.detach_subtree ["x"] (of_list [["x"; "y"], 10; [], 20]))

let test_detach_subtree_2 () =
  Alcotest.(check @@ pair (trie int) (trie int)) "same trie"
    (of_list [[], 10], Trie.empty)
    (Trie.detach_subtree ["x"] (of_list [["x"], 10]))

let test_detach_subtree_3 () =
  Alcotest.(check @@ pair (trie int) (trie int)) "same trie"
    (Trie.empty, of_list [[], 10])
    (Trie.detach_subtree ["x"] (of_list [[], 10]))

let test_detach_singleton_1 () =
  Alcotest.(check @@ pair (option int) (trie int)) "same trie"
    (None, of_list [["x"; "y"], 10; [], 20])
    (Trie.detach_singleton ["x"] (of_list [["x"; "y"], 10; [], 20]))

let test_detach_singleton_2 () =
  Alcotest.(check @@ pair (option int) (trie int)) "same trie"
    (Some 20, of_list [["x"; "y"], 10])
    (Trie.detach_singleton [] (of_list [["x"; "y"], 10; [], 20]))

let test_detach_singleton_3 () =
  Alcotest.(check @@ pair (option int) (trie int)) "same trie"
    (Some 10, Trie.empty)
    (Trie.detach_singleton ["x"; "y"] (of_list [["x"; "y"], 10]))

let () =
  let open Alcotest in
  run "Trie" [
    "empty", [
      test_case "empty" `Quick test_empty;
    ];
    "is_empty", [
      test_case "is_empty" `Quick test_is_empty_empty;
      test_case "is_empty" `Quick test_is_empty_root;
    ];
    "root_opt", [
      test_case "root_opt" `Quick test_root_opt_none;
      test_case "root_opt" `Quick test_root_opt_some;
    ];
    "prefix", [
      test_case "prefix" `Quick test_prefix_1;
      test_case "prefix" `Quick test_prefix_2;
    ];
    "equal", [
      test_case "equal" `Quick test_equal_1;
      test_case "equal" `Quick test_equal_2;
      test_case "physical equality" `Quick test_equal_phy_eq;
    ];
    "singleton", [
      test_case "singleton" `Quick test_singleton_1;
      test_case "singleton" `Quick test_singleton_2;
    ];
    "root", [
      test_case "root" `Quick test_root;
    ];
    "find_subtree", [
      test_case "find_subtree" `Quick test_find_subtree_1;
      test_case "find_subtree" `Quick test_find_subtree_2;
    ];
    "find_singleton", [
      test_case "find_singleton" `Quick test_find_singleton_1;
      test_case "find_singleton" `Quick test_find_singleton_2;
    ];
    "find_root", [
      test_case "find_root" `Quick test_find_root_1;
      test_case "find_root" `Quick test_find_root_2;
      test_case "find_root" `Quick test_find_root_3;
    ];
    "map", [
      test_case "mapi" `Quick test_mapi_1;
      test_case "mapi" `Quick test_mapi_2;
    ];
    "map_endo", [
      test_case "mapi_endo" `Quick test_mapi_endo_1;
      test_case "map_endo" `Quick test_mapi_endo_2;
      test_case "physical equality" `Quick test_mapi_endo_phy_eq_1;
      test_case "physical equality" `Quick test_mapi_endo_phy_eq_2;
    ];
    "filteri", [
      test_case "filteri" `Quick test_filteri_1;
      test_case "filteri" `Quick test_filteri_2;
      test_case "filteri" `Quick test_filteri_3;
      test_case "filteri" `Quick test_filteri_4;
      test_case "physical equality" `Quick test_filteri_phy_eq_1;
      test_case "physical equality" `Quick test_filteri_phy_eq_2;
    ];
    "filter_map", [
      test_case "filter_mapi" `Quick test_filter_mapi_1;
      test_case "filter_mapi" `Quick test_filter_mapi_2;
    ];
    "filter_map_endo", [
      test_case "filter_mapi_endo" `Quick test_filter_mapi_endo_1;
      test_case "filter_mapi_endo" `Quick test_filter_mapi_endo_2;
      test_case "filter_mapi_endo" `Quick test_filter_mapi_endo_3;
      test_case "physical equality" `Quick test_filter_mapi_endo_phy_eq_1;
      test_case "physical equality" `Quick test_filter_mapi_endo_phy_eq_2;
    ];
    "update_subtree", [
      test_case "update_subtree" `Quick test_update_subtree;
      test_case "physical equality" `Quick test_update_subtree_phy_eq;
    ];
    "update_singleton", [
      test_case "update_singleton" `Quick test_update_singleton_1;
      test_case "update_singleton" `Quick test_update_singleton_2;
      test_case "update_singleton" `Quick test_update_singleton_3;
      test_case "physical equality" `Quick test_update_singleton_phy_eq_1;
      test_case "physical equality" `Quick test_update_singleton_phy_eq_2;
    ];
    "update_root", [
      test_case "update_root" `Quick test_update_root_1;
      test_case "update_root" `Quick test_update_root_2;
      test_case "update_root" `Quick test_update_root_3;
      test_case "physical equality" `Quick test_update_root_phy_eq_1;
      test_case "physical equality" `Quick test_update_root_phy_eq_2;
    ];
    "union", [
      test_case "union" `Quick test_union;
      test_case "physical equality" `Quick test_union_phy_eq_1;
      test_case "physical equality" `Quick test_union_phy_eq_2;
      test_case "physical equality" `Quick test_union_phy_eq_3;
      test_case "physical equality" `Quick test_union_phy_eq_4;
    ];
    "union_subtree", [
      test_case "union_subtree" `Quick test_union_subtree;
      test_case "physical equality" `Quick test_union_subtree_phy_eq_1;
      test_case "physical equality" `Quick test_union_subtree_phy_eq_2;
    ];
    "union_singleton", [
      test_case "union_singleton" `Quick test_union_singleton;
      test_case "physical equality" `Quick test_union_singleton_phy_eq;
    ];
    "detach_subtree", [
      test_case "detach_subtree" `Quick test_detach_subtree_1;
      test_case "detach_subtree" `Quick test_detach_subtree_2;
      test_case "detach_subtree" `Quick test_detach_subtree_3;
    ];
    "detach_singleton", [
      test_case "detach_singleton" `Quick test_detach_singleton_1;
      test_case "detach_singleton" `Quick test_detach_singleton_2;
      test_case "detach_singleton" `Quick test_detach_singleton_3;
    ];
  ]
