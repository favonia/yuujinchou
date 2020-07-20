open StdLabels

type path = string list

module StringMap = Map.Make (struct
    type t = string
    let compare = String.compare
  end)

type 'a node = {
  root : 'a option;
  children : 'a node StringMap.t;
}

type 'a t = 'a node option

let empty : 'a t = None

let is_empty : 'a t -> bool = Option.is_none

let non_empty (t : 'a node) : 'a t = Some t

(** {1 Making (non-empty) trees} *)

let mk_tree root children =
  if Option.is_none root && StringMap.is_empty children
  then empty
  else non_empty {root; children}

let mk_root_node data = {root = Some data; children = StringMap.empty}

let mk_root_opt root = Option.map mk_root_node root

let rec prefix_node path t : 'a node =
  let f seg t =
    {root = None; children = StringMap.singleton seg @@ prefix_node path t}
  in
  List.fold_right ~f path ~init:t

let prefix path = Option.map @@ prefix_node path

let singleton_node (path, data) = prefix_node path @@ mk_root_node data

let singleton (path, data) = non_empty @@ singleton_node (path, data)

let root data = non_empty @@ mk_root_node data

(** {1 Getting data} *)

let rec find_node_cont path t k =
  match path with
  | [] -> k t
  | seg::path ->
    Option.bind (StringMap.find_opt seg t.children) @@ fun t ->
    find_node_cont path t k

let find_subtree path t =
  Option.bind t @@ fun t -> find_node_cont path t non_empty

let find_singleton path t =
  Option.bind t @@ fun t -> find_node_cont path t @@ fun t -> t.root

let find_root t = find_singleton [] t

(** {1 Traversing the trees} *)

let rec update_node_cont t path k =
  match path with
  | [] -> k @@ non_empty t
  | seg::path ->
    mk_tree t.root @@ StringMap.update seg (fun t -> update_cont t path k) t.children

and update_cont t path k =
  match t with
  | None -> prefix path @@ k empty
  | Some t -> update_node_cont t path k

(** {1 Union} *)

let union_option f x x' =
  match x, x' with
  | _, None -> x
  | None, _ -> x'
  | Some x, Some x' -> Some (f x x')

let rec union_node m t t' =
  let root = union_option m t.root t'.root in
  let children =
    let f _key t t' = Some (union_node m t t') in
    StringMap.union f t.children t'.children
  in
  {root; children}

let union m = union_option @@ union_node m

let union_subtree m t (path, t') =
  match t, t' with
  | None, _ -> prefix path t'
  | _, None -> t
  | Some t, Some t' ->
    update_node_cont t path @@ function
    | None -> non_empty t'
    | Some t -> non_empty @@ union_node m t t'

let union_singleton m t (path, data) =
  match t with
  | None -> singleton (path, data)
  | Some t -> update_node_cont t path @@ function
    | None -> non_empty @@ mk_root_node data
    | Some t -> non_empty {t with root = union_option m t.root @@ Some data}

let union_root m t data = union_singleton m t ([], data)

(** {1 Updating trees} *)

let update_subtree path f t = update_cont t path f

let update_singleton path f t = update_cont t path @@
  function
  | None -> mk_root_opt @@ f None
  | Some t -> mk_tree (f t.root) t.children

let update_root f t = update_singleton [] f t

(** {1 Detaching subtrees} *)

let rec update_extra_node_cont path t k =
  match path with
  | [] -> k @@ non_empty t
  | seg::path ->
    let new_child, info = update_extra_cont path (StringMap.find_opt seg t.children) k in
    let children = StringMap.update seg (Fun.const new_child) t.children in
    mk_tree t.root children, info

and update_extra_cont path t k =
  match t with
  | None -> let t, info = k empty in prefix path t, info
  | Some t -> update_extra_node_cont path t k

let detach_subtree path t = update_extra_cont path t @@ fun t -> empty, t

let detach_singleton path t = update_extra_cont path t @@ function
  | None -> empty, None
  | Some t -> mk_tree None t.children, t.root

let detach_root t = detach_singleton [] t

(** {1 Conversion from/to Seq} *)

let rec node_to_seq prefix_stack t () =
  match t.root with
  | None -> children_to_seq prefix_stack t.children ()
  | Some data ->
    let path = List.rev prefix_stack in
    Seq.Cons ((path, data), children_to_seq prefix_stack t.children)

and children_to_seq prefix_stack children =
  StringMap.to_seq children |> Seq.flat_map @@ fun (seg, t) ->
  node_to_seq (seg :: prefix_stack) t

let to_seq t = Option.fold ~none:Seq.empty ~some:(node_to_seq []) t

let of_seq m = Seq.fold_left (union_singleton m) empty
