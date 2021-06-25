type path = string list

type switch = [`Keep | `Hide]

type 'custom act =
  | A_switch of switch
  | A_custom of 'custom

let pp_act pp_custom fmt =
  function
  | A_switch `Keep -> Format.pp_print_string fmt "use"
  | A_switch `Hide -> Format.pp_print_string fmt "hide"
  | A_custom f -> Format.fprintf fmt "@[<hov 1>(custom@ %a)@]" pp_custom f

let act_use = A_switch `Keep
let act_hide = A_switch `Hide

type 'custom split =
  { mode : [`Subtree | `Node]
  ; prefix : path
  ; prefix_replacement : path option
  ; on_target : 'custom t
  ; on_others : 'custom t
  }

and 'custom t =
  | P_act of 'custom act
  | P_split of 'custom split
  | P_seq of 'custom t list
  | P_union of 'custom t list

let id = P_seq []
let hide = P_act act_hide
let use = P_act act_use
let ignore = P_union []

let any = use
let none = hide

let[@inline] select_and_update_subtree prefix on_target =
  P_split {mode = `Subtree; prefix; prefix_replacement = None; on_target; on_others = ignore}
let[@inline] select_and_update_singleton prefix on_target =
  P_split {mode = `Node; prefix; prefix_replacement = None; on_target; on_others = ignore}

let wildcard =
  P_split {mode = `Node; prefix = []; prefix_replacement = None; on_target = ignore; on_others = use}
let root =
  P_split {mode = `Node; prefix = []; prefix_replacement = None; on_target = use; on_others = ignore}
let only p = select_and_update_singleton p use
let only_subtree p = select_and_update_subtree p use

let update_subtree prefix on_target =
  P_split {mode = `Subtree; prefix; prefix_replacement = None; on_target; on_others = id}
let update_singleton prefix on_target =
  P_split {mode = `Node; prefix; prefix_replacement = None; on_target; on_others = id}

let except p = update_singleton p hide
let except_subtree p = update_subtree p hide
let in_subtree prefix = update_subtree prefix

let rename_and_update_subtree prefix prefix_replacement on_target =
  P_split {mode = `Subtree; prefix; prefix_replacement = Some prefix_replacement; on_target; on_others = id}
let rename_and_update_singleton prefix prefix_replacement on_target =
  P_split {mode = `Node; prefix; prefix_replacement = Some prefix_replacement; on_target; on_others = id}

let renaming p p' = rename_and_update_singleton p p' use
let renaming_subtree p p' = rename_and_update_subtree p p' use

let seq pats = P_seq pats

let custom f = P_act (A_custom f)

let union l = P_union l

let equal_act equal_custom a1 a2 =
  match a1, a2 with
  | A_switch s1, A_switch s2 -> s1 = s2
  | A_custom f1, A_custom f2 -> equal_custom f1 f2
  | _ -> false

let rec equal equal_custom p1 p2 =
  match p1, p2 with
  | P_act a1, P_act a2 -> equal_act equal_custom a1 a2
  | P_split s1, P_split s2 ->
    s1.mode = s2.mode &&
    s1.prefix = s2.prefix &&
    s1.prefix_replacement = s2.prefix_replacement &&
    equal equal_custom s1.on_target s2.on_target &&
    equal equal_custom s1.on_others s2.on_others
  | P_seq ps1, P_seq ps2 | P_union ps1, P_union ps2 ->
    begin try List.for_all2 (equal equal_custom) ps1 ps2 with Invalid_argument _ -> false end
  | _ -> false

let pp_path fmt path =
  Format.pp_print_string fmt @@ String.concat "." path

let pp_split_mode fmt =
  function
  | `Subtree -> Format.pp_print_string fmt "'subtree"
  | `Node -> Format.pp_print_string fmt "'node"

let pp_prefix fmt =
  function
  | (prefix, None) -> pp_path fmt prefix
  | (prefix, Some replacement) ->
    Format.fprintf fmt "@[<hov 1>(=>@ %a@ %a)@]" pp_path prefix pp_path replacement

let rec pp_patterns pp_custom fmt =
  List.iter (Format.fprintf fmt "@ %a" (pp pp_custom))

and pp pp_custom fmt =
  function
  | P_act act -> Format.fprintf fmt "@[<hov 1>(act@ %a)@]" (pp_act pp_custom) act
  | P_split {mode; prefix; prefix_replacement; on_target; on_others} ->
    Format.fprintf fmt "@[<hov 1>(split@ %a@ %a@ %a %a)@]"
      pp_split_mode mode
      pp_prefix (prefix, prefix_replacement)
      (pp pp_custom) on_target
      (pp pp_custom) on_others
  | P_seq ps ->
    Format.fprintf fmt "@[<hov 1>(seq%a)@]" (pp_patterns pp_custom) ps
  | P_union ps ->
    Format.fprintf fmt "@[<hov 1>(union%a)@]" (pp_patterns pp_custom) ps
