type path = string list

val pp_path : Format.formatter -> path -> unit

type switch = [`Use | `Hide]

type 'hook act =
  | A_switch of switch
  | A_hook of 'hook

type 'hook split =
  { mode : [`Subtree | `Node]
  ; prefix : path
  ; prefix_replacement : path option
  ; on_target : 'hook t
  ; on_others : 'hook t
  }

and 'hook t =
  | P_act of 'hook act
  | P_split of 'hook split
  | P_seq of 'hook t list
  | P_union of 'hook t list

val equal : ('hook -> 'hook -> bool) -> 'hook t -> 'hook t -> bool

val any : 'hook t
val root : 'hook t
val wildcard : 'hook t

val only : path -> 'hook t
val only_subtree : path -> 'hook t

val none : 'hook t
val except : path -> 'hook t
val except_subtree : path -> 'hook t
val in_subtree : path -> 'hook t -> 'hook t

val renaming : path -> path -> 'hook t
val renaming_subtree : path -> path -> 'hook t

val seq : 'hook t list -> 'hook t

val union : 'hook t list -> 'hook t

val hook : 'hook -> 'hook t
