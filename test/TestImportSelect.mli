open Yuujinchou

module DataSet : Set.S with type elt = int

type env = int Trie.t

val import : env -> int Pattern.t -> env -> env
val select : env -> int Pattern.t -> DataSet.t