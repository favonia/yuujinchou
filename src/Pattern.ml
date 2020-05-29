type exportability = [`Public | `Private]

type path = string list

type pattern =
  | PatWildcard
  | PatId of path * path option
  | PatScope of path * path option * pattern
  | PatSeq of pattern list
  | PatNeg of pattern
  | PatExport of exportability * pattern

let wildcard = PatWildcard
let root = PatNeg PatWildcard
let id p = PatId (p, None)
let renaming p r = PatId (p, Some r)
let all = PatId ([], None)
let none = PatNeg (PatId ([], None))
let scope p a = PatScope (p, None, a)
let renaming_scope p r a = PatScope (p, Some r, a)
let seq acts = PatSeq acts
let neg a = PatNeg a
let public a = PatExport (`Public, a)
let private_ a = PatExport (`Private, a)