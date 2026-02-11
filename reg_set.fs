module Reg_set

type t = Set<Assembly.reg>

let empty: t = Set.empty
let singleton x: t = Set.singleton x
let add x (s: t): t = Set.add x s
let remove x (s: t): t = Set.remove x s
let mem x (s: t) = Set.contains x s
let union (s1: t) (s2: t): t = Set.union s1 s2
let inter (s1: t) (s2: t): t = Set.intersect s1 s2
let diff (s1: t) (s2: t): t = Set.difference s1 s2
let is_empty (s: t) = Set.isEmpty s
let cardinal (s: t) = Set.count s
let iter f (s: t) = Set.iter f s
let fold f (s: t) acc = Set.fold f acc s
let map f (s: t): t = Set.map f s
let filter f (s: t): t = Set.filter f s
let exists f (s: t) = Set.exists f s
let for_all f (s: t) = Set.forall f s
let elements (s: t) = Set.toList s
let of_list (l: Assembly.reg list): t = Set.ofList l
let subset (s1: t) (s2: t) = Set.isSubset s1 s2
let choose (s: t) = Set.minElement s