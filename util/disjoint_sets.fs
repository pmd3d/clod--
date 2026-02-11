module DisjointSets

type t<'a when 'a: comparison> = Map<'a, 'a>

let init<'a when 'a: comparison> : t<'a> = Map.empty

let union (x: 'a) (y: 'a) (disjSets: t<'a>) : t<'a> =
    Map.add x y disjSets

let rec find (x: 'a) (disjSets: t<'a>) : 'a =
    if Map.containsKey x disjSets then
        let mappedTo = Map.find x disjSets
        find mappedTo disjSets
    else
        x

let isEmpty (disjSets: t<'a>) : bool = Map.isEmpty disjSets