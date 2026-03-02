module DisjointSets

type DisjointSet<'a when 'a: comparison> = Map<'a, 'a>

let init<'a when 'a: comparison> : DisjointSet<'a> = Map.empty

let union (x: 'a) (y: 'a) (disjSets: DisjointSet<'a>) : DisjointSet<'a> =
    Map.add x y disjSets

let rec find (x: 'a) (disjSets: DisjointSet<'a>) : 'a =
    if Map.containsKey x disjSets then
        let mappedTo = Map.find x disjSets
        find mappedTo disjSets
    else
        x

let isEmpty (disjSets: DisjointSet<'a>) : bool = Map.isEmpty disjSets