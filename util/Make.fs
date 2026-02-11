module Disjoint_sets

type Make<'T when 'T : comparison>() =
    
    // type t = Map<'T, 'T>
    // type elt = 'T

    static member init = Map.empty<'T, 'T>

    static member union x y disj_sets = 
        Map.add x y disj_sets

    static member rec find x disj_sets =
        if Map.containsKey x disj_sets then
            let mapped_to = Map.find x disj_sets
            Make<'T>.find mapped_to disj_sets
        else
            x

    static member is_empty disj_sets = 
        Map.isEmpty disj_sets