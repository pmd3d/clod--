module ListUtil
    let max cmp l =
        l |> List.sortWith cmp |> List.rev |> List.head

    let min cmp l =
        l |> List.sortWith cmp |> List.head

    let makeList len v = List.init len (fun _ -> v)

    let last l = l |> List.rev |> List.head

    let rec take n l =
        match l with
        | [] -> []
        | _ :: _ when n <= 0 -> []
        | h :: t -> h :: take (n - 1) t

    let rec takeDrop n l =
        match l with
        | h :: t when n > 0 ->
            let l1, l2 = takeDrop (n - 1) t
            (h :: l1, l2)
        | l -> ([], l)
