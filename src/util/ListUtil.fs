module ListUtil
    let tryMax cmp l =
        match l with
        | [] -> None
        | _ -> l |> List.sortWith cmp |> List.rev |> Some |> Option.map List.head

    let max cmp l =
        tryMax cmp l |> Option.get

    let tryMin cmp l =
        match l with
        | [] -> None
        | _ -> l |> List.sortWith cmp |> Some |> Option.map List.head

    let min cmp l =
        tryMin cmp l |> Option.get

    let makeList len v = List.init len (fun _ -> v)

    let tryLast l =
        match l with
        | [] -> None
        | _ -> l |> List.rev |> List.head |> Some

    let last l =
        tryLast l |> Option.get

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
