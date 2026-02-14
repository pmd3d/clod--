module Stream

exception Failure

type 'a t = { mutable Items: 'a list }

let next (s: 'a t) =
    match s.Items with
    | x :: rest ->
        s.Items <- rest
        x
    | [] -> raise Failure

let peek (s: 'a t) =
    match s.Items with
    | x :: _ -> Some x
    | [] -> None

let npeek n (s: 'a t) =
    s.Items |> List.truncate n

let empty (s: 'a t) =
    match s.Items with
    | [] -> ()
    | _ -> raise Failure

let ofList (lst: 'a list) : 'a t = { Items = lst }