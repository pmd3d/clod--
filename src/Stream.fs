module Stream

exception Failure

type 'a Stream = { mutable Items: 'a list }

let next (s: 'a Stream) =
    match s.Items with
    | x :: rest ->
        s.Items <- rest
        x
    | [] -> raise Failure

let peek (s: 'a Stream) =
    match s.Items with
    | x :: _ -> Some x
    | [] -> None

let npeek n (s: 'a Stream) =
    s.Items |> List.truncate n

let empty (s: 'a Stream) =
    match s.Items with
    | [] -> ()
    | _ -> raise Failure

let ofList (lst: 'a list) : 'a Stream = { Items = lst }