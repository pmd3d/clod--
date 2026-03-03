module Stream

exception Failure

type 'a Stream = { Items: 'a list }

let next (s: 'a Stream) =
    match s.Items with
    | x :: rest -> (x, { Items = rest })
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

let isEmpty (s: 'a Stream) =
    s.Items = []

let ofList (lst: 'a list) : 'a Stream = { Items = lst }
