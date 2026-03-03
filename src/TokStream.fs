module TokStream

type TokStream = Tokens.Token Stream.Stream

exception End_of_stream

let takeToken (tokens: TokStream) =
    try Stream.next tokens
    with Stream.Failure -> raise End_of_stream

let peek (tokens: TokStream) =
    match Stream.peek tokens with
    | Some t -> t
    | None -> raise End_of_stream

let npeek n (tokens: TokStream) = Stream.npeek n tokens

let isEmpty (tokens: TokStream) = Stream.isEmpty tokens

let ofList = Stream.ofList
