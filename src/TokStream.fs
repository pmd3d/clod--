module TokStream

type TokStream = Tokens.Token Stream.Stream

exception End_of_stream

let takeToken (tokens: TokStream) =
    try Stream.next tokens
    with Stream.Failure -> raise End_of_stream

let peek (tokens: TokStream) =
    match Stream.peek tokens with
    (* non-empty stream *)
    | Some t -> t
    (* empty stream - raise an exception, we'll catch it at top level *)
    | None -> raise End_of_stream

let npeek = Stream.npeek

let isEmpty (tokens: TokStream) =
    try
        Stream.empty tokens
        true
    with Stream.Failure -> false

let ofList = Stream.ofList
