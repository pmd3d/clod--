module ResultCE

type ResultBuilder() =
    member _.Return(x) = Ok x
    member _.ReturnFrom(m) = m
    member _.Bind(m, f) =
        match m with
        | Ok x -> f x
        | Error e -> Error e
    member _.Zero() = Ok ()
    member _.Combine(m1, m2) =
        match m1 with
        | Ok () -> m2 ()
        | Error e -> Error e
    member _.Delay(f) = f
    member _.Run(f) = f ()

let result = ResultBuilder()

let resultTraverse f items =
    List.foldBack (fun item acc ->
        match acc with
        | Error e -> Error e
        | Ok xs ->
            match f item with
            | Ok x -> Ok (x :: xs)
            | Error e -> Error e) items (Ok [])

let resultFold f state items =
    List.fold (fun acc item ->
        match acc with
        | Error e -> Error e
        | Ok s -> f s item) (Ok state) items
