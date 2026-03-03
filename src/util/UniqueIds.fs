module UniqueIds

type Counter = int

let makeTemporary (counter: Counter) : Counter * string =
    (counter + 1, "tmp." + string counter)

let makeLabel (prefix: string) (counter: Counter) : Counter * string =
    (counter + 1, prefix + "." + string counter)

let makeNamedTemporary = makeLabel

let initialCounter : Counter = 0

// Shared mutable counter for modules that haven't been fully converted to threading yet
let mutable sharedCounter : Counter = 0

let makeTemporaryShared () =
    let c, name = makeTemporary sharedCounter
    sharedCounter <- c
    name

let makeLabelShared prefix =
    let c, name = makeLabel prefix sharedCounter
    sharedCounter <- c
    name

let makeNamedTemporaryShared = makeLabelShared
