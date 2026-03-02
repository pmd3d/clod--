module UniqueIds

let counter = ref 0

let makeTemporary () =
    let n = !counter
    counter := n + 1
    "tmp." + string n

let makeLabel prefix =
    let n = !counter
    counter := n + 1
    prefix + "." + string n

let makeNamedTemporary = makeLabel