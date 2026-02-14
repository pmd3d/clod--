module Int8

(* Represent signed chars internally with the int32 type *)
type t = int32

let equal (a: t) (b: t) = a = b
let compare (a: t) (b: t) = compare a b

let show (i8: t) = i8.ToString()

let zero: t = 0

(* internal function to sign-or-zero-extend into upper bytes *)
let resetUpperBytes (x: t) =
    if x &&& 128 = 0 then
        (* result is positive, zero out upper bits *)
        let bitmask = 0x000000ff
        x &&& bitmask
    else
        (* result is negative, set upper bits to 1 *)
        let bitmask = 0xffffff00 |> int32
        x ||| bitmask

let ofInt (i: int) : t = resetUpperBytes (int32 i)
let toInt (x: t) : int = int x
let ofInt64 (i: int64) : t = resetUpperBytes (int32 i)
let toInt64 (x: t) : int64 = int64 x
let toString (x: t) : string = x.ToString()