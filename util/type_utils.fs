module TypeUtils

open Types

let getType (e: Ast.TypedExp.Expression) = e.t
let setType e newType : Ast.Typed.Expression = { e = e; t = newType }

let rec getSize =
    function
    | Char | SChar | UChar -> 1
    | Int | UInt -> 4
    | Long | ULong | Double | Pointer _ -> 8
    | Array(elemType, size) -> size * getSize elemType
    | Structure tag -> (TypeTable.find tag).size
    | (FunType _ | Void) as t ->
        failwith
            ("Internal error: type doesn't have size: " + show t)

let rec getAlignment =
    function
    | Char | SChar | UChar -> 1
    | Int | UInt -> 4
    | Long | ULong | Double | Pointer _ -> 8
    | Array(elemType, _) -> getAlignment elemType
    | Structure tag -> (TypeTable.find tag).alignment
    | (FunType _ | Void) as t ->
        failwith
            ("Internal error: type doesn't have alignment: " + show t)

let isSigned =
    function
    | Int | Long | Char | SChar -> true
    | UInt | ULong | Pointer _ | UChar -> false
    | (Double | FunType _ | Array _ | Void | Structure _) as t ->
        failwith
            ("Internal error: signedness doesn't make sense for non-integral type "
             + show t)

let isPointer = function Pointer _ -> true | _ -> false

let isInteger =
    function
    | Char | UChar | SChar | Int | UInt | Long | ULong -> true
    | Double | Array _ | Pointer _ | FunType _ | Void | Structure _ -> false

let isArray = function Array _ -> true | _ -> false
let isCharacter = function Char | SChar | UChar -> true | _ -> false

let isArithmetic =
    function
    | Int | UInt | Long | ULong | Char | UChar | SChar | Double -> true
    | FunType _ | Pointer _ | Array _ | Void | Structure _ -> false

let isScalar =
    function
    | Array _ | Void | FunType _ | Structure _ -> false
    | Int | UInt | Long | ULong | Char | UChar | SChar | Double | Pointer _ ->
        true

let isComplete =
    function
    | Void -> false
    | Structure tag -> TypeTable.mem tag
    | _ -> true

let isCompletePointer = function Pointer t -> isComplete t | _ -> false