module Types

type t =
    | Char
    | SChar
    | UChar
    | Int
    | Long
    | UInt
    | ULong
    | Double
    | Pointer of t
    | Void
    | Array of elemType: t * size: int64
    | FunType of paramTypes: t list * retType: t
    | Structure of string (* tag *)

let rec show (typ: t) : string =
    match typ with
    | Char -> "Char"
    | SChar -> "SChar"
    | UChar -> "UChar"
    | Int -> "Int"
    | Long -> "Long"
    | UInt -> "UInt"
    | ULong -> "ULong"
    | Double -> "Double"
    | Pointer inner -> sprintf "%s*" (show inner)
    | Void -> "Void"
    | Array(elemType, size) ->
        sprintf "(%s, %s)" (show elemType) (string size)
    | FunType(paramTypes, retType) ->
        let paramStr =
            paramTypes
            |> List.map show
            |> String.concat "; "
        sprintf "(FunType (param_types = [%s], ret_type = %s))"
            paramStr
            (show retType)
    | Structure tag -> sprintf "(Structure %s)" tag

let pp (out: System.IO.TextWriter) (t: t) = out.Write(show t)