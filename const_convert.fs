module ConstConvert

module C = Const
module T = Types
module Ast = Ast.Typed

(** Convert constant to an int64. If constant is smaller than int64 it will be
    zero- or sign-extended to preserve value; if it's the same size we preserve
    its representation. *)
let const_to_int64 = function
    | C.ConstChar c -> int64 c
    | C.ConstUChar uc -> int64 uc
    | C.ConstInt i -> int64 i
    | C.ConstUInt ui -> int64 ui
    | C.ConstLong l -> l
    | C.ConstULong ul -> int64 ul
    | C.ConstDouble d -> int64 d

(** Convert int64 to a constant. Preserve the value if possible and wrap modulo
    the size of the target type otherwise. *)
let const_of_int64 v = function
    | T.Char | T.SChar -> C.ConstChar(sbyte v)
    | T.UChar -> C.ConstUChar(byte v)
    | T.Int -> C.ConstInt(int32 v)
    | T.Long -> C.ConstLong v
    | T.UInt -> C.ConstUInt(uint32 v)
    | T.ULong | T.Pointer _ -> C.ConstULong(uint64 v)
    | T.Double -> C.ConstDouble(float v)
    | (T.FunType _ | T.Array _ | T.Void | T.Structure _) as t ->
        failwith
            ("Internal error: can't convert constant to non_scalar type "
             + Types.show t)

let const_convert target_type c =
    if C.type_of_const c = target_type then c
    else
        match (target_type, c) with
        (* Convert to/from double directly to avoid precision loss
           going through the int64 roundtrip *)
        | T.Double, _ ->
            C.ConstDouble(float (const_to_int64 c))
        | T.ULong, C.ConstDouble d ->
            C.ConstULong(uint64 d)
        | _, C.ConstDouble d ->
            const_of_int64 (int64 d) target_type
        | _ ->
            (* Convert c to int64, then to target type, to avoid exponential
               explosion of different cases. Conversion to int64 preserves value
               (except when converting from out-of-range ulong, where it preserves
               representation). Conversion from int64 to const wraps modulo const
               size. *)
            let as_int64 = const_to_int64 c
            const_of_int64 as_int64 target_type