module Assembly

type reg =
    | AX
    | BX
    | CX
    | DX
    | DI
    | SI
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    | SP
    | BP
    | XMM0
    | XMM1
    | XMM2
    | XMM3
    | XMM4
    | XMM5
    | XMM6
    | XMM7
    | XMM8
    | XMM9
    | XMM10
    | XMM11
    | XMM12
    | XMM13
    | XMM14
    | XMM15

type indexed_operand = { ``base``: reg; index: reg; scale: int }

type operand =
    | Imm of int64
    | Reg of reg
    | Pseudo of string
    | Memory of reg * int
    | Data of string * int
    | PseudoMem of string * int
    | Indexed of indexed_operand

type unary_operator = Neg | Not | Shr

type binary_operator =
    | Add
    | Sub
    | Mult
    | DivDouble
    | And
    | Or
    | Xor
    | Shl
    | ShrBinop

type cond_code = E | NE | G | GE | L | LE | A | AE | B | BE

type byte_array_info = { size: int; alignment: int }

type asm_type =
    | Byte
    | Longword
    | Quadword
    | Double
    | ByteArray of byte_array_info

type movsx_info = {
    src_type: asm_type
    dst_type: asm_type
    src: operand
    dst: operand
}

type mov_zero_extend_info = {
    src_type: asm_type
    dst_type: asm_type
    src: operand
    dst: operand
}

type binary_info = {
    op: binary_operator
    t: asm_type
    src: operand
    dst: operand
}

type instruction =
    | Mov of asm_type * operand * operand
    | Movsx of movsx_info
    | MovZeroExtend of mov_zero_extend_info
    | Lea of operand * operand
    | Cvttsd2si of asm_type * operand * operand
    | Cvtsi2sd of asm_type * operand * operand
    | Unary of unary_operator * asm_type * operand
    | Binary of binary_info
    | Cmp of asm_type * operand * operand
    | Idiv of asm_type * operand
    | Div of asm_type * operand
    | Cdq of asm_type
    | Jmp of string
    | JmpCC of cond_code * string
    | SetCC of cond_code * operand
    | Label of string
    | Push of operand
    | Pop of reg
    | Call of string
    | Ret

type function_def = {
    name: string
    ``global``: bool
    instructions: instruction list
}

type static_variable_def = {
    name: string
    alignment: int
    ``global``: bool
    init: Initializers.static_init list
}

type static_constant_def = {
    name: string
    alignment: int
    init: Initializers.static_init
}

type top_level =
    | Function of function_def
    | StaticVariable of static_variable_def
    | StaticConstant of static_constant_def

type t = Program of top_level list