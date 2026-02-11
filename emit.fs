module Emit

open Assembly

let suffix = function
    | Byte -> "b"
    | Longword -> "l"
    | Quadword -> "q"
    | Double -> "sd"
    | ByteArray _ ->
        failwith
            "Internal error: found instruction w/ non-scalar operand type"

let align_directive =
    match !Settings.platform with
    | OS_X -> ".balign"
    | Linux -> ".align"

let show_label name =
    match !Settings.platform with
    | OS_X -> "_" + name
    | Linux -> name

let show_local_label label =
    match !Settings.platform with
    | OS_X -> "L" + label
    | Linux -> ".L" + label

let show_fun_name f =
    match !Settings.platform with
    | OS_X -> "_" + f
    | Linux ->
        if AssemblySymbols.is_defined f then f else f + "@PLT"

let show_long_reg = function
    | AX -> "%eax"
    | BX -> "%ebx"
    | CX -> "%ecx"
    | DX -> "%edx"
    | DI -> "%edi"
    | SI -> "%esi"
    | R8 -> "%r8d"
    | R9 -> "%r9d"
    | R10 -> "%r10d"
    | R11 -> "%r11d"
    | R12 -> "%r12d"
    | R13 -> "%r13d"
    | R14 -> "%r14d"
    | R15 -> "%r15d"
    | SP -> failwith "Internal error: no 32-bit RSP"
    | BP -> failwith "Internal error: no 32-bit RBP"
    | _ ->
        failwith
            "Internal error: can't store longword type in XMM register"

let show_quadword_reg = function
    | AX -> "%rax"
    | BX -> "%rbx"
    | CX -> "%rcx"
    | DX -> "%rdx"
    | DI -> "%rdi"
    | SI -> "%rsi"
    | R8 -> "%r8"
    | R9 -> "%r9"
    | R10 -> "%r10"
    | R11 -> "%r11"
    | R12 -> "%r12"
    | R13 -> "%r13"
    | R14 -> "%r14"
    | R15 -> "%r15"
    | SP -> "%rsp"
    | BP -> "%rbp"
    | _ ->
        failwith
            "Internal error: can't store quadword type in XMM register"

let show_double_reg = function
    | XMM0 -> "%xmm0"
    | XMM1 -> "%xmm1"
    | XMM2 -> "%xmm2"
    | XMM3 -> "%xmm3"
    | XMM4 -> "%xmm4"
    | XMM5 -> "%xmm5"
    | XMM6 -> "%xmm6"
    | XMM7 -> "%xmm7"
    | XMM8 -> "%xmm8"
    | XMM9 -> "%xmm9"
    | XMM10 -> "%xmm10"
    | XMM11 -> "%xmm11"
    | XMM12 -> "%xmm12"
    | XMM13 -> "%xmm13"
    | XMM14 -> "%xmm14"
    | XMM15 -> "%xmm15"
    | _ ->
        failwith
            "Internal error: can't store double type in general-purpose register"

let show_byte_reg = function
    | AX -> "%al"
    | BX -> "%bl"
    | CX -> "%cl"
    | DX -> "%dl"
    | DI -> "%dil"
    | SI -> "%sil"
    | R8 -> "%r8b"
    | R9 -> "%r9b"
    | R10 -> "%r10b"
    | R11 -> "%r11b"
    | R12 -> "%r12b"
    | R13 -> "%r13b"
    | R14 -> "%r14b"
    | R15 -> "%r15b"
    | SP -> failwith "Internal error: no one-byte RSP"
    | BP -> failwith "Internal error: no one-byte RBP"
    | _ ->
        failwith "Internal error: can't store byte type in XMM register"

let show_operand t = function
    | Reg r ->
        (match t with
        | Byte -> show_byte_reg r
        | Longword -> show_long_reg r
        | Quadword -> show_quadword_reg r
        | Double -> show_double_reg r
        | ByteArray _ ->
            failwith
                "Internal error: can't store non-scalar operand in register")
    | Imm i -> sprintf "$%s" (string i)
    | Memory(r, 0) -> sprintf "(%s)" (show_quadword_reg r)
    | Memory(r, i) -> sprintf "%d(%s)" i (show_quadword_reg r)
    | Data(name, offset) ->
        let lbl =
            if AssemblySymbols.is_constant name then
                show_local_label name
            else show_label name
        if offset = 0 then sprintf "%s(%%rip)" lbl
        else sprintf "%s+%d(%%rip)" lbl offset
    | Indexed { ``base`` = b; index = index; scale = scale } ->
        sprintf "(%s, %s, %d)" (show_quadword_reg b)
            (show_quadword_reg index) scale
    (* printing out pseudoregisters is only for debugging *)
    | Pseudo name -> sprintf "%%%s" name
    | PseudoMem(name, offset) -> sprintf "%d(%%%s)" offset name

let show_byte_operand = function
    | Reg r -> show_byte_reg r
    | other -> show_operand Longword other

let show_unary_instruction = function
    | Neg -> "neg"
    | Not -> "not"
    | Shr -> "shr"

let show_binary_instruction = function
    | Add -> "add"
    | Sub -> "sub"
    | Mult -> "imul"
    | DivDouble -> "div"
    | And -> "and"
    | Or -> "or"
    | Shl -> "shl"
    | ShrBinop -> "shr"
    | Xor ->
        failwith
            "Internal error, should handle xor as special case"

let show_cond_code = function
    | E -> "e"
    | NE -> "ne"
    | G -> "g"
    | GE -> "ge"
    | L -> "l"
    | LE -> "le"
    | A -> "a"
    | AE -> "ae"
    | B -> "b"
    | BE -> "be"

let emit_instruction (chan: System.IO.StreamWriter) = function
    | Mov(t, src, dst) ->
        chan.Write(
            sprintf "\tmov%s %s, %s\n" (suffix t) (show_operand t src)
                (show_operand t dst))
    | Unary(operator, t, dst) ->
        chan.Write(
            sprintf "\t%s%s %s\n"
                (show_unary_instruction operator)
                (suffix t) (show_operand t dst))
    | Binary { op = Xor; t = Double; src = src; dst = dst } ->
        chan.Write(
            sprintf "\txorpd %s, %s\n" (show_operand Double src)
                (show_operand Double dst))
    | Binary { op = Mult; t = Double; src = src; dst = dst } ->
        chan.Write(
            sprintf "\tmulsd %s, %s\n" (show_operand Double src)
                (show_operand Double dst))
    | Binary { op = op; t = t; src = src; dst = dst } ->
        chan.Write(
            sprintf "\t%s%s %s, %s\n"
                (show_binary_instruction op)
                (suffix t) (show_operand t src) (show_operand t dst))
    | Cmp(Double, src, dst) ->
        chan.Write(
            sprintf "\tcomisd %s, %s\n" (show_operand Double src)
                (show_operand Double dst))
    | Cmp(t, src, dst) ->
        chan.Write(
            sprintf "\tcmp%s %s, %s\n" (suffix t) (show_operand t src)
                (show_operand t dst))
    | Idiv(t, operand) ->
        chan.Write(
            sprintf "\tidiv%s %s\n" (suffix t) (show_operand t operand))
    | Div(t, operand) ->
        chan.Write(
            sprintf "\tdiv%s %s\n" (suffix t) (show_operand t operand))
    | Lea(src, dst) ->
        chan.Write(
            sprintf "\tleaq %s, %s\n"
                (show_operand Quadword src)
                (show_operand Quadword dst))
    | Cdq Longword -> chan.Write("\tcdq\n")
    | Cdq Quadword -> chan.Write("\tcqo\n")
    | Jmp lbl ->
        chan.Write(sprintf "\tjmp %s\n" (show_local_label lbl))
    | JmpCC(code, lbl) ->
        chan.Write(
            sprintf "\tj%s %s\n" (show_cond_code code)
                (show_local_label lbl))
    | SetCC(code, operand) ->
        chan.Write(
            sprintf "\tset%s %s\n" (show_cond_code code)
                (show_byte_operand operand))
    | Label lbl ->
        chan.Write(sprintf "%s:\n" (show_local_label lbl))
    | Push op ->
        chan.Write(
            sprintf "\tpushq %s\n" (show_operand Quadword op))
    | Pop r ->
        chan.Write(sprintf "\tpopq %s\n" (show_quadword_reg r))
    | Call f ->
        chan.Write(sprintf "\tcall %s\n" (show_fun_name f))
    | Movsx { src_type = src_type; dst_type = dst_type; src = src;
              dst = dst } ->
        chan.Write(
            sprintf "\tmovs%s%s %s, %s\n" (suffix src_type)
                (suffix dst_type)
                (show_operand src_type src)
                (show_operand dst_type dst))
    | MovZeroExtend { src_type = src_type; dst_type = dst_type;
                      src = src; dst = dst } ->
        chan.Write(
            sprintf "\tmovz%s%s %s, %s\n" (suffix src_type)
                (suffix dst_type)
                (show_operand src_type src)
                (show_operand dst_type dst))
    | Cvtsi2sd(t, src, dst) ->
        chan.Write(
            sprintf "\tcvtsi2sd%s %s, %s\n" (suffix t)
                (show_operand t src) (show_operand Double dst))
    | Cvttsd2si(t, src, dst) ->
        chan.Write(
            sprintf "\tcvttsd2si%s %s, %s\n" (suffix t)
                (show_operand Double src) (show_operand t dst))
    | Ret ->
        chan.Write(
            "\n\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n")
    | Cdq(Double | Byte | ByteArray _) ->
        failwith
            "Internal error: can't apply cdq to a byte or non-integer type"

let emit_global_directive (chan: System.IO.StreamWriter) ``global`` label =
    if ``global`` then chan.Write(sprintf "\t.globl %s\n" label)

let escape s =
    let escape_char c =
        if StringUtil.isAlnum c then string c
        (* use octal escape for everything except alphanumeric values
         * make sure to pad out octal escapes to 3 digits so we don't, e.g.
         * escape "hello 1" as "hello\401" *)
        else sprintf "\\%03o" (int c)
    String.concat ""
        (s |> Seq.map escape_char |> Seq.toList)

let emit_init (chan: System.IO.StreamWriter) = function
    | Initializers.IntInit i ->
        chan.Write(sprintf "\t.long %s\n" (string i))
    | Initializers.LongInit l ->
        chan.Write(sprintf "\t.quad %d\n" l)
    | Initializers.UIntInit u ->
        chan.Write(sprintf "\t.long %s\n" (string u))
    | Initializers.ULongInit l ->
        chan.Write(sprintf "\t.quad %s\n" (string l))
    | Initializers.CharInit c ->
        chan.Write(sprintf "\t.byte %s\n" (string c))
    | Initializers.UCharInit uc ->
        chan.Write(sprintf "\t.byte %s\n" (string uc))
    | Initializers.DoubleInit d ->
        chan.Write(
            sprintf "\t.quad %d\n"
                (System.BitConverter.DoubleToInt64Bits d))
    (* a partly-initialized array can include a mix of zero and non-zero
       initializers *)
    | Initializers.ZeroInit byte_count ->
        chan.Write(sprintf "\t.zero %d\n" byte_count)
    | Initializers.StringInit(s, true) ->
        chan.Write(sprintf "\t.asciz \"%s\"\n" (escape s))
    | Initializers.StringInit(s, false) ->
        chan.Write(sprintf "\t.ascii \"%s\"\n" (escape s))
    | Initializers.PointerInit lbl ->
        chan.Write(sprintf "\t.quad %s\n" (show_local_label lbl))

let emit_constant (chan: System.IO.StreamWriter) name alignment init =
    let constant_section_name =
        match (!Settings.platform, init) with
        | Settings.Linux, _ -> ".section .rodata"
        | Settings.OS_X, Initializers.StringInit _ -> ".cstring"
        | Settings.OS_X, _ ->
            if alignment = 8 then ".literal8"
            else if alignment = 16 then ".literal16"
            else
                failwith
                    "Internal error: found constant with bad alignment"
    chan.Write(
        sprintf "\n\t%s\n\t%s %d\n  %s:\n"
            constant_section_name align_directive alignment
            (show_local_label name))
    emit_init chan init
    (* macOS linker gets cranky if you write only 8 bytes to .literal16 section *)
    if constant_section_name = ".literal16" then
        emit_init chan (Initializers.LongInit 0L)

let emit_tl (chan: System.IO.StreamWriter) = function
    | Function { name = name; ``global`` = isGlobal;
                 instructions = instructions } ->
        let label = show_label name
        emit_global_directive chan isGlobal label
        chan.Write(
            sprintf "\n\t.text\n%s:\n\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n"
                label)
        List.iter (emit_instruction chan) instructions
    | StaticVariable { name = name; ``global`` = isGlobal; init = init;
                       alignment = alignment }
        when List.forall Initializers.is_zero init ->
        let label = show_label name
        emit_global_directive chan isGlobal label
        chan.Write(
            sprintf "\n\t.bss\n\t%s %d\n%s:\n"
                align_directive alignment label)
        List.iter (emit_init chan) init
    | StaticVariable { name = name; ``global`` = isGlobal; init = init;
                       alignment = alignment } ->
        let label = show_label name
        emit_global_directive chan isGlobal label
        chan.Write(
            sprintf "\n\t.data\n\t%s %d\n%s:\n"
                align_directive alignment label)
        List.iter (emit_init chan) init
    | StaticConstant { name = name; alignment = alignment; init = init } ->
        emit_constant chan name alignment init

let emit_stack_note (chan: System.IO.StreamWriter) =
    match !Settings.platform with
    | Settings.OS_X -> ()
    | Settings.Linux ->
        chan.Write("\t.section .note.GNU-stack,\"\",@progbits\n")

let emit assembly_file (Program tls) =
    use output_channel = new System.IO.StreamWriter(assembly_file)
    List.iter (emit_tl output_channel) tls
    emit_stack_note output_channel
    output_channel.Flush()