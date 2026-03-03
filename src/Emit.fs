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

let alignDirective platform =
    match platform with
    | Settings.OS_X -> ".balign"
    | Settings.Linux -> ".align"

let showLabel platform name =
    match platform with
    | Settings.OS_X -> "_" + name
    | Settings.Linux -> name

let showLocalLabel platform label =
    match platform with
    | Settings.OS_X -> "L" + label
    | Settings.Linux -> ".L" + label

let showFunName platform f =
    match platform with
    | Settings.OS_X -> "_" + f
    | Settings.Linux ->
        if AssemblySymbols.isDefined f then f else f + "@PLT"

let showLongReg = function
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

let showQuadwordReg = function
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

let showDoubleReg = function
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

let showByteReg = function
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

let showOperand platform t = function
    | Reg r ->
        (match t with
        | Byte -> showByteReg r
        | Longword -> showLongReg r
        | Quadword -> showQuadwordReg r
        | Double -> showDoubleReg r
        | ByteArray _ ->
            failwith
                "Internal error: can't store non-scalar operand in register")
    | Imm i -> sprintf "$%s" (string i)
    | Memory(r, 0) -> sprintf "(%s)" (showQuadwordReg r)
    | Memory(r, i) -> sprintf "%d(%s)" i (showQuadwordReg r)
    | Data(name, offset) ->
        let lbl =
            if AssemblySymbols.isConstant name then
                showLocalLabel platform name
            else showLabel platform name
        if offset = 0 then sprintf "%s(%%rip)" lbl
        else sprintf "%s+%d(%%rip)" lbl offset
    | Indexed { ``base`` = b; index = index; scale = scale } ->
        sprintf "(%s, %s, %d)" (showQuadwordReg b)
            (showQuadwordReg index) scale
    (* printing out pseudoregisters is only for debugging *)
    | Pseudo name -> sprintf "%%%s" name
    | PseudoMem(name, offset) -> sprintf "%d(%%%s)" offset name

let showByteOperand platform = function
    | Reg r -> showByteReg r
    | other -> showOperand platform Longword other

let showUnaryInstruction = function
    | Neg -> "neg"
    | Not -> "not"
    | Shr -> "shr"

let showBinaryInstruction = function
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

let showCondCode = function
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

let emitInstruction platform (chan: System.IO.TextWriter) = function
    | Mov(t, src, dst) ->
        chan.Write(
            sprintf "\tmov%s %s, %s\n" (suffix t) (showOperand platform t src)
                (showOperand platform t dst))
    | Unary(operator, t, dst) ->
        chan.Write(
            sprintf "\t%s%s %s\n"
                (showUnaryInstruction operator)
                (suffix t) (showOperand platform t dst))
    | Binary { op = Xor; t = Double; src = src; dst = dst } ->
        chan.Write(
            sprintf "\txorpd %s, %s\n" (showOperand platform Double src)
                (showOperand platform Double dst))
    | Binary { op = Mult; t = Double; src = src; dst = dst } ->
        chan.Write(
            sprintf "\tmulsd %s, %s\n" (showOperand platform Double src)
                (showOperand platform Double dst))
    | Binary { op = op; t = t; src = src; dst = dst } ->
        chan.Write(
            sprintf "\t%s%s %s, %s\n"
                (showBinaryInstruction op)
                (suffix t) (showOperand platform t src) (showOperand platform t dst))
    | Cmp(Double, src, dst) ->
        chan.Write(
            sprintf "\tcomisd %s, %s\n" (showOperand platform Double src)
                (showOperand platform Double dst))
    | Cmp(t, src, dst) ->
        chan.Write(
            sprintf "\tcmp%s %s, %s\n" (suffix t) (showOperand platform t src)
                (showOperand platform t dst))
    | Idiv(t, operand) ->
        chan.Write(
            sprintf "\tidiv%s %s\n" (suffix t) (showOperand platform t operand))
    | Div(t, operand) ->
        chan.Write(
            sprintf "\tdiv%s %s\n" (suffix t) (showOperand platform t operand))
    | Lea(src, dst) ->
        chan.Write(
            sprintf "\tleaq %s, %s\n"
                (showOperand platform Quadword src)
                (showOperand platform Quadword dst))
    | Cdq Longword -> chan.Write("\tcdq\n")
    | Cdq Quadword -> chan.Write("\tcqo\n")
    | Jmp lbl ->
        chan.Write(sprintf "\tjmp %s\n" (showLocalLabel platform lbl))
    | JmpCC(code, lbl) ->
        chan.Write(
            sprintf "\tj%s %s\n" (showCondCode code)
                (showLocalLabel platform lbl))
    | SetCC(code, operand) ->
        chan.Write(
            sprintf "\tset%s %s\n" (showCondCode code)
                (showByteOperand platform operand))
    | Label lbl ->
        chan.Write(sprintf "%s:\n" (showLocalLabel platform lbl))
    | Push op ->
        chan.Write(
            sprintf "\tpushq %s\n" (showOperand platform Quadword op))
    | Pop r ->
        chan.Write(sprintf "\tpopq %s\n" (showQuadwordReg r))
    | Call f ->
        chan.Write(sprintf "\tcall %s\n" (showFunName platform f))
    | Movsx { src_type = src_type; dst_type = dst_type; src = src;
              dst = dst } ->
        chan.Write(
            sprintf "\tmovs%s%s %s, %s\n" (suffix src_type)
                (suffix dst_type)
                (showOperand platform src_type src)
                (showOperand platform dst_type dst))
    | MovZeroExtend { src_type = src_type; dst_type = dst_type;
                      src = src; dst = dst } ->
        chan.Write(
            sprintf "\tmovz%s%s %s, %s\n" (suffix src_type)
                (suffix dst_type)
                (showOperand platform src_type src)
                (showOperand platform dst_type dst))
    | Cvtsi2sd(t, src, dst) ->
        chan.Write(
            sprintf "\tcvtsi2sd%s %s, %s\n" (suffix t)
                (showOperand platform t src) (showOperand platform Double dst))
    | Cvttsd2si(t, src, dst) ->
        chan.Write(
            sprintf "\tcvttsd2si%s %s, %s\n" (suffix t)
                (showOperand platform Double src) (showOperand platform t dst))
    | Ret ->
        chan.Write(
            "\n\tmovq %rbp, %rsp\n\tpopq %rbp\n\tret\n")
    | Cdq(Double | Byte | ByteArray _) ->
        failwith
            "Internal error: can't apply cdq to a byte or non-integer type"

let emitGlobalDirective (chan: System.IO.TextWriter) ``global`` label =
    if ``global`` then chan.Write(sprintf "\t.globl %s\n" label)

let escape s =
    let escapeChar c =
        if StringUtil.isAlnum c then string c
        (* use octal escape for everything except alphanumeric values
         * make sure to pad out octal escapes to 3 digits so we don't, e.g.
         * escape "hello 1" as "hello\401" *)
        else sprintf "\\%03o" (int c)
    String.concat ""
        (s |> Seq.map escapeChar |> Seq.toList)

let emitInit platform (chan: System.IO.TextWriter) = function
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
        chan.Write(sprintf "\t.quad %s\n" (showLocalLabel platform lbl))

let emitConstant platform (chan: System.IO.TextWriter) name alignment init =
    let constantSectionName =
        match (platform, init) with
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
            constantSectionName (alignDirective platform) alignment
            (showLocalLabel platform name))
    emitInit platform chan init
    (* macOS linker gets cranky if you write only 8 bytes to .literal16 section *)
    if constantSectionName = ".literal16" then
        emitInit platform chan (Initializers.LongInit 0L)

let emitTl platform (chan: System.IO.TextWriter) = function
    | Function { name = name; ``global`` = isGlobal;
                 instructions = instructions } ->
        let label = showLabel platform name
        emitGlobalDirective chan isGlobal label
        chan.Write(
            sprintf "\n\t.text\n%s:\n\tpushq %%rbp\n\tmovq %%rsp, %%rbp\n"
                label)
        List.iter (emitInstruction platform chan) instructions
    | StaticVariable { name = name; ``global`` = isGlobal; init = init;
                       alignment = alignment }
        when List.forall Initializers.isZero init ->
        let label = showLabel platform name
        emitGlobalDirective chan isGlobal label
        chan.Write(
            sprintf "\n\t.bss\n\t%s %d\n%s:\n"
                (alignDirective platform) alignment label)
        List.iter (emitInit platform chan) init
    | StaticVariable { name = name; ``global`` = isGlobal; init = init;
                       alignment = alignment } ->
        let label = showLabel platform name
        emitGlobalDirective chan isGlobal label
        chan.Write(
            sprintf "\n\t.data\n\t%s %d\n%s:\n"
                (alignDirective platform) alignment label)
        List.iter (emitInit platform chan) init
    | StaticConstant { name = name; alignment = alignment; init = init } ->
        emitConstant platform chan name alignment init

let emitStackNote platform (chan: System.IO.TextWriter) =
    match platform with
    | Settings.OS_X -> ()
    | Settings.Linux ->
        chan.Write("\t.section .note.GNU-stack,\"\",@progbits\n")

let emitToString platform (Program tls) =
    use sw = new System.IO.StringWriter()
    List.iter (emitTl platform sw) tls
    emitStackNote platform sw
    sw.ToString()

let emit platform assembly_file program =
    let content = emitToString platform program
    System.IO.File.WriteAllText(assembly_file, content)
