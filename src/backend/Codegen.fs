module Codegen

open TypeUtils
open Const
open System.Collections.Generic

let intParamPassingRegs = [ Assembly.DI; Assembly.SI; Assembly.DX; Assembly.CX; Assembly.R8; Assembly.R9 ]

let dblParamPassingRegs =
  [ Assembly.XMM0; Assembly.XMM1; Assembly.XMM2; Assembly.XMM3; Assembly.XMM4; Assembly.XMM5; Assembly.XMM6; Assembly.XMM7 ]

let zero = Assembly.Imm 0L
let constants = new Dictionary<int64, string * int>()

let addConstant alignmentOpt dbl =
  let alignment = defaultArg alignmentOpt 8
  let key = System.BitConverter.DoubleToInt64Bits dbl
  (* see if we've defined this double already *)
  if constants.ContainsKey(key) then
    let name, old_alignment = constants.[key]
    (* update alignment to max of current and new *)
    constants.[key] <- (name, max alignment old_alignment)
    name
  else
    (* we haven't defined it yet, add it to the table *)
    let name = UniqueIds.makeLabel "dbl"
    constants.[key] <- (name, alignment)
    name

(* Get the operand type we should use to move an eightbyte of a struct.

   If it contains exactly 8, 4, or 1 bytes, use the corresponding type (note
   that all but the last eightbyte of a struct are exactly 8 bytes). If it's an
   uneven size, use the ByteArray type. *)
let getEightbyteType eightbyte_idx total_var_size =
  let bytes_left = total_var_size - (eightbyte_idx * 8)
  match bytes_left with
  | x when x >= 8 -> Assembly.Quadword (* *)
  | 4 -> Assembly.Longword
  | 1 -> Assembly.Byte
  | x -> Assembly.ByteArray { size = x; alignment = 8 }

let addOffset n = function
  | Assembly.PseudoMem (base_, off) -> Assembly.PseudoMem (base_, off + n)
  | Assembly.Memory (r, off) -> Assembly.Memory (r, off + n)
  (* you could do pointer arithmetic w/ indexed or data operands but we don't
     need to *)
  | Assembly.Imm _ | Assembly.Reg _ | Assembly.Pseudo _ | Assembly.Indexed _ | Assembly.Data _ ->
      failwith
        "Internal error: trying to copy data to or from non-memory operand"

let rec copyBytes src_val dst_val byte_count =
  if byte_count = 0 then []
  else
    let operand_type, operand_size =
      if byte_count < 4 then (Assembly.Byte, 1)
      else if byte_count < 8 then (Assembly.Longword, 4)
      else (Assembly.Quadword, 8)
    
    let next_src = addOffset operand_size src_val
    let next_dst = addOffset operand_size dst_val
    let bytes_left = byte_count - operand_size
    Assembly.Mov (operand_type, src_val, dst_val)
    :: copyBytes next_src next_dst bytes_left

(* copy an uneven, smaller-than-quadword eightbyte from memory into a register:
 * repeatedly copy byte into register and shift left, starting w/ highest byte and working down to lowest *)
let copyBytesToReg src_val dst_reg byte_count =
  let copy_byte i =
    let mv = Assembly.Mov (Assembly.Byte, addOffset i src_val, Assembly.Reg dst_reg) in
    if i = 0 then [ mv ]
    else
      [ mv; Assembly.Binary { op = Assembly.Shl; t = Assembly.Quadword; src = Assembly.Imm 8L; dst = Assembly.Reg dst_reg } ]
  
  (* [0; 1; ... ; byte_count - 1] *)
  let byte_counts = List.init byte_count id |> List.rev

  List.collect copy_byte byte_counts

(* copy an uneven, smaller-than-quadword eightbyte from a register into memory;
 * repeatedly copy byte into register and shift right, starting w/ byte 0  and working up *)
let copyBytesFromReg src_reg dst_val byte_count =
  let copy_byte i =
    let mv = Assembly.Mov (Assembly.Byte, Assembly.Reg src_reg, addOffset i dst_val) in
    if i < byte_count - 1 then
        [
          mv;
          Assembly.Binary
            { op = Assembly.ShrBinop; t = Assembly.Quadword; src = Assembly.Imm 8L; dst = Assembly.Reg src_reg };
        ]
    else [ mv ]
  
  List.concat (List.init byte_count copy_byte)

let convertVal = function
  | Tacky.Constant (Const.ConstChar c) -> Assembly.Imm (int64 c)
  | Tacky.Constant (Const.ConstUChar uc) -> Assembly.Imm (int64 uc)
  | Tacky.Constant (Const.ConstInt i) -> Assembly.Imm (int64 i)
  | Tacky.Constant (Const.ConstLong l) -> Assembly.Imm l
  | Tacky.Constant (Const.ConstUInt u) -> Assembly.Imm (int64 u)
  | Tacky.Constant (Const.ConstULong ul) -> Assembly.Imm (int64 ul)
  | Tacky.Constant (Const.ConstDouble d) -> Assembly.Data (addConstant None d, 0)
  | Tacky.Var v ->
      if TypeUtils.isScalar (Symbols.get v).symType then Assembly.Pseudo v
      else Assembly.PseudoMem (v, 0)

let convertType = function
  | Types.Int | Types.UInt -> Assembly.Longword
  | Types.Long | Types.ULong | Types.Pointer _ -> Assembly.Quadword
  | Types.Char | Types.SChar | Types.UChar -> Assembly.Byte
  | Types.Double -> Assembly.Double
  | (Types.Array _ | Types.Structure _) as t ->
      Assembly.ByteArray
        { size = int (TypeUtils.getSize t); alignment = TypeUtils.getAlignment t }
  | (Types.FunType _ | Types.Void) as t ->
      failwith
        ("Internal error, converting type to assembly: " + Types.show t)

let asmType v = convertType (Tacky.typeOfVal v)

let convertUnop = function
  | Tacky.Complement -> Assembly.Not
  | Tacky.Negate -> Assembly.Neg
  | Tacky.Not ->
      failwith "Internal error, can't convert TACKY not directly to assembly"

let convertBinop = function
  | Tacky.Add -> Assembly.Add
  | Tacky.Subtract -> Assembly.Sub
  | Tacky.Multiply -> Assembly.Mult
  | Tacky.Divide ->
      Assembly.DivDouble (* NB should only be called for operands on doubles *)
  | Tacky.Mod | Tacky.Equal | Tacky.NotEqual | Tacky.GreaterOrEqual | Tacky.LessOrEqual | Tacky.GreaterThan
  | Tacky.LessThan ->
      failwith "Internal error: not a binary assembly instruction"

let convertCondCode signed = function
  | Tacky.Equal -> Assembly.E
  | Tacky.NotEqual -> Assembly.NE
  | Tacky.GreaterThan -> if signed then Assembly.G else Assembly.A
  | Tacky.GreaterOrEqual -> if signed then Assembly.GE else Assembly.AE
  | Tacky.LessThan -> if signed then Assembly.L else Assembly.B
  | Tacky.LessOrEqual -> if signed then Assembly.LE else Assembly.BE
  | _ -> failwith "Internal error: not a condition code"

type ParamClass = Mem | SSE | Integer

let classifyNewStructure tag =
  let { TypeTable.size = size } = TypeTable.find tag
  if size > 16 then
    let eightbyte_count = (size / 8) + if size % 8 = 0 then 0 else 1
    ListUtil.makeList eightbyte_count Mem
  else
    let rec f = function
      | Types.Structure struct_tag ->
          let member_types = TypeTable.getMemberTypes struct_tag
          List.collect f member_types
      | Types.Array (elemType, size) ->
          List.concat (ListUtil.makeList (int size) (f elemType))
      | t -> [ t ]
    
    let scalar_types = f (Types.Structure tag)
    let first, last = (List.head scalar_types, List.last scalar_types)
    if size > 8 then
      let first_class = if first = Types.Double then SSE else Integer
      let last_class = if last = Types.Double then SSE else Integer
      [ first_class; last_class ]
    else if first = Types.Double then [ SSE ]
    else [ Integer ]

(* memoize results of classifyStructure *)
let classifiedStructures = new Dictionary<_,_>()

let classifyStructure tag =
  match classifiedStructures.TryGetValue tag with
  | true, classes -> classes
  | false, _ ->
      let classes = classifyNewStructure tag
      classifiedStructures.Add(tag, classes)
      classes

let classifyParamsHelper typed_asm_vals return_on_stack =
  let int_regs_available = if return_on_stack then 5 else 6
  let process_one_param (int_reg_args, dbl_reg_args, stack_args)
      (tacky_t, operand) =
    let t = convertType tacky_t
    let typed_operand = (t, operand)
    match tacky_t with
    | Types.Structure s ->
        (* it's a structure *)
        let var_name =
          match operand with
          | Assembly.PseudoMem (n, 0) -> n
          | _ -> failwith "Bad structure operand"
        
        let var_size = int (TypeUtils.getSize tacky_t)
        let classes = classifyStructure s
        let updated_int, updated_dbl, use_stack =
          if List.head classes = Mem then
            (* all eightbytes go on the stack*)
            (int_reg_args, dbl_reg_args, true)
          else
            (* tentative assign eigthbytes to registers *)
            let process_one_eightbyte (i, tentative_ints, tentative_dbls) cls =
              let operand = Assembly.PseudoMem (var_name, i * 8)
              match cls with
              | SSE -> (i + 1, tentative_ints, operand :: tentative_dbls)
              | Integer ->
                  let eightbyte_type =
                    getEightbyteType i var_size
                  in
                  ( i + 1,
                    (eightbyte_type, operand) :: tentative_ints,
                    tentative_dbls )
              | Mem ->
                  failwith
                    "Internal error: found eightbyte in Mem class, but first \
                     eighbyte wasn't Mem"
            
            let _, tentative_ints, tentative_dbs =
              List.fold process_one_eightbyte
                (0, int_reg_args, dbl_reg_args)
                classes
            in
            if
              List.length tentative_ints <= int_regs_available
              && List.length tentative_dbs <= 8
            then
              (* assignment to regs succeeded *)
              (tentative_ints, tentative_dbs, false)
            else (int_reg_args, dbl_reg_args, true)
        
        let add_stack_args stk i =
          let eightbyte_type =
            getEightbyteType i var_size
          in
          (eightbyte_type, Assembly.PseudoMem (var_name, i * 8)) :: stk
        
        let updated_stack_args =
          if use_stack then
            (* add each eighbyte of structure to stack_args s*)
            List.fold add_stack_args stack_args
              (List.mapi (fun idx _ -> idx) classes)
          else stack_args
        
        (updated_int, updated_dbl, updated_stack_args)
    | Types.Double ->
        if List.length dbl_reg_args < 8 then
          (int_reg_args, operand :: dbl_reg_args, stack_args)
        else (int_reg_args, dbl_reg_args, typed_operand :: stack_args)
    | _ ->
        if List.length int_reg_args < int_regs_available then
          (typed_operand :: int_reg_args, dbl_reg_args, stack_args)
        else (int_reg_args, dbl_reg_args, typed_operand :: stack_args)
  

  let reversed_int, reversed_dbl, reversed_stack =
    List.fold process_one_param ([], [], []) typed_asm_vals
  in
  (List.rev reversed_int, List.rev reversed_dbl, List.rev reversed_stack)

let classifyParameters params_ return_on_stack =
  let f v = (Tacky.typeOfVal v, convertVal v) in
  classifyParamsHelper (List.map f params_) return_on_stack

let classifyParamTypes type_list return_on_stack =
  let f t =
    if TypeUtils.isScalar t then (t, Assembly.Pseudo "dummy")
    else (t, Assembly.PseudoMem ("dummy", 0))
  in
  let ints, dbls, _ =
    classifyParamsHelper (List.map f type_list) return_on_stack
  in
  let int_regs = ListUtil.take (List.length ints) intParamPassingRegs in
  let dbl_regs = ListUtil.take (List.length dbls) dblParamPassingRegs in
  int_regs @ dbl_regs

let classifyReturnHelper ret_type asm_retval =
  match ret_type with
  | Types.Structure tag ->
      let classes = classifyStructure tag in
      let var_name =
        match asm_retval with
        | Assembly.PseudoMem (n, 0) -> n
        | _ ->
            failwith
              "Internal error: invalid assembly operand for structure type"
      in
      if List.head classes = Mem then ([], [], true)
      else
        (* return in registers, can move everything w/ quadword operands *)
        let process_quadword (i, ints, dbls) cls =
          let operand = Assembly.PseudoMem (var_name, i * 8) in
          match cls with
          | SSE -> (i + 1, ints, dbls @ [ operand ])
          | Integer ->
              let eightbyte_type =
                getEightbyteType i
                  (int (TypeUtils.getSize ret_type))
              in
              (i + 1, ints @ [ (eightbyte_type, operand) ], dbls)
          | Mem ->
              failwith
                "Internal error: found eightbyte in Mem class, but first \
                 eighbyte wasn't Mem"
        
        let _, i, d = List.fold process_quadword (0, [], []) classes in
        (i, d, false)
  | Types.Double -> ([], [ asm_retval ], false)
  | t ->
      let typed_operand = (convertType t, asm_retval) in
      ([ typed_operand ], [], false)

let classifyReturnValue retval =
  classifyReturnHelper (Tacky.typeOfVal retval) (convertVal retval)

let classifyReturnType = function
  | Types.Void -> ([], false)
  | t ->
      let asm_val =
        if TypeUtils.isScalar t then Assembly.Pseudo "dummy"
        else Assembly.PseudoMem ("dummy", 0)
      in
      let ints, dbls, return_on_stack = classifyReturnHelper t asm_val in
      if return_on_stack then ([ Assembly.AX ], true)
      else
        let int_regs = ListUtil.take (List.length ints) [ Assembly.AX; Assembly.DX ] in
        let dbl_regs =
          ListUtil.take (List.length dbls) [ Assembly.XMM0; Assembly.XMM1 ]
        in
        (int_regs @ dbl_regs, false)

let convertFunctionCall f args dst =
  let int_retvals, dbl_retvals, return_on_stack =
    match dst with Some d -> classifyReturnValue d | None -> ([], [], false)
  in
  (* load address of dest into DI *)
  let load_dst_instruction, first_intreg_idx =
    if return_on_stack then
      ([ Assembly.Lea (convertVal (Option.get dst), Assembly.Reg Assembly.DI) ], 1)
    else ([], 0)
  in

  let int_reg_args, dbl_reg_args, stack_args =
    classifyParameters args return_on_stack
  in

  (* adjust stack alignment *)
  let stack_padding = if List.length stack_args % 2 = 0 then 0 else 8
  let alignment_instruction =
    if stack_padding = 0 then []
    else
      [
        Assembly.Binary
          {
            op = Assembly.Sub;
            t = Assembly.Quadword;
            src = Assembly.Imm (int64 stack_padding);
            dst = Assembly.Reg Assembly.SP;
          };
      ]
  in
  let instructions = load_dst_instruction @ alignment_instruction in
  (* pass args in registers *)
  let pass_int_reg_arg idx (arg_t, arg) =
    let r = List.item (idx + first_intreg_idx) intParamPassingRegs in
    match arg_t with
    | Assembly.ByteArray { size = size } ->
        copyBytesToReg arg r size (* copy_thru_redzone arg r size *)
    | _ -> [ Assembly.Mov (arg_t, arg, Assembly.Reg r) ]
  in
  let instructions =
    instructions @ List.concat (List.mapi pass_int_reg_arg int_reg_args)
  in

  (* pass args in registers *)
  let pass_dbl_reg_arg idx arg =
    let r = List.item idx dblParamPassingRegs in
    Assembly.Mov (Assembly.Double, arg, Assembly.Reg r)
  in
  let instructions = instructions @ List.mapi pass_dbl_reg_arg dbl_reg_args in

  (* pass args on the stack*)
  let pass_stack_arg (arg_t, arg) =
    match (arg, arg_t) with
    | (Assembly.Imm _ | Assembly.Reg _), _ | _, (Assembly.Quadword | Assembly.Double) ->
        [ Assembly.Push arg ]
    | _, Assembly.ByteArray { size = size } ->
        Assembly.Binary
          { op = Assembly.Sub; t = Assembly.Quadword; src = Assembly.Imm (int64 8); dst = Assembly.Reg Assembly.SP }
        :: copyBytes arg (Assembly.Memory (Assembly.SP, 0)) size
    | _ ->
        (* copy into a register before pushing *)
        [ Assembly.Mov (arg_t, arg, Assembly.Reg Assembly.AX); Assembly.Push (Assembly.Reg Assembly.AX) ]
  in
  let instructions =
    instructions @ List.concat (List.map pass_stack_arg stack_args |> List.rev)
  in

  (* adjust stack pointer *)
  let instructions = instructions @ [ Assembly.Call f ] in

  (* adjust stack pointer *)
  let bytes_to_remove = (8 * List.length stack_args) + stack_padding in
  let dealloc =
    if bytes_to_remove = 0 then []
    else
      [
        Assembly.Binary
          {
            op = Assembly.Add;
            t = Assembly.Quadword;
            src = Assembly.Imm (int64 bytes_to_remove);
            dst = Assembly.Reg Assembly.SP;
          };
      ]
  in
  let instructions = instructions @ dealloc in

  (* retrieve return value *)
  let int_ret_regs = [ Assembly.AX; Assembly.DX ] in
  let dbl_ret_regs = [ Assembly.XMM0; Assembly.XMM1 ] in
  let retrieve_result =
    match (dst, return_on_stack) with
    | Some _, false ->
        let get_int i (t, op) =
          let r = List.item i int_ret_regs in
          match t with
          | Assembly.ByteArray { size = size } ->
              copyBytesFromReg r op size
          | _ -> [ Assembly.Mov (t, Assembly.Reg r, op) ]
        in
        let get_dbl i op =
          let r = List.item i dbl_ret_regs in
          Assembly.Mov (Assembly.Double, Assembly.Reg r, op)
        in
        List.concat (List.mapi get_int int_retvals)
        @ List.mapi get_dbl dbl_retvals
    | _ -> []
  in
  instructions @ retrieve_result

let convertReturnInstruction = function
  | None -> [ Assembly.Ret ]
  | Some v ->
      let int_retvals, dbl_retvals, return_on_stack = classifyReturnValue v in
      if return_on_stack then
        let byte_count = int (TypeUtils.getSize (Tacky.typeOfVal v)) in
        let get_ptr = Assembly.Mov (Assembly.Quadword, Assembly.Memory (Assembly.BP, -8), Assembly.Reg Assembly.AX) in
        let copy_into_ptr =
          copyBytes (convertVal v) (Assembly.Memory (Assembly.AX, 0)) byte_count
        in
        (get_ptr :: copy_into_ptr) @ [ Assembly.Ret ]
      else
        let return_ints =
          List.concat
            (List.mapi
               (fun i (t, op) ->
                 let dst_reg = List.item i [ Assembly.AX; Assembly.DX ] in
                 match t with
                 | Assembly.ByteArray { size = size } ->
                     copyBytesToReg op dst_reg
                       size (* copy_thru_redzone op dst_reg size *)
                 | _ -> [ Assembly.Mov (t, op, Assembly.Reg dst_reg) ])
               int_retvals)
        in
        let return_dbls =
          List.mapi
            (fun i op -> Assembly.Mov (Assembly.Double, op, Assembly.Reg (List.item i [ Assembly.XMM0; Assembly.XMM1 ])))
            dbl_retvals
        in
        return_ints @ return_dbls @ [ Assembly.Ret ]

let convertInstruction = function
  | Tacky.Copy { src = src; dst = dst } when TypeUtils.isScalar (Tacky.typeOfVal src) ->
      let t = asmType src in
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      [ Assembly.Mov (t, asm_src, asm_dst) ]
  | Tacky.Copy { src = src; dst = dst } ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      let byte_count = int (TypeUtils.getSize (Tacky.typeOfVal src)) in
      copyBytes asm_src asm_dst byte_count
  | Tacky.Return maybe_val -> convertReturnInstruction maybe_val
  | Tacky.Unary { op = Tacky.Not; src = src; dst = dst } ->
      let src_t = asmType src in
      let dst_t = asmType dst in

      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      if src_t = Assembly.Double then
        [
          Assembly.Binary
            { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Reg Assembly.XMM0; dst = Assembly.Reg Assembly.XMM0 };
          Assembly.Cmp (src_t, asm_src, Assembly.Reg Assembly.XMM0);
          Assembly.Mov (dst_t, zero, asm_dst);
          Assembly.SetCC (Assembly.E, asm_dst);
        ]
      else
        [
          Assembly.Cmp (src_t, zero, asm_src);
          Assembly.Mov (dst_t, zero, asm_dst);
          Assembly.SetCC (Assembly.E, asm_dst);
        ]
  | Tacky.Unary { op = Tacky.Negate; src = src; dst = dst } when Tacky.typeOfVal src = Types.Double ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      let negative_zero = addConstant (Some 16) (-0.0) in
      [
        Assembly.Mov (Assembly.Double, asm_src, asm_dst);
        Assembly.Binary
          { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Data (negative_zero, 0); dst = asm_dst };
      ]
  | Tacky.Unary { op = op; src = src; dst = dst } ->
      let t = asmType src in
      let asm_op = convertUnop op in
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      [ Assembly.Mov (t, asm_src, asm_dst); Assembly.Unary (asm_op, t, asm_dst) ]
  | Tacky.Binary { op = op; src1 = src1; src2 = src2; dst = dst } -> (
      let src_t = asmType src1 in
      let dst_t = asmType dst in
      let asm_src1 = convertVal src1 in
      let asm_src2 = convertVal src2 in
      let asm_dst = convertVal dst in
      match op with
      (* Relational operator *)
      | Tacky.Equal | Tacky.NotEqual | Tacky.GreaterThan | Tacky.GreaterOrEqual | Tacky.LessThan | Tacky.LessOrEqual
        ->
          let signed =
            if src_t = Assembly.Double then false
            else TypeUtils.isSigned (Tacky.typeOfVal src1)
          in
          let cond_code = convertCondCode signed op in
          [
            Assembly.Cmp (src_t, asm_src2, asm_src1);
            Assembly.Mov (dst_t, zero, asm_dst);
            Assembly.SetCC (cond_code, asm_dst);
          ]
      (* Division/modulo *)
      | (Tacky.Divide | Tacky.Mod) when src_t <> Assembly.Double ->
          let result_reg = if op = Tacky.Divide then Assembly.AX else Assembly.DX in
          if TypeUtils.isSigned (Tacky.typeOfVal src1) then
            [
              Assembly.Mov (src_t, asm_src1, Assembly.Reg Assembly.AX);
              Assembly.Cdq src_t;
              Assembly.Idiv (src_t, asm_src2);
              Assembly.Mov (src_t, Assembly.Reg result_reg, asm_dst);
            ]
          else
            [
              Assembly.Mov (src_t, asm_src1, Assembly.Reg Assembly.AX);
              Assembly.Mov (src_t, zero, Assembly.Reg Assembly.DX);
              Assembly.Div (src_t, asm_src2);
              Assembly.Mov (src_t, Assembly.Reg result_reg, asm_dst);
            ]
          (* Addition/subtraction/multiplication*)
      | _ ->
          let asm_op = convertBinop op in
          [
            Assembly.Mov (src_t, asm_src1, asm_dst);
            Assembly.Binary { op = asm_op; t = src_t; src = asm_src2; dst = asm_dst };
          ])
  | Tacky.Load loadInfo
    when TypeUtils.isScalar (Tacky.typeOfVal loadInfo.dst) ->
      let asm_src_ptr = convertVal loadInfo.src_ptr in
      let asm_dst = convertVal loadInfo.dst in
      let t = asmType loadInfo.dst in
      [ Assembly.Mov (Assembly.Quadword, asm_src_ptr, Assembly.Reg Assembly.R9); Assembly.Mov (t, Assembly.Memory (Assembly.R9, 0), asm_dst) ]
  | Tacky.Load loadInfo ->
      let asm_src_ptr = convertVal loadInfo.src_ptr in
      let asm_dst = convertVal loadInfo.dst in
      let byte_count = int (TypeUtils.getSize (Tacky.typeOfVal loadInfo.dst)) in
      Assembly.Mov (Assembly.Quadword, asm_src_ptr, Assembly.Reg Assembly.R9)
      :: copyBytes (Assembly.Memory (Assembly.R9, 0)) asm_dst byte_count
  | Tacky.Store storeInfo
    when TypeUtils.isScalar (Tacky.typeOfVal storeInfo.src) ->
      let asm_src = convertVal storeInfo.src in
      let t = asmType storeInfo.src in
      let asm_dst_ptr = convertVal storeInfo.dst_ptr in
      [ Assembly.Mov (Assembly.Quadword, asm_dst_ptr, Assembly.Reg Assembly.R9); Assembly.Mov (t, asm_src, Assembly.Memory (Assembly.R9, 0)) ]
  | Tacky.Store storeInfo ->
      let asm_src = convertVal storeInfo.src in
      let asm_dst_ptr = convertVal storeInfo.dst_ptr in
      let byte_count = int (TypeUtils.getSize (Tacky.typeOfVal storeInfo.src)) in
      Assembly.Mov (Assembly.Quadword, asm_dst_ptr, Assembly.Reg Assembly.R9)
      :: copyBytes asm_src (Assembly.Memory (Assembly.R9, 0)) byte_count
  | Tacky.GetAddress { src = src; dst = dst } ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      [ Assembly.Lea (asm_src, asm_dst) ]
  | Tacky.Jump target -> [ Assembly.Jmp target ]
  | Tacky.JumpIfZero (cond, target) ->
      let t = asmType cond in
      let asm_cond = convertVal cond in
      if t = Assembly.Double then
        [
          Assembly.Binary
            { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Reg Assembly.XMM0; dst = Assembly.Reg Assembly.XMM0 };
          Assembly.Cmp (t, asm_cond, Assembly.Reg Assembly.XMM0);
          Assembly.JmpCC (Assembly.E, target);
        ]
      else [ Assembly.Cmp (t, zero, asm_cond); Assembly.JmpCC (Assembly.E, target) ]
  | Tacky.JumpIfNotZero (cond, target) ->
      let t = asmType cond in
      let asm_cond = convertVal cond in
      if t = Assembly.Double then
        [
          Assembly.Binary
            { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Reg Assembly.XMM0; dst = Assembly.Reg Assembly.XMM0 };
          Assembly.Cmp (t, asm_cond, Assembly.Reg Assembly.XMM0);
          Assembly.JmpCC (Assembly.NE, target);
        ]
      else [ Assembly.Cmp (t, zero, asm_cond); Assembly.JmpCC (Assembly.NE, target) ]
  | Tacky.Label l -> [ Assembly.Label l ]
  | Tacky.FunCall { f = f; args = args; dst = dst } -> convertFunctionCall f args dst
  | Tacky.SignExtend { src = src; dst = dst } ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      [
        Assembly.Movsx
          {
            src_type = asmType src;
            dst_type = asmType dst;
            src = asm_src;
            dst = asm_dst;
          };
      ]
  | Tacky.Truncate { src = src; dst = dst } ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      [ Assembly.Mov (asmType dst, asm_src, asm_dst) ]
  | Tacky.ZeroExtend { src = src; dst = dst } ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      [
        Assembly.MovZeroExtend
          {
            src_type = asmType src;
            dst_type = asmType dst;
            src = asm_src;
            dst = asm_dst;
          };
      ]
  | Tacky.IntToDouble { src = src; dst = dst } ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      let t = asmType src in
      if t = Assembly.Byte then
        [
          Assembly.Movsx
            {
              src_type = Assembly.Byte;
              dst_type = Assembly.Longword;
              src = asm_src;
              dst = Assembly.Reg Assembly.R9;
            };
          Assembly.Cvtsi2sd (Assembly.Longword, Assembly.Reg Assembly.R9, asm_dst);
        ]
      else [ Assembly.Cvtsi2sd (t, asm_src, asm_dst) ]
  | Tacky.DoubleToInt { src = src; dst = dst } ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      let t = asmType dst in
      if t = Assembly.Byte then
        [ Assembly.Cvttsd2si (Assembly.Longword, asm_src, Assembly.Reg Assembly.R9); Assembly.Mov (Assembly.Byte, Assembly.Reg Assembly.R9, asm_dst) ]
      else [ Assembly.Cvttsd2si (t, asm_src, asm_dst) ]
  | Tacky.UIntToDouble { src = src; dst = dst } ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      if Tacky.typeOfVal src = Types.UChar then
        [
          Assembly.MovZeroExtend
            {
              src_type = Assembly.Byte;
              dst_type = Assembly.Longword;
              src = asm_src;
              dst = Assembly.Reg Assembly.R9;
            };
          Assembly.Cvtsi2sd (Assembly.Longword, Assembly.Reg Assembly.R9, asm_dst);
        ]
      else if Tacky.typeOfVal src = Types.UInt then
        [
          Assembly.MovZeroExtend
            {
              src_type = Assembly.Longword;
              dst_type = Assembly.Quadword;
              src = asm_src;
              dst = Assembly.Reg Assembly.R9;
            };
          Assembly.Cvtsi2sd (Assembly.Quadword, Assembly.Reg Assembly.R9, asm_dst);
        ]
      else
        let out_of_bounds = UniqueIds.makeLabel "ulong2dbl.oob" in
        let end_lbl = UniqueIds.makeLabel "ulong2dbl.end" in
        let r1, r2 = (Assembly.Reg Assembly.R8, Assembly.Reg Assembly.R9) in
        [
          (* check whether asm_src is w/in range of long *)
          Assembly.Cmp (Assembly.Quadword, zero, asm_src);
          Assembly.JmpCC (Assembly.L, out_of_bounds);
          (* it's in range, just use normal cvtsi2sd then jump to end *)
          Assembly.Cvtsi2sd (Assembly.Quadword, asm_src, asm_dst);
          Assembly.Jmp end_lbl;
          (* it's out of bounds *)
          Assembly.Label out_of_bounds;
          (* halve source and round to dd*)
          Assembly.Mov (Assembly.Quadword, asm_src, r1);
          Assembly.Mov (Assembly.Quadword, r1, r2);
          Assembly.Unary (Assembly.Shr, Assembly.Quadword, r2);
          Assembly.Binary { op = Assembly.And; t = Assembly.Quadword; src = Assembly.Imm 1L; dst = r1 };
          Assembly.Binary { op = Assembly.Or; t = Assembly.Quadword; src = r1; dst = r2 };
          (* convert to double, then double it *)
          Assembly.Cvtsi2sd (Assembly.Quadword, r2, asm_dst);
          Assembly.Binary { op = Assembly.Add; t = Assembly.Double; src = asm_dst; dst = asm_dst };
          Assembly.Label end_lbl;
        ]
  | Tacky.DoubleToUInt { src = src; dst = dst } ->
      let asm_src = convertVal src in
      let asm_dst = convertVal dst in
      if Tacky.typeOfVal dst = Types.UChar then
        [ Assembly.Cvttsd2si (Assembly.Longword, asm_src, Assembly.Reg Assembly.R9); Assembly.Mov (Assembly.Byte, Assembly.Reg Assembly.R9, asm_dst) ]
      else if Tacky.typeOfVal dst = Types.UInt then
          [
            Assembly.Cvttsd2si (Assembly.Quadword, asm_src, Assembly.Reg Assembly.R9);
            Assembly.Mov (Assembly.Longword, Assembly.Reg Assembly.R9, asm_dst);
          ]
      else
        let out_of_bounds = UniqueIds.makeLabel "dbl2ulong.oob" in
        let end_lbl = UniqueIds.makeLabel "dbl2ulong.end" in
        let upper_bound = addConstant None 9223372036854775808.0 in
        let upper_bound_as_int =
          (* interpreted as signed integer, upper bound wraps around to become
             minimum int *)
          Assembly.Imm System.Int64.MinValue
        in
        let r, x = (Assembly.Reg Assembly.R9, Assembly.Reg Assembly.XMM7) in
        [
          Assembly.Cmp (Assembly.Double, Assembly.Data (upper_bound, 0), asm_src);
          Assembly.JmpCC (Assembly.AE, out_of_bounds);
          Assembly.Cvttsd2si (Assembly.Quadword, asm_src, asm_dst);
          Assembly.Jmp end_lbl;
          Assembly.Label out_of_bounds;
          Assembly.Mov (Assembly.Double, asm_src, x);
          Assembly.Binary { op = Assembly.Sub; t = Assembly.Double; src = Assembly.Data (upper_bound, 0); dst = x };
          Assembly.Cvttsd2si (Assembly.Quadword, x, asm_dst);
          Assembly.Mov (Assembly.Quadword, upper_bound_as_int, r);
          Assembly.Binary { op = Assembly.Add; t = Assembly.Quadword; src = r; dst = asm_dst };
          Assembly.Label end_lbl;
        ]
  | Tacky.CopyToOffset { src = src; dst = dst; offset = offset }
    when TypeUtils.isScalar (Tacky.typeOfVal src) ->
      [ Assembly.Mov (asmType src, convertVal src, Assembly.PseudoMem (dst, offset)) ]
  | Tacky.CopyToOffset { src = src; dst = dst; offset = offset } ->
      let asm_src = convertVal src in
      let asm_dst = Assembly.PseudoMem (dst, offset) in
      let byte_count = int (TypeUtils.getSize (Tacky.typeOfVal src)) in
      copyBytes asm_src asm_dst byte_count
  | Tacky.CopyFromOffset { src = src; dst = dst; offset = offset }
    when TypeUtils.isScalar (Tacky.typeOfVal dst) ->
      [ Assembly.Mov (asmType dst, Assembly.PseudoMem (src, offset), convertVal dst) ]
  | Tacky.CopyFromOffset { src = src; dst = dst; offset = offset } ->
      let asm_src = Assembly.PseudoMem (src, offset) in
      let asm_dst = convertVal dst in
      let byte_count = int (TypeUtils.getSize (Tacky.typeOfVal dst)) in
      copyBytes asm_src asm_dst byte_count
  | Tacky.AddPtr { ptr = ptr; index = Tacky.Constant (Const.ConstLong c); scale = scale; dst = dst } ->
      (* note that typechecker converts index to long. QUESTION: what's the
         largest offset we should support? *)
      let i = int c
      [
        Assembly.Mov (Assembly.Quadword, convertVal ptr, Assembly.Reg Assembly.R9);
        Assembly.Lea (Assembly.Memory (Assembly.R9, i * scale), convertVal dst);
      ]
  | Tacky.AddPtr { ptr = ptr; index = index; scale = scale; dst = dst } ->
      if scale = 1 || scale = 2 || scale = 4 || scale = 8 then
        [
          Assembly.Mov (Assembly.Quadword, convertVal ptr, Assembly.Reg Assembly.R8);
          Assembly.Mov (Assembly.Quadword, convertVal index, Assembly.Reg Assembly.R9);
          Assembly.Lea (Assembly.Indexed { ``base`` = Assembly.R8; index = Assembly.R9; scale = scale }, convertVal dst);
        ]
      else
        [
          Assembly.Mov (Assembly.Quadword, convertVal ptr, Assembly.Reg Assembly.R8);
          Assembly.Mov (Assembly.Quadword, convertVal index, Assembly.Reg Assembly.R9);
          Assembly.Binary
            {
              op = Assembly.Mult;
              t = Assembly.Quadword;
              src = Assembly.Imm (int64 scale);
              dst = Assembly.Reg Assembly.R9;
            };
          Assembly.Lea (Assembly.Indexed { ``base`` = Assembly.R8; index = Assembly.R9; scale = 1 }, convertVal dst);
        ]

let passParams param_list return_on_stack =
  let int_reg_params, dbl_reg_params, stack_params =
    classifyParameters param_list return_on_stack
  in
  let copy_dst_ptr, remaining_int_regs =
    if return_on_stack then
      ( [ Assembly.Mov (Assembly.Quadword, Assembly.Reg Assembly.DI, Assembly.Memory (Assembly.BP, -8)) ],
        List.tail intParamPassingRegs )
    else ([], intParamPassingRegs)
  in
  (* pass parameter in register *)
  let pass_in_int_register idx (param_t, param) =
    let r = List.item idx remaining_int_regs in
    match param_t with
    | Assembly.ByteArray { size = size } ->
        copyBytesFromReg r param size
    | _ -> [ Assembly.Mov (param_t, Assembly.Reg r, param) ]
  in
  let pass_in_dbl_register idx param =
    let r = List.item idx dblParamPassingRegs in
    Assembly.Mov (Assembly.Double, Assembly.Reg r, param)
  in
  let pass_on_stack idx (param_t, param) =
    (* first param passed on stack has idx 0 and is passed at 16(%rbp) *)
    let stk = Assembly.Memory (Assembly.BP, 16 + (8 * idx)) in
    match param_t with
    | Assembly.ByteArray { size = size } -> copyBytes stk param size
    | _ -> [ Assembly.Mov (param_t, stk, param) ]
  in
  copy_dst_ptr
  @ List.concat (List.mapi pass_in_int_register int_reg_params)
  @ List.mapi pass_in_dbl_register dbl_reg_params
  @ List.concat (List.mapi pass_on_stack stack_params)

let returnsOnStack fn_name =
  match (Symbols.get fn_name).symType with
  | Types.FunType (_, Types.Structure tag) -> (
      match classifyStructure tag with Mem :: _ -> true | _ -> false)
  | Types.FunType _ -> false
  | _ -> failwith "Internal error: not a function name"

(* Special-case logic to get type/alignment of array; array variables w/ size
   >=16 bytes have alignment of 16 *)
let getVarAlignment = function
  | Types.Array _ as t when int (TypeUtils.getSize t) >= 16 -> 16
  | t -> TypeUtils.getAlignment t

let convertVarType = function
  | Types.Array _ as t ->
      Assembly.ByteArray
        { size = int (TypeUtils.getSize t); alignment = getVarAlignment t }
  | other -> convertType other

let convertTopLevel = function
  | Tacky.Function { name = name; ``global`` = ``global``; body = body; ``params`` = params_ } ->
      let return_on_stack = returnsOnStack name in
      let params_as_tacky = List.map (fun name -> Tacky.Var name) params_ in
      let instructions =
        passParams params_as_tacky return_on_stack
        @ List.collect convertInstruction body
      in
      Assembly.Function { name = name; ``global`` = ``global``; instructions = instructions }
  | Tacky.StaticVariable { name = name; ``global`` = ``global``; t = t; init = init } ->
      Assembly.StaticVariable
        { name = name; ``global`` = ``global``; alignment = getVarAlignment t; init = init }
  | Tacky.StaticConstant { name = name; t = t; init = init } ->
      Assembly.StaticConstant
        { name = name; alignment = TypeUtils.getAlignment t; init = init }

let convertConstant (kvp: KeyValuePair<int64, string * int>) =
  let key = kvp.Key
  let (name, alignment) = kvp.Value
  let dbl = System.BitConverter.Int64BitsToDouble key
  AssemblySymbols.addConstant name Assembly.Double
  Assembly.StaticConstant
    { name = name; alignment = alignment; init = Initializers.DoubleInit dbl }

(* convert each symbol table entry to assembly symbol table equivalent*)
let convertSymbol name = function
  | { Symbols.symType = Types.FunType (paramTypes, retType);
      Symbols.attrs = Symbols.FunAttr { defined = defined };
    }
    when (TypeUtils.isComplete retType || retType = Types.Void)
         && List.forall TypeUtils.isComplete paramTypes ->
      let ret_regs, return_on_stack = classifyReturnType retType in

      let param_regs = classifyParamTypes paramTypes return_on_stack in
      AssemblySymbols.addFun name defined (returnsOnStack name) param_regs
        ret_regs
  | { Symbols.symType = Types.FunType _; Symbols.attrs = Symbols.FunAttr { defined = defined } } ->
      (* If this function has incomplete return type besides void, or any incomplete
       * param type (implying we don't define or call it in this translation unit)
       * use dummy values *)
      assert (not defined)
      AssemblySymbols.addFun name defined false [] []
  | { Symbols.symType = t; attrs = Symbols.ConstAttr _ } ->
      AssemblySymbols.addConstant name (convertType t)
  (* use dummy type for static variables of incomplete type *)
  | { Symbols.symType = t; attrs = Symbols.StaticAttr _ } when not (TypeUtils.isComplete t) ->
      AssemblySymbols.addVar name Assembly.Byte true
  | { Symbols.symType = t; attrs = Symbols.StaticAttr _ } ->
      AssemblySymbols.addVar name (convertVarType t) true
  | { Symbols.symType = t } -> AssemblySymbols.addVar name (convertVarType t) false

let gen (Tacky.Program top_levels) =
  (* clear the hashtable (necessary if we're compiling multiple source) *)
  constants.Clear()
  let tls = List.map convertTopLevel top_levels
  let constants_list =
    constants
    |> Seq.map convertConstant
    |> List.ofSeq
  
  let prog = Assembly.Program (constants_list @ tls)
  let _ = Symbols.iter convertSymbol
  prog