open Unsigned
open TypeUtils
open Const
open System.Collections.Generic

let int_param_passing_regs = [ Assembly.DI; Assembly.SI; Assembly.DX; Assembly.CX; Assembly.R8; Assembly.R9 ]

let dbl_param_passing_regs =
  [ Assembly.XMM0; Assembly.XMM1; Assembly.XMM2; Assembly.XMM3; Assembly.XMM4; Assembly.XMM5; Assembly.XMM6; Assembly.XMM7 ]

let zero = Assembly.Imm 0L
let constants = new Dictionary<int64, string * int>()

let add_constant alignmentOpt dbl =
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
let get_eightbyte_type eightbyte_idx total_var_size =
  let bytes_left = total_var_size - (eightbyte_idx * 8)
  match bytes_left with
  | x when x >= 8 -> Assembly.Quadword (* *)
  | 4 -> Assembly.Longword
  | 1 -> Assembly.Byte
  | x -> Assembly.ByteArray { size = x; alignment = 8 }

let add_offset n = function
  | Assembly.PseudoMem (base_, off) -> Assembly.PseudoMem (base_, off + n)
  | Assembly.Memory (r, off) -> Assembly.Memory (r, off + n)
  (* you could do pointer arithmetic w/ indexed or data operands but we don't
     need to *)
  | Assembly.Imm _ | Assembly.Reg _ | Assembly.Pseudo _ | Assembly.Indexed _ | Assembly.Data _ ->
      failwith
        "Internal error: trying to copy data to or from non-memory operand"

let rec copy_bytes src_val dst_val byte_count =
  if byte_count = 0 then []
  else
    let operand_type, operand_size =
      if byte_count < 4 then (Assembly.Byte, 1)
      else if byte_count < 8 then (Assembly.Longword, 4)
      else (Assembly.Quadword, 8)
    
    let next_src = add_offset operand_size src_val
    let next_dst = add_offset operand_size dst_val
    let bytes_left = byte_count - operand_size
    Assembly.Mov (operand_type, src_val, dst_val)
    :: copy_bytes next_src next_dst bytes_left

(* copy an uneven, smaller-than-quadword eightbyte from memory into a register:
 * repeatedly copy byte into register and shift left, starting w/ highest byte and working down to lowest *)
let copy_bytes_to_reg src_val dst_reg byte_count =
  let copy_byte i =
    let mv = Assembly.Mov (Assembly.Byte, add_offset i src_val, Assembly.Reg dst_reg) in
    if i = 0 then [ mv ]
    else
      [ mv; Assembly.Binary { op = Assembly.Shl; t = Assembly.Quadword; src = Assembly.Imm 8L; dst = Assembly.Reg dst_reg } ]
  
  (* [0; 1; ... ; byte_count - 1] *)
  let byte_counts = List.init byte_count id |> List.rev

  List.collect copy_byte byte_counts

(* copy an uneven, smaller-than-quadword eightbyte from a register into memory;
 * repeatedly copy byte into register and shift right, starting w/ byte 0  and working up *)
let copy_bytes_from_reg src_reg dst_val byte_count =
  let copy_byte i =
    let mv = Assembly.Mov (Assembly.Byte, Assembly.Reg src_reg, add_offset i dst_val) in
    if i < byte_count - 1 then
        [
          mv;
          Assembly.Binary
            { op = Assembly.ShrBinop; t = Assembly.Quadword; src = Assembly.Imm 8L; dst = Assembly.Reg src_reg };
        ]
    else [ mv ]
  
  List.concat (List.init byte_count copy_byte)

let convert_val = function
  | Tacky.Constant (Const.ConstChar c) -> Assembly.Imm (int64 c)
  | Tacky.Constant (Const.ConstUChar uc) -> Assembly.Imm (int64 uc)
  | Tacky.Constant (Const.ConstInt i) -> Assembly.Imm (int64 i)
  | Tacky.Constant (Const.ConstLong l) -> Assembly.Imm l
  | Tacky.Constant (Const.ConstUInt u) -> Assembly.Imm (int64 u)
  | Tacky.Constant (Const.ConstULong ul) -> Assembly.Imm (int64 ul)
  | Tacky.Constant (Const.ConstDouble d) -> Assembly.Data (add_constant None d, 0)
  | Tacky.Var v ->
      if TypeUtils.isScalar (Symbols.get v).t then Assembly.Pseudo v
      else Assembly.PseudoMem (v, 0)

let convert_type = function
  | Types.Int | Types.UInt -> Assembly.Longword
  | Types.Long | Types.ULong | Types.Pointer _ -> Assembly.Quadword
  | Types.Char | Types.SChar | Types.UChar -> Assembly.Byte
  | Types.Double -> Assembly.Double
  | (Types.Array _ | Types.Structure _) as t ->
      Assembly.ByteArray
        { size = TypeUtils.getSize t; alignment = TypeUtils.getAlignment t }
  | (Types.FunType _ | Types.Void) as t ->
      failwith
        ("Internal error, converting type to assembly: " + Types.show t)

let asm_type v = convert_type (Tacky.type_of_val v)

let convert_unop = function
  | Tacky.Complement -> Assembly.Not
  | Tacky.Negate -> Assembly.Neg
  | Tacky.Not ->
      failwith "Internal error, can't convert TACKY not directly to assembly"

let convert_binop = function
  | Tacky.Add -> Assembly.Add
  | Tacky.Subtract -> Assembly.Sub
  | Tacky.Multiply -> Assembly.Mult
  | Tacky.Divide ->
      Assembly.DivDouble (* NB should only be called for operands on doubles *)
  | Tacky.Mod | Tacky.Equal | Tacky.NotEqual | Tacky.GreaterOrEqual | Tacky.LessOrEqual | Tacky.GreaterThan
  | Tacky.LessThan ->
      failwith "Internal error: not a binary assembly instruction"

let convert_cond_code signed = function
  | Tacky.Equal -> Assembly.E
  | Tacky.NotEqual -> Assembly.NE
  | Tacky.GreaterThan -> if signed then Assembly.G else Assembly.A
  | Tacky.GreaterOrEqual -> if signed then Assembly.GE else Assembly.AE
  | Tacky.LessThan -> if signed then Assembly.L else Assembly.B
  | Tacky.LessOrEqual -> if signed then Assembly.LE else Assembly.BE
  | _ -> failwith "Internal error: not a condition code"

type cls = Mem | SSE | Integer

let classify_new_structure tag =
  let { Type_table.size = size; _ } = Type_table.find tag
  if size > 16 then
    let eightbyte_count = (size / 8) + if size % 8 = 0 then 0 else 1
    ListUtil.make_list eightbyte_count Mem
  else
    let rec f = function
      | Types.Structure struct_tag ->
          let member_types = Type_table.get_member_types struct_tag
          List.collect f member_types
      | Types.Array { elem_type; size } ->
          List.concat (ListUtil.make_list size (f elem_type))
      | t -> [ t ]
    
    let scalar_types = f (Types.Structure tag)
    let first, last = (List.head scalar_types, List.last scalar_types)
    if size > 8 then
      let first_class = if first = Types.Double then SSE else Integer
      let last_class = if last = Types.Double then SSE else Integer
      [ first_class; last_class ]
    else if first = Types.Double then [ SSE ]
    else [ Integer ]

(* memoize results of classify_structure *)
let classified_structures = new Dictionary<_,_>()

let classify_structure tag =
  match classified_structures.TryGetValue tag with
  | true, classes -> classes
  | false, _ ->
      let classes = classify_new_structure tag
      classified_structures.Add(tag, classes)
      classes

let classify_params_helper typed_asm_vals return_on_stack =
  let int_regs_available = if return_on_stack then 5 else 6
  let process_one_param (int_reg_args, dbl_reg_args, stack_args)
      (tacky_t, operand) =
    let t = convert_type tacky_t
    let typed_operand = (t, operand)
    match tacky_t with
    | Types.Structure s ->
        (* it's a structure *)
        let var_name =
          match operand with
          | Assembly.PseudoMem (n, 0) -> n
          | _ -> failwith "Bad structure operand"
        
        let var_size = Type_utils.get_size tacky_t
        let classes = classify_structure s
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
                    get_eightbyte_type i var_size
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
            get_eightbyte_type i var_size
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

let classify_parameters params_ return_on_stack =
  let f v = (Tacky.type_of_val v, convert_val v) in
  classify_params_helper (List.map f params_) return_on_stack

let classify_param_types type_list return_on_stack =
  let f t =
    if Type_utils.is_scalar t then (t, Assembly.Pseudo "dummy")
    else (t, Assembly.PseudoMem ("dummy", 0))
  in
  let ints, dbls, _ =
    classify_params_helper (List.map f type_list) return_on_stack
  in
  let int_regs = ListUtil.take (List.length ints) int_param_passing_regs in
  let dbl_regs = ListUtil.take (List.length dbls) dbl_param_passing_regs in
  int_regs @ dbl_regs

let classify_return_helper ret_type asm_retval =
  match ret_type with
  | Types.Structure tag ->
      let classes = classify_structure tag in
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
                get_eightbyte_type i
                  (Type_utils.get_size ret_type)
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
      let typed_operand = (convert_type t, asm_retval) in
      ([ typed_operand ], [], false)

let classify_return_value retval =
  classify_return_helper (Tacky.type_of_val retval) (convert_val retval)

let classify_return_type = function
  | Types.Void -> ([], false)
  | t ->
      let asm_val =
        if Type_utils.is_scalar t then Assembly.Pseudo "dummy"
        else Assembly.PseudoMem ("dummy", 0)
      in
      let ints, dbls, return_on_stack = classify_return_helper t asm_val in
      if return_on_stack then ([ Assembly.AX ], true)
      else
        let int_regs = ListUtil.take (List.length ints) [ Assembly.AX; Assembly.DX ] in
        let dbl_regs =
          ListUtil.take (List.length dbls) [ Assembly.XMM0; Assembly.XMM1 ]
        in
        (int_regs @ dbl_regs, false)

let convert_function_call f args dst =
  let int_retvals, dbl_retvals, return_on_stack =
    match dst with Some d -> classify_return_value d | None -> ([], [], false)
  in
  (* load address of dest into DI *)
  let load_dst_instruction, first_intreg_idx =
    if return_on_stack then
      ([ Assembly.Lea (convert_val (Option.get dst), Assembly.Reg Assembly.DI) ], 1)
    else ([], 0)
  in

  let int_reg_args, dbl_reg_args, stack_args =
    classify_parameters args return_on_stack
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
    let r = List.item (idx + first_intreg_idx) int_param_passing_regs in
    match arg_t with
    | Assembly.ByteArray { size; _ } ->
        copy_bytes_to_reg arg r size (* copy_thru_redzone arg r size *)
    | _ -> [ Assembly.Mov (arg_t, arg, Assembly.Reg r) ]
  in
  let instructions =
    instructions @ List.concat (List.mapi pass_int_reg_arg int_reg_args)
  in

  (* pass args in registers *)
  let pass_dbl_reg_arg idx arg =
    let r = List.item idx dbl_param_passing_regs in
    Assembly.Mov (Assembly.Double, arg, Assembly.Reg r)
  in
  let instructions = instructions @ List.mapi pass_dbl_reg_arg dbl_reg_args in

  (* pass args on the stack*)
  let pass_stack_arg (arg_t, arg) =
    match (arg, arg_t) with
    | (Assembly.Imm _ | Assembly.Reg _), _ | _, (Assembly.Quadword | Assembly.Double) ->
        [ Assembly.Push arg ]
    | _, Assembly.ByteArray { size; _ } ->
        Assembly.Binary
          { op = Assembly.Sub; t = Assembly.Quadword; src = Assembly.Imm (int64 8); dst = Assembly.Reg Assembly.SP }
        :: copy_bytes arg (Assembly.Memory (Assembly.SP, 0)) size
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
          | Assembly.ByteArray { size; _ } ->
              copy_bytes_from_reg r op size
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

let convert_return_instruction = function
  | None -> [ Assembly.Ret ]
  | Some v ->
      let int_retvals, dbl_retvals, return_on_stack = classify_return_value v in
      if return_on_stack then
        let byte_count = TypeUtils.getSize (Tacky.type_of_val v) in
        let get_ptr = Assembly.Mov (Assembly.Quadword, Assembly.Memory (Assembly.BP, -8), Assembly.Reg Assembly.AX) in
        let copy_into_ptr =
          copy_bytes (convert_val v) (Assembly.Memory (Assembly.AX, 0)) byte_count
        in
        (get_ptr :: copy_into_ptr) @ [ Assembly.Ret ]
      else
        let return_ints =
          List.concat
            (List.mapi
               (fun i (t, op) ->
                 let dst_reg = List.item i [ Assembly.AX; Assembly.DX ] in
                 match t with
                 | Assembly.ByteArray { size; _ } ->
                     copy_bytes_to_reg op dst_reg
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

let convert_instruction = function
  | Tacky.Copy { src; dst } when Type_utils.is_scalar (Tacky.type_of_val src) ->
      let t = asm_type src in
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Assembly.Mov (t, asm_src, asm_dst) ]
  | Tacky.Copy { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      let byte_count = Type_utils.get_size (Tacky.type_of_val src) in
      copy_bytes asm_src asm_dst byte_count
  | Tacky.Return maybe_val -> convert_return_instruction maybe_val
  | Tacky.Unary { op = Tacky.Not; src; dst } ->
      let src_t = asm_type src in
      let dst_t = asm_type dst in

      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
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
  | Tacky.Unary { op = Tacky.Negate; src; dst } when Tacky.type_of_val src = Types.Double ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      let negative_zero = add_constant (Some 16) (-0.0) in
      [
        Assembly.Mov (Assembly.Double, asm_src, asm_dst);
        Assembly.Binary
          { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Data (negative_zero, 0); dst = asm_dst };
      ]
  | Tacky.Unary { op; src; dst } ->
      let t = asm_type src in
      let asm_op = convert_unop op in
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Assembly.Mov (t, asm_src, asm_dst); Assembly.Unary (asm_op, t, asm_dst) ]
  | Tacky.Binary { op; src1; src2; dst } -> (
      let src_t = asm_type src1 in
      let dst_t = asm_type dst in
      let asm_src1 = convert_val src1 in
      let asm_src2 = convert_val src2 in
      let asm_dst = convert_val dst in
      match op with
      (* Relational operator *)
      | Tacky.Equal | Tacky.NotEqual | Tacky.GreaterThan | Tacky.GreaterOrEqual | Tacky.LessThan | Tacky.LessOrEqual
        ->
          let signed =
            if src_t = Assembly.Double then false
            else Type_utils.is_signed (Tacky.type_of_val src1)
          in
          let cond_code = convert_cond_code signed op in
          [
            Assembly.Cmp (src_t, asm_src2, asm_src1);
            Assembly.Mov (dst_t, zero, asm_dst);
            Assembly.SetCC (cond_code, asm_dst);
          ]
      (* Division/modulo *)
      | (Tacky.Divide | Tacky.Mod) when src_t <> Assembly.Double ->
          let result_reg = if op = Tacky.Divide then Assembly.AX else Assembly.DX in
          if Type_utils.is_signed (Tacky.type_of_val src1) then
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
          let asm_op = convert_binop op in
          [
            Assembly.Mov (src_t, asm_src1, asm_dst);
            Assembly.Binary { op = asm_op; t = src_t; src = asm_src2; dst = asm_dst };
          ])
  | Tacky.Load { src_ptr; dst }
    when Type_utils.is_scalar (Tacky.type_of_val dst) ->
      let asm_src_ptr = convert_val src_ptr in
      let asm_dst = convert_val dst in
      let t = asm_type dst in
      [ Assembly.Mov (Assembly.Quadword, asm_src_ptr, Assembly.Reg Assembly.R9); Assembly.Mov (t, Assembly.Memory (Assembly.R9, 0), asm_dst) ]
  | Tacky.Load { src_ptr; dst } ->
      let asm_src_ptr = convert_val src_ptr in
      let asm_dst = convert_val dst in
      let byte_count = Type_utils.get_size (Tacky.type_of_val dst) in
      Assembly.Mov (Assembly.Quadword, asm_src_ptr, Assembly.Reg Assembly.R9)
      :: copy_bytes (Assembly.Memory (Assembly.R9, 0)) asm_dst byte_count
  | Tacky.Store { src; dst_ptr }
    when Type_utils.is_scalar (Tacky.type_of_val src) ->
      let asm_src = convert_val src in
      let t = asm_type src in
      let asm_dst_ptr = convert_val dst_ptr in
      [ Assembly.Mov (Assembly.Quadword, asm_dst_ptr, Assembly.Reg Assembly.R9); Assembly.Mov (t, asm_src, Assembly.Memory (Assembly.R9, 0)) ]
  | Tacky.Store { src; dst_ptr } ->
      let asm_src = convert_val src in
      let asm_dst_ptr = convert_val dst_ptr in
      let byte_count = Type_utils.get_size (Tacky.type_of_val src) in
      Assembly.Mov (Assembly.Quadword, asm_dst_ptr, Assembly.Reg Assembly.R9)
      :: copy_bytes asm_src (Assembly.Memory (Assembly.R9, 0)) byte_count
  | Tacky.GetAddress { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Assembly.Lea (asm_src, asm_dst) ]
  | Tacky.Jump target -> [ Assembly.Jmp target ]
  | Tacky.JumpIfZero (cond, target) ->
      let t = asm_type cond in
      let asm_cond = convert_val cond in
      if t = Assembly.Double then
        [
          Assembly.Binary
            { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Reg Assembly.XMM0; dst = Assembly.Reg Assembly.XMM0 };
          Assembly.Cmp (t, asm_cond, Assembly.Reg Assembly.XMM0);
          Assembly.JmpCC (Assembly.E, target);
        ]
      else [ Assembly.Cmp (t, zero, asm_cond); Assembly.JmpCC (Assembly.E, target) ]
  | Tacky.JumpIfNotZero (cond, target) ->
      let t = asm_type cond in
      let asm_cond = convert_val cond in
      if t = Assembly.Double then
        [
          Assembly.Binary
            { op = Assembly.Xor; t = Assembly.Double; src = Assembly.Reg Assembly.XMM0; dst = Assembly.Reg Assembly.XMM0 };
          Assembly.Cmp (t, asm_cond, Assembly.Reg Assembly.XMM0);
          Assembly.JmpCC (Assembly.NE, target);
        ]
      else [ Assembly.Cmp (t, zero, asm_cond); Assembly.JmpCC (Assembly.NE, target) ]
  | Tacky.Label l -> [ Assembly.Label l ]
  | Tacky.FunCall { f; args; dst } -> convert_function_call f args dst
  | Tacky.SignExtend { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [
        Assembly.Movsx
          {
            src_type = asm_type src;
            dst_type = asm_type dst;
            src = asm_src;
            dst = asm_dst;
          };
      ]
  | Tacky.Truncate { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [ Assembly.Mov (asm_type dst, asm_src, asm_dst) ]
  | Tacky.ZeroExtend { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      [
        Assembly.MovZeroExtend
          {
            src_type = asm_type src;
            dst_type = asm_type dst;
            src = asm_src;
            dst = asm_dst;
          };
      ]
  | Tacky.IntToDouble { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      let t = asm_type src in
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
  | Tacky.DoubleToInt { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      let t = asm_type dst in
      if t = Assembly.Byte then
        [ Assembly.Cvttsd2si (Assembly.Longword, asm_src, Assembly.Reg Assembly.R9); Assembly.Mov (Assembly.Byte, Assembly.Reg Assembly.R9, asm_dst) ]
      else [ Assembly.Cvttsd2si (t, asm_src, asm_dst) ]
  | Tacky.UIntToDouble { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      if Tacky.type_of_val src = Types.UChar then
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
      else if Tacky.type_of_val src = Types.UInt then
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
        let out_of_bounds = Unique_ids.make_label "ulong2dbl.oob" in
        let end_lbl = Unique_ids.make_label "ulong2dbl.end" in
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
  | Tacky.DoubleToUInt { src; dst } ->
      let asm_src = convert_val src in
      let asm_dst = convert_val dst in
      if Tacky.type_of_val dst = Types.UChar then
        [ Assembly.Cvttsd2si (Assembly.Longword, asm_src, Assembly.Reg Assembly.R9); Assembly.Mov (Assembly.Byte, Assembly.Reg Assembly.R9, asm_dst) ]
      else if Tacky.type_of_val dst = Types.UInt then
          [
            Assembly.Cvttsd2si (Assembly.Quadword, asm_src, Assembly.Reg Assembly.R9);
            Assembly.Mov (Assembly.Longword, Assembly.Reg Assembly.R9, asm_dst);
          ]
      else
        let out_of_bounds = Unique_ids.make_label "dbl2ulong.oob" in
        let end_lbl = Unique_ids.make_label "dbl2ulong.end" in
        let upper_bound = add_constant None 9223372036854775808.0 in
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
  | Tacky.CopyToOffset { src; dst; offset }
    when Type_utils.is_scalar (Tacky.type_of_val src) ->
      [ Assembly.Mov (asm_type src, convert_val src, Assembly.PseudoMem (dst, offset)) ]
  | Tacky.CopyToOffset { src; dst; offset } ->
      let asm_src = convert_val src in
      let asm_dst = Assembly.PseudoMem (dst, offset) in
      let byte_count = Type_utils.get_size (Tacky.type_of_val src) in
      copy_bytes asm_src asm_dst byte_count
  | Tacky.CopyFromOffset { src; dst; offset }
    when Type_utils.is_scalar (Tacky.type_of_val dst) ->
      [ Assembly.Mov (asm_type dst, Assembly.PseudoMem (src, offset), convert_val dst) ]
  | Tacky.CopyFromOffset { src; dst; offset } ->
      let asm_src = Assembly.PseudoMem (src, offset) in
      let asm_dst = convert_val dst in
      let byte_count = Type_utils.get_size (Tacky.type_of_val dst) in
      copy_bytes asm_src asm_dst byte_count
  | Tacky.AddPtr { ptr; index = Tacky.Constant (Tacky.ConstLong c); scale; dst } ->
      (* note that typechecker converts index to long. QUESTION: what's the
         largest offset we should support? *)
      let i = int c
      [
        Assembly.Mov (Assembly.Quadword, convert_val ptr, Assembly.Reg Assembly.R9);
        Assembly.Lea (Assembly.Memory (Assembly.R9, i * scale), convert_val dst);
      ]
  | Tacky.AddPtr { ptr; index; scale; dst } ->
      if scale = 1 || scale = 2 || scale = 4 || scale = 8 then
        [
          Assembly.Mov (Assembly.Quadword, convert_val ptr, Assembly.Reg Assembly.R8);
          Assembly.Mov (Assembly.Quadword, convert_val index, Assembly.Reg Assembly.R9);
          Assembly.Lea (Assembly.Indexed { base_ = Assembly.R8; index = Assembly.R9; scale = scale }, convert_val dst);
        ]
      else
        [
          Assembly.Mov (Assembly.Quadword, convert_val ptr, Assembly.Reg Assembly.R8);
          Assembly.Mov (Assembly.Quadword, convert_val index, Assembly.Reg Assembly.R9);
          Assembly.Binary
            {
              op = Assembly.Mult;
              t = Assembly.Quadword;
              src = Assembly.Imm (int64 scale);
              dst = Assembly.Reg Assembly.R9;
            };
          Assembly.Lea (Assembly.Indexed { base_ = Assembly.R8; index = Assembly.R9; scale = 1 }, convert_val dst);
        ]

let pass_params param_list return_on_stack =
  let int_reg_params, dbl_reg_params, stack_params =
    classify_parameters param_list return_on_stack
  in
  let copy_dst_ptr, remaining_int_regs =
    if return_on_stack then
      ( [ Assembly.Mov (Assembly.Quadword, Assembly.Reg Assembly.DI, Assembly.Memory (Assembly.BP, -8)) ],
        List.tail int_param_passing_regs )
    else ([], int_param_passing_regs)
  in
  (* pass parameter in register *)
  let pass_in_int_register idx (param_t, param) =
    let r = List.item idx remaining_int_regs in
    match param_t with
    | Assembly.ByteArray { size; _ } ->
        copy_bytes_from_reg r param size
    | _ -> [ Assembly.Mov (param_t, Assembly.Reg r, param) ]
  in
  let pass_in_dbl_register idx param =
    let r = List.item idx dbl_param_passing_regs in
    Assembly.Mov (Assembly.Double, Assembly.Reg r, param)
  in
  let pass_on_stack idx (param_t, param) =
    (* first param passed on stack has idx 0 and is passed at 16(%rbp) *)
    let stk = Assembly.Memory (Assembly.BP, 16 + (8 * idx)) in
    match param_t with
    | Assembly.ByteArray { size; _ } -> copy_bytes stk param size
    | _ -> [ Assembly.Mov (param_t, stk, param) ]
  in
  copy_dst_ptr
  @ List.concat (List.mapi pass_in_int_register int_reg_params)
  @ List.mapi pass_in_dbl_register dbl_reg_params
  @ List.concat (List.mapi pass_on_stack stack_params)

let returns_on_stack fn_name =
  match (Symbols.get fn_name).t with
  | Types.FunType { ret_type = Types.Structure tag; _ } -> (
      match classify_structure tag with Mem :: _ -> true | _ -> false)
  | Types.FunType _ -> false
  | _ -> failwith "Internal error: not a function name"

(* Special-case logic to get type/alignment of array; array variables w/ size
   >=16 bytes have alignment of 16 *)
let get_var_alignment = function
  | Types.Array _ as t when Type_utils.get_size t >= 16 -> 16
  | t -> Type_utils.get_alignment t

let convert_var_type = function
  | Types.Array _ as t ->
      Assembly.ByteArray
        { size = Type_utils.get_size t; alignment = get_var_alignment t }
  | other -> convert_type other

let convert_top_level = function
  | Tacky.Function { name; global; body; params_ } ->
      let return_on_stack = returns_on_stack name in
      let params_as_tacky = List.map (fun name -> Tacky.Var name) params_ in
      let instructions =
        pass_params params_as_tacky return_on_stack
        @ List.collect convert_instruction body
      in
      Assembly.Function { name; global; instructions }
  | Tacky.StaticVariable { name; global; t; init } ->
      Assembly.StaticVariable
        { name; global; alignment = get_var_alignment t; init }
  | Tacky.StaticConstant { name; t; init } ->
      Assembly.StaticConstant
        { name; alignment = Type_utils.get_alignment t; init }

let convert_constant (kvp: KeyValuePair<int64, string * int>) =
  let key = kvp.Key
  let (name, alignment) = kvp.Value
  let dbl = System.BitConverter.Int64BitsToDouble key
  Assembly_symbols.add_constant name Assembly.Double
  Assembly.StaticConstant
    { name; alignment; init = Initializers.DoubleInit dbl }

(* convert each symbol table entry to assembly symbol table equivalent*)
let convert_symbol name = function
  | Symbols.
      {
        t = Types.FunType { param_types; ret_type };
        attrs = Symbols.FunAttr { defined; _ };
      }
    when (Type_utils.is_complete ret_type || ret_type = Types.Void)
         && List.forall Type_utils.is_complete param_types ->
      let ret_regs, return_on_stack = classify_return_type ret_type in

      let param_regs = classify_param_types param_types return_on_stack in
      Assembly_symbols.add_fun name defined (returns_on_stack name) param_regs
        ret_regs
  | Symbols.{ t = Types.FunType _; attrs = Symbols.FunAttr { defined; _ } } ->
      (* If this function has incomplete return type besides void, or any incomplete
       * param type (implying we don't define or call it in this translation unit)
       * use dummy values *)
      assert (not defined)
      Assembly_symbols.add_fun name defined false [] []
  | Symbols.{ t; attrs = Symbols.ConstAttr _ } ->
      Assembly_symbols.add_constant name (convert_type t)
  (* use dummy type for static variables of incomplete type *)
  | Symbols.{ t; attrs = Symbols.StaticAttr _ } when not (Type_utils.is_complete t) ->
      Assembly_symbols.add_var name Assembly.Byte true
  | Symbols.{ t; attrs = Symbols.StaticAttr _; _ } ->
      Assembly_symbols.add_var name (convert_var_type t) true
  | Symbols.{ t; _ } -> Assembly_symbols.add_var name (convert_var_type t) false

let gen (Tacky.Program top_levels) =
  (* clear the hashtable (necessary if we're compiling multiple source) *)
  constants.Clear()
  let tls = List.map convert_top_level top_levels
  let constants_list =
    constants
    |> Seq.map convert_constant
    |> List.ofSeq
  
  let prog = Assembly.Program (constants_list @ tls)
  let _ = Symbols.iter convert_symbol
  prog