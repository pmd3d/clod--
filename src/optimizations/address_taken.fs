module Address_taken

let analyze instrs =
    let addr_taken =
        function
        | Tacky.GetAddress { src = Tacky.Var v } -> Some v
        | _ -> None

    Set.ofList (List.choose addr_taken instrs)

let analyze_program (Tacky.Program tls) =
    let analyze_tl =
        function
        | Tacky.Function f -> analyze f.body
        | _ -> Set.empty

    let aliased_vars_per_fun = List.map analyze_tl tls
    List.fold Set.union Set.empty aliased_vars_per_fun