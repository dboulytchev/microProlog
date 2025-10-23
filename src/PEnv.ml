class c =
  object (self)
    val clauses : Ast.clause list ref = ref []

    val index  = ref 0
    val trace  = ref false
    val depth  = ref None 
    val factor = ref 2

    method index           = !index
    method increment_index = incr index
    method clauses         = !clauses
    method check_depth n   = match !depth with None -> true | Some depth -> n <= depth
    method increment       = match !depth with None -> self | Some d -> (depth := Some (d * !factor); self)
    method set_factor  n   = if n > 0 then factor := n else Printf.printf "Factor should be positive.\n"
    method set_depth   n   = if n > 0 then depth := (Some n) else Printf.printf "Depth should be positive.\n"
    method unset_depth     = depth := None
    method show_depth      = match !depth with
                             | None   -> "Iterative deepening is disabled."
                             | Some d -> Printf.sprintf "Iterative deepening depth=%d, factor=%d." d !factor
    method trace_on        = trace := true
    method trace_off       = trace := false
    method trace       s   = if !trace then Printf.printf "%s\n%!" s
    method wait            = if !trace then ignore (read_line ())

    method add (c : Ast.clause) = clauses := !clauses @ [c]

    method show =
      List.iter (fun c -> Printf.printf "%s\n" (Ostap.Pretty.toString (Ast.pretty_clause c))) !clauses
    method clear = clauses := []
  end
