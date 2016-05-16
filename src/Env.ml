class c =
  object (self)
    val clauses : Ast.clause list ref = ref [] 
    val index = ref 0
    val trace = ref false
    val increment = ref 50
    method index = !index
    method increment_index = incr index
    method clauses = !clauses
    method increment = !increment
    method set_increment n = increment := n
    method trace_on  = trace := true
    method trace_off = trace := false
    method trace s = if !trace then Printf.printf "%s\n%!" s
    method wait = if !trace then ignore (read_line ())
    method add (c : Ast.clause) = clauses := !clauses @ [c]
    method show =
      List.iter (fun c -> Printf.printf "%s\n" (Ostap.Pretty.toString (Ast.pretty_clause c))) !clauses
    method clear = clauses := []
  end
