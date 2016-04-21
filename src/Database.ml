class c =
  object
    val clauses : Ast.clause list ref = ref [] 
    method add (c : Ast.clause) = clauses := c :: !clauses
    method show =
      List.iter (fun c -> Printf.printf "%s\n" (Ostap.Pretty.toString (Ast.pretty_clause c))) !clauses
    method clear = clauses := []
    method find (a : Ast.atom) (s : Unify.subst) = 
      List.fold_left 
	(fun acc (`Clause (b, `Body bs)) ->
           match Unify.unify (Some s) (Ast.to_term a) (Ast.to_term b) with
	   | None    -> acc
	   | Some s' -> (s', List.map (Unify.apply s') bs)::acc
	) 
	[] 
	!clauses
  end
