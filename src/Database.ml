class c =
  object
    val clauses : Ast.clause list ref = ref [] 
    val index = ref 0
    method add (c : Ast.clause) = clauses := c :: !clauses
    method show =
      List.iter (fun c -> Printf.printf "%s\n" (Ostap.Pretty.toString (Ast.pretty_clause c))) !clauses
    method clear = clauses := []
    method find (a : Ast.atom) (s : Unify.subst) = 
      let name = 
	let i = !index in
	fun s -> Printf.sprintf "%d_%s" i s 
      in
      incr index;
      List.fold_left 
	(fun acc (`Clause (b, `Body bs)) ->
	   let module M = Map.Make (String) in
	   let m = ref M.empty in
           let rename a =
	     GT.transform(Ast.atom) 
               (object inherit [Ast.atom] @Ast.atom[gmap] 
                  method c_Var _ _ x = 
		    try `Var (M.find x !m)
		    with Not_found ->
		      let x' = name x in
		      m := M.add x x' !m;
		      `Var x'
                end) 
	       ()
	       a
	   in
	   let b  = rename b in
	   let bs = 
	     List.map (
	       function
	       | `Cut -> `Cut
               | #Ast.atom as a -> (rename a :> Ast.body_item)
	     ) bs 
	   in
           match Unify.unify (Some s) (Ast.to_term a) (Ast.to_term b) with
	   | None    -> acc
	   | Some s' -> (s', List.map (Unify.apply s') bs)::acc
	) 
	[] 
	!clauses
  end
