class c =
  object (self)
    val clauses : Ast.clause list ref = ref [] 
    val index = ref 0
    val trace = ref false
    val increment = ref 50
    method increment = !increment
    method set_increment n = increment := n
    method trace_on  = trace := true
    method trace_off = trace := false
    method trace s = if !trace then Printf.printf "%s\n%!" s
    method wait = if !trace then ignore (read_line ())
    method add (c : Ast.clause) = clauses := c :: !clauses
    method show =
      List.iter (fun c -> Printf.printf "%s\n" (Ostap.Pretty.toString (Ast.pretty_clause c))) !clauses
    method clear = clauses := []
    method find (a : Ast.atom) (s : Unify.subst) = 
      let name = 
	let i = !index in
	fun s -> Printf.sprintf "$%d_%s" i s 
      in
      incr index;
      let found =
        List.rev @@
        List.fold_right 
	  (fun (`Clause (b, `Body bs)) acc ->
	     let module M = Map.Make (String) in
	     let m = ref M.empty in
             let rename a =
	       GT.transform(Ast.atom)              
                 (object inherit [Ast.atom] @Ast.atom[gmap]
                    method c_Functor _ _ f ts =
                      `Functor (
                         f,
                         GT.gmap(GT.list)
                            (GT.transform(Ast.term)
                                (object inherit [Ast.term] @Ast.term[gmap]        
                                   method c_Var _ _ x = 
		                     try `Var (M.find x !m)
		                     with Not_found ->
		                       let x' = name x in
		                       m := M.add x x' !m;
		                       `Var x'
			         end
			        )
                                ()
                            )
                            ts 
                       )
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
	     | Some s' -> (s', bs)::acc
	  )	 
	  !clauses 
          []
      in
      let rec pull = function
      | []  -> []
      | [x] -> [x]
      | ((_, []) as x) :: (((_, []) :: _) as tl) -> x :: pull tl
      | ((_, []) as x) :: y :: tl -> y :: pull (x :: tl)
      | x::tl -> x :: pull tl
      in
      pull found 

  end
