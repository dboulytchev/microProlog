type goal  = Ast.body_item list
type state = goal * Unify.subst
type stack = state list

let pretty_goal goal = Ostap.Pretty.listByComma @@ GT.gmap(GT.list) Ast.pretty_body_item goal

let pretty_state (goal, subst) =
  Ostap.Pretty.seq [
    pretty_goal goal;
    Ostap.Pretty.newline;
    Unify.pretty_subst (Some subst) 
  ]

let pretty_stack stack = Ostap.Pretty.seq @@
  GT.gmap(GT.list) (fun s -> Ostap.Pretty.seq [pretty_state s; Ostap.Pretty.newline]) stack

let rec solve_dfs env stack = 
  env#trace "Stack:";
  env#trace (Ostap.Pretty.toString (pretty_stack stack));
  env#wait;
  match stack with
  | [] -> `End
  | (goal, subst)::stack ->
      (match goal with
       | [] -> `Answer (subst, stack)
       | a::atoms ->
          (match a with
	   | `Cut -> invalid_arg "cut not supported"
	   | #Ast.atom as a ->
              (match env#find a subst with
	      | [] -> solve_dfs env stack
	      | versions ->
	          solve_dfs env @@
                     List.fold_left (fun stack (subst', btoms) -> (btoms @ atoms, subst')::stack) 
		       stack
		       versions
	      )
          )
      )

let rec solve_bfs env frontier = 
  env#trace "Frontier:";
  env#trace (Ostap.Pretty.toString (pretty_stack frontier));
  env#wait;
  match frontier with
  | [] -> `End
  | frontier ->
      let answers, rest = List.partition (function ([], _) -> true | _ -> false) frontier in
      (match answers with
       | (_, subst)::answers' -> `Answer (subst, answers' @ rest)
       | _ ->
          solve_bfs env @@
          List.flatten @@
          List.map 
	     (fun (goal, subst) -> 
	        let rec split acc prefix = function
		| []    -> acc
		| x::xs -> split ((x, prefix @ xs) :: acc) (prefix @[x]) xs
		in
		let variants = split [] [] goal in
                List.flatten @@
		List.map
		  (fun (a, atoms) ->
		     match a with
		     | `Cut -> invalid_arg "cut is not supported"
		     | #Ast.atom as a ->
                         List.map (fun (subst', btoms) -> btoms @ atoms, subst') (env#find a subst)
		  )
		  variants
	     ) 
          frontier
      )

let solve env stack = 
  (if env#is_dfs then solve_dfs else solve_bfs) env stack 

