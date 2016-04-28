type goal  = Ast.body_item list
type state = int * goal * Unify.subst
type stack = state list

let pretty_goal goal = Ostap.Pretty.listByComma @@ GT.gmap(GT.list) Ast.pretty_body_item goal

let pretty_state (depth, goal, subst) =
  Ostap.Pretty.seq [
    Ostap.Pretty.int depth;
    Ostap.Pretty.newline;
    pretty_goal goal;
    Ostap.Pretty.newline;
    Unify.pretty_subst (Some subst) 
  ]

let pretty_stack stack = Ostap.Pretty.seq @@
  GT.gmap(GT.list) (fun s -> Ostap.Pretty.seq [pretty_state s; Ostap.Pretty.newline]) stack

let rec solve env (bound, stack, pruned) = 
  env#trace "Stack:";
  env#trace (Ostap.Pretty.toString (pretty_stack stack));
  env#wait;
  match stack with
  | [] -> (match pruned with [] -> `End | _ -> solve env (bound + env#increment, pruned, []))
  | (depth, goal, subst)::stack when depth < bound ->
      (match goal with
       | [] -> `Answer (subst, (bound, stack, pruned))
       | a::atoms ->
          (match a with
	   | `Cut -> invalid_arg "cut not supported"
	   | #Ast.atom as a ->
              (match env#find a subst with
	      | [] -> solve env (bound, stack, pruned)
	      | versions ->
	          solve env @@ (
                     bound,
                     List.fold_left (fun stack (subst', btoms) -> (depth + 1, btoms @ atoms, subst')::stack) 
		       stack
		       versions,
                     pruned
                  )
	      )
          )
      )
  | state::stack -> solve env (bound, stack, state::pruned)

