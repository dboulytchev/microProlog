type goal  = Ast.body_item list
type state = goal * Unify.subst
type stack = state list

let rec solve database = function
  | [] -> `End
  | (goal, subst)::stack ->
      (match goal with
       | [] -> `Answer (subst, stack)
       | a::atoms ->
          (match a with
	   | `Cut -> invalid_arg "cut not supported"
	   | #Ast.atom as a ->
              (match database#find a subst with
	      | [] -> solve database stack
	      | versions ->
	          solve database @@
                     List.fold_left (fun stack (subst', btoms) -> (btoms @ List.map (Unify.apply subst') atoms, subst')::stack) 
		       stack
		       versions
	      )
          )
      )
