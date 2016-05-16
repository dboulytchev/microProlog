type goal  = Ast.body_item list
type state = int * goal * Unify.subst * Ast.clause list * stack
and  stack = state list

let pretty_goal goal = Ostap.Pretty.listByComma @@ GT.gmap(GT.list) Ast.pretty_body_item goal

let pretty_state (depth, goal, subst, clauses, stack) =
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
  let rec find (a : Ast.atom) (s : Unify.subst) clauses = 
    let name = 
      let i = env#index in
      fun s -> Printf.sprintf "$%d_%s" i s 
    in
    env#increment_index;
    let rec inner = function
    | [] -> None
    | `Clause (b, `Body bs) :: clauses' ->
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
        | None    -> inner clauses'
        | Some s' -> Some (s', bs, clauses')
    in
    inner clauses
  in
  env#trace "Stack:";
  env#trace (Ostap.Pretty.toString (pretty_stack stack));
  env#wait;
  match stack with
  | [] -> (match pruned with [] -> `End | _ -> solve env (bound + env#increment, pruned, []))
  | (depth, goal, subst, clauses, cut)::stack when depth < bound ->
      (match goal with
       | [] -> `Answer (subst, (bound, stack, pruned))
       | a::atoms ->
          (match a with
           | `Cut -> solve env (bound, (depth, atoms, subst, clauses, stack)::cut, pruned)
           | #Ast.atom as a ->
              (match find a subst clauses with
              | None -> solve env (bound, stack, pruned)
              | Some (subst', btoms, clauses') ->
                  solve env @@ (
                     bound,
                     (depth+1, btoms @ atoms, subst', env#clauses, cut) :: (depth, goal, subst, clauses', cut) :: stack,
                     pruned
                  )
              )
          )
      )
  | state::stack -> solve env (bound, stack, state::pruned)

