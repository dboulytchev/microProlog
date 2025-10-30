type item  = [ Ast.atom | `Cut of stack | `Diseq of Ast.term * Ast.term ] 
and  goal  = item list
and  state = int * goal * Unify.subst * ctor * Ast.clause list
and  stack = state list
and  ctor  = (Ast.term * Ast.term) list

let extend is =  
  List.map (function
            | `Cut            -> `Cut []
            | `Diseq (t1, t2) -> `Diseq (t1, t2)
            | #Ast.atom as a  -> (a :> item)
           ) is
 
let pretty_goal goal =
  Ostap.Pretty.listByComma @@
  GT.gmap(GT.list) (function
                      `Cut    _       -> Ostap.Pretty.string "!"
                    | `Diseq (t1, t2) -> Ast.pretty_body_item (`Diseq (t1, t2))
                    | #Ast.atom as a  -> Ast.pretty_atom a
                   ) goal

let pretty_state (depth, goal, subst, ctor, clauses) =
  Ostap.Pretty.seq [
    Ostap.Pretty.int depth;
    Ostap.Pretty.newline;
    pretty_goal goal;
    Ostap.Pretty.newline;
    Unify.pretty_subst (Some subst) 
  ]

let pretty_stack stack = Ostap.Pretty.seq @@
  GT.gmap(GT.list) (fun s -> Ostap.Pretty.seq [pretty_state s; Ostap.Pretty.newline]) stack

let rec solve env (stack, pruned) =
  let update_ctor ctor subst = (Unify.subst_to_diseq subst) :: ctor in
  let decorate_cut stack atoms = List.map (function `Cut _ -> `Cut stack | a -> a) atoms in 			
  let rec find (a : Ast.atom) (s : Unify.subst) clauses cut = 
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
        let rename_term t =
           GT.transform(Ast.term)
              (fun self -> object inherit [Ast.term, _] @Ast.term[gmap] self
                              method c_Var _ _ x = 
                                try `Var (M.find x !m)
                                with Not_found ->
                                  let x' = name x in
                                  m := M.add x x' !m;
                                  `Var x'
                           end
              )
              ()
              t
        in
        let rename a =
          GT.transform(Ast.atom)              
            (fun self -> object inherit [Ast.atom, _] @Ast.atom[gmap] self
               method c_Functor _ _ f ts =
                 `Functor (
                    f,
                    GT.gmap(GT.list) rename_term ts 
                  )
             end) 
            ()
            a
        in
        let b  = rename b in
        let bs = 
          List.map (
            function
            | `Cut            -> `Cut cut
            | `Diseq (t1, t2) -> `Diseq (rename_term t1, rename_term t2)
            | #Ast.atom as a  -> (rename a :> item)
          ) bs 
        in
        match Unify.unify (Some s) (Ast.to_term a) (Ast.to_term b) with
        | None    -> inner clauses'
        | Some s' ->
           if Unify.is_empty s'
           then Some (s', bs, clauses')
           else
             (try 
                let ctor' = List.flatten @@ List.map
                              (fun (t1, t2) ->
                                 match Unify.unify (Some s') t1 t2 with
                                 | None -> []
                                 | Some s'' ->
                                    if Unify.is_empty s''
                                    then raise Disequality_violated
                                    else [Unify.subst_to_diseq s'']
                              )
                              ctor
                in
              with Disequality_violated -> inner clauses'
             )
    in
    inner clauses
  in
  env#trace "Stack:";
  env#trace (Ostap.Pretty.toString (pretty_stack stack)); 
  env#wait;
  match stack with
  | [] -> (match pruned with [] -> `End | _ -> solve env (pruned, []))
  | (depth, goal, subst, ctor, clauses)::stack when env#check_depth depth ->
      (match goal with
       | [] -> `Answer (subst, (stack, pruned))
       | a::atoms ->
          (match a with
           | `Cut cut -> solve env ((depth, atoms, subst, ctor, clauses) :: cut, pruned)
           | `Diseq (t1, t2) ->
              (match Unify.unify (Some subst) t1 t2 with
               | None -> solve env ((depth, atoms, subst, ctor, clauses) :: stack, pruned)
               | Some subst' ->
                  if Unify.is_empty subst'
                  then solve env (stack, pruned)
                  else solve env ((depth, atoms, subst, update_ctor ctor subst', clauses) :: stack, pruned)  
              )
              
           | #Ast.atom as a ->
             (match find a subst clauses stack with
              | None -> solve env (stack, pruned)
              | Some (subst', btoms, clauses') ->
                  let stack' = (depth, goal, subst, ctor, clauses')::stack in
                  solve env @@ (
                     (depth+1, (decorate_cut stack btoms) @ atoms, subst', ctor, env#clauses)::stack',
                     pruned
                  )
              )
          )
      )
  | state::stack -> solve env#increment (stack, state::pruned)

