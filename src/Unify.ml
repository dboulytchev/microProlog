module M = Map.Make (String)

type subst = Ast.term M.t

let empty = M.empty

let pretty_subst = function
| None   -> Ostap.Pretty.string "fail"
| Some m ->
    Ostap.Pretty.listByBreak @@
    List.map 
      (fun (x, t) -> 
	 Ostap.Pretty.listBySpace [
	   Ostap.Pretty.string x; 
	   Ostap.Pretty.string "="; 
	   Ast.pretty_term t
         ]
      )
      (M.bindings m)

let rec walk : subst -> Ast.term -> Ast.term = fun s x -> 
  match x with
  | `Var y -> (try walk s (M.find y s) with Not_found -> x)
  | _ -> x

let walk' : subst -> Ast.term -> Ast.term = fun s x -> 
 GT.transform(Ast.term) 
    (object inherit [Ast.term] @Ast.term[gmap]
       method c_Var _ v x = 
         try v.GT.f () (M.find x s) with Not_found -> `Var x
     end) 
    () 
    x

let occurs : subst -> string -> Ast.term -> bool = fun s x t ->
  try 
    ignore (
      GT.transform(Ast.term) 
         (object inherit [Ast.term] @Ast.term[gmap]
            method c_Var _ v y = 
              if y = x 
	      then raise Not_found
	      else v.GT.x
          end)
         ()
         (walk s t)
    );
    false
  with Not_found -> true

let rec unify : subst option -> Ast.term -> Ast.term -> subst option = fun s x y ->
  let extend s x t =
    if occurs s x t then None else Some (M.add x t s)
  in
  match s with
  | None   -> None
  | Some s ->
      let x, y = walk s x, walk s y in
      match x, y with
      | `Var a, `Var b -> if a = b then Some s else extend s a y
      | `Var a, _ -> extend s a y
      | _, `Var b -> extend s b x
      | `Functor (fa, ta), `Functor (fb, tb) ->
         if (fa = fb) && (List.length ta = List.length tb)
	 then 
	   let rec inner s = function
	   | [], []       -> s
	   | a::ta, b::tb -> inner (unify s a b) (ta, tb)
	   in
	   inner (Some s) (ta, tb)
	 else None

let apply : subst -> Ast.body_item -> Ast.body_item = 
  let walk_atom subst = function
  | `Functor (f, terms) -> `Functor (f, List.map (walk' subst) terms)
  in
  fun s -> function
  | `Cut -> `Cut
  | #Ast.atom as a -> walk_atom s a
