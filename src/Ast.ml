@type term = [
| `Var     of GT.string 
| `Functor of GT.string * term GT.list
]               
with gmap, foldl, show

@type atom = [
| `Functor of GT.string * term GT.list
] with gmap, foldl, show

@type body_item = [ atom | `Cut | `Diseq of term * term ] with gmap, foldl, show

@type body = [ `Body of body_item GT.list ] with gmap, foldl, show

@type clause = [ `Clause of atom * body ] with gmap, foldl, show

let to_term : atom -> term = fun a -> (a :> term)
 
class pretty_term self =
  object inherit [unit, term, Ostap.Pretty.printer] @term 
    method c_Var _ _ s = Ostap.Pretty.string s
    method c_Functor s same f ts =
      match f with
      | "[]" -> Ostap.Pretty.string "[]"
      | "::" ->         
         let rec pull_out acc = function
         | `Functor ("::", [h; t]) -> pull_out (h :: acc) t
         | other -> (acc, other)
         in
         let prefix, tail = pull_out [] same in        
         Ostap.Pretty.sboxed (
           Ostap.Pretty.seq [
            Ostap.Pretty.listByComma (List.map (self s) (List.rev prefix));
            match tail with
            | `Functor ("[]", []) -> Ostap.Pretty.empty
            | _                   -> Ostap.Pretty.seq [Ostap.Pretty.string " | "; self s tail]
           ]
         )         
      | _    ->
         Ostap.Pretty.listBySpace [
           Ostap.Pretty.string f;
           match ts with
           | [] -> Ostap.Pretty.empty
           | ys -> Ostap.Pretty.rboxed (Ostap.Pretty.listByComma (List.map (self s) ts))
         ]        
  end

let pretty_term t = GT.transform(term) (new pretty_term) () t
                  
class ['a] pretty_atom =
  object inherit [unit, 'a, Ostap.Pretty.printer] @atom
    method c_Functor s _ f ts = 
      Ostap.Pretty.listBySpace [
        Ostap.Pretty.string f;
        match ts with
        | [] -> Ostap.Pretty.empty
        | ys -> Ostap.Pretty.rboxed (Ostap.Pretty.listByComma (List.map pretty_term ts))
      ]        
  end

let pretty_atom a = GT.transform(atom) (GT.lift @@ new pretty_atom) () a

class pretty_body_item =
  object inherit [unit, body_item, Ostap.Pretty.printer] @body_item
         inherit [body_item] pretty_atom
    method c_Diseq   s _ t1 t2 =
      Ostap.Pretty.listBySpace [
          pretty_term t1;
          Ostap.Pretty.string "/=";
          pretty_term t2;
      ]
    method c_Cut _ _ = Ostap.Pretty.string "!" 
  end

let pretty_body_item i = GT.transform(body_item) (GT.lift @@ new pretty_body_item) () i

class pretty_body =
  object inherit [unit, body, Ostap.Pretty.printer] @body
    method c_Body _ _ bs = Ostap.Pretty.listByComma (List.map pretty_body_item bs)
  end

let pretty_body b = GT.transform(body) (GT.lift @@ new pretty_body) () b

class pretty_clause =
  object inherit [unit, clause, Ostap.Pretty.printer] @clause
    method c_Clause _ _ a ((`Body bs) as b) = 
      (fun a ->
         match bs with
         | [] -> Ostap.Pretty.seq [a; Ostap.Pretty.string "."]
         | _  -> Ostap.Pretty.seq [
                   Ostap.Pretty.listBySpace [a; Ostap.Pretty.string ":-"; pretty_body b]; 
                   Ostap.Pretty.string "."
                 ]
      ) (pretty_atom a)
  end

let pretty_clause c = GT.transform(clause) (GT.lift @@ new pretty_clause) () c

module S = Set.Make (String)

let vars atoms =
  GT.foldl(GT.list) 
    (GT.transform(body_item)
       (fun self -> object inherit [S.t, _] @body_item[foldl] self
          method c_Functor s _ _ ts =
            GT.foldl(GT.list)
               (GT.transform(term) (fun self ->
		  (object inherit [S.t, _] @term[foldl] self
                     method c_Var s _ x = S.add x s
		   end))
	       )
               s
	       ts
        end
       )
    ) 
    S.empty 
    atoms

let fv = function
| `Var _ as x -> vars [`Functor ("", [x])]
| #atom  as t -> vars [t]
