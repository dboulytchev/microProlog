@type term = [
  | `Var     of GT.string 
  | `Functor of GT.string * term GT.list
] with gmap, foldl, show

@type atom = [ `Functor of GT.string * term GT.list ] with gmap, foldl, show

@type body_item = [ atom | `Cut ] with gmap, foldl, show

@type body = [ `Body of body_item GT.list ] with gmap, foldl, show

@type clause = [ `Clause of atom * body ] with gmap, foldl, show

let to_term : atom -> term = fun a -> (a :> term)

class pretty_term =
  object inherit [unit, Ostap.Pretty.printer] @term
    method c_Var     _ _ s = Ostap.Pretty.string s
    method c_Functor _ s f ts = 
      Ostap.Pretty.listBySpace [
        Ostap.Pretty.string f;
        match ts with
        | [] -> Ostap.Pretty.empty
        | ys -> Ostap.Pretty.rboxed (Ostap.Pretty.listByComma (List.map (s.GT.f ()) ts))
      ]
  end

let pretty_term t = GT.transform(term) (new pretty_term) () t

class pretty_atom =
  object inherit [unit, Ostap.Pretty.printer] @atom
    method c_Functor _ s f ts = 
      Ostap.Pretty.listBySpace [
        Ostap.Pretty.string f;
        match ts with
        | [] -> Ostap.Pretty.empty
        | ys -> Ostap.Pretty.rboxed (Ostap.Pretty.listByComma (List.map pretty_term ts))
      ]        
  end

let pretty_atom a = GT.transform(atom) (new pretty_atom) () a

class pretty_body_item =
  object inherit [unit, Ostap.Pretty.printer] @body_item
         inherit pretty_atom
    method c_Cut _ _ = Ostap.Pretty.string "!"
  end

let pretty_body_item i = GT.transform(body_item) (new pretty_body_item) () i

class pretty_body =
  object inherit [unit, Ostap.Pretty.printer] @body
    method c_Body _ _ bs = Ostap.Pretty.listByComma (List.map pretty_body_item bs)
  end

let pretty_body b = GT.transform(body) (new pretty_body) () b

class pretty_clause =
  object inherit [unit, Ostap.Pretty.printer] @clause
    method c_Clause _ _ a ((`Body bs) as b) = 
      (fun a ->
         match bs with
         | [] -> a
         | _  -> Ostap.Pretty.listBySpace [a; Ostap.Pretty.string ":-"; pretty_body b]
      ) (pretty_atom a)
  end

let pretty_clause c = GT.transform(clause) (new pretty_clause) () c
