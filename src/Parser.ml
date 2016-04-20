module Lexer =
  struct

    let keywords = ["quit"; "clear"; "show"]

    let r = Ostap.Matcher.Token.repr

    let is_keyword = 
      let module S = Set.Make (String) in
      let s = List.fold_left (fun s k -> S.add k s) S.empty keywords in
      (fun i -> S.mem i s)     

    ostap (
      ident  : x:IDENT =>{not (is_keyword (r x))}=> {r x};
      var    : x:VAR   =>{not (is_keyword (r x))}=> {r x};
      literal: x:LITERAL {int_of_string (r x)} 
    )

    class t s = 
      let skip = Ostap.Matcher.Skip.create [
                   Ostap.Matcher.Skip.whitespaces " \n\t\r"; 
                   Ostap.Matcher.Skip.nestedComment "(*" "*)";
                   Ostap.Matcher.Skip.lineComment "--"
                 ] 
      in

      let ident   = Re_str.regexp "[a-z]\([a-zA-Z_0-9]\)*\\b" in 
      let var     = Re_str.regexp "[A-Z]\([a-zA-Z_0-9]\)*\\b" in 
      let literal = Re_str.regexp "-?[0-9]+" in
      object (self)
        inherit Ostap.Matcher.t s 
        method skip p coord = skip s p coord
        method getIDENT     = self#get "identifier" ident
        method getVAR       = self#get "variable"   var
        method getLITERAL   = self#get "literal"    literal         
      end

    exception Error of Ostap.Msg.t

    let fromString p s =
      try
        Ostap.Combinators.unwrap (p (new t s)) (fun x -> Checked.Ok x) 
          (fun (Some err) ->
             let [loc, m :: _] = err#retrieve (`First 1) (`Desc) in
             let m =  match m with `Msg m -> m | `Comment (s, _) -> Ostap.Msg.make s [||] loc in
             Checked.Fail [m]
          )
      with Error m -> Checked.Fail [m]

  end

ostap (
  ident     : !(Lexer.ident);
  var       : !(Lexer.var);
  key[name] : @(name ^ "\\b" : name);
  term      : x:var {`Var x} | f:ident a:(-"(" !(Ostap.Util.list term) -")")? {
    `Functor (f, match a with Some a -> a | None -> [])
  }
)

ostap (
  atom : f:ident a:(-"(" !(Ostap.Util.list term) -")")? {
    `Functor (f, match a with Some a -> a | None -> [])
  }
)

ostap (
  body     : l:!(Ostap.Util.list (ostap(atom | "!" {`Cut}))) {`Body l};
  clause   : l:atom r:(-":-" body)? "." {
    match r with Some r -> `Clause (l, r) | _ -> `Clause (l, `Body [])
  }   
)

ostap (
  main: 
    key["quit"]                {`Quit}
  | key["clear"]               {`Clear}
  | key["show"]                {`Show}
  | key["unify"] x:term y:term {`Unify x y}
  | c:clause                   {`Clause c}
  | "?" a:atom                 {`Query a}
)
