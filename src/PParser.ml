open Ostap
   
module Lexer =
  struct

    let keywords = ["quit"; "clear"; "show"; "unify"; "help"; "load"; "trace"; "on"; "off"; "depth"; "factor"]

    let r = Ostap.Matcher.Token.repr

    let is_keyword = 
      let module S = Set.Make (String) in
      let s = List.fold_left (fun s k -> S.add k s) S.empty keywords in
      (fun i -> S.mem i s)     

    ostap (
      ident  : x:LIDENT  => {not (is_keyword x)}=> {x};
      var    : x:UIDENT  => {not (is_keyword x)}=> {x};
      literal: x:DECIMAL {x}; 
      string : x:STRING  {x} 
    )

  end

let wildvar =
  let i = ref 0 in
  fun () ->
    incr i;
    `Var (Printf.sprintf "_%d" !i)

ostap (
  ident     : !(Lexer.ident);
  var       : !(Lexer.var);
  string    : !(Lexer.string);
  literal   : !(Lexer.literal);
  key[name] : @(name ^ "\\b" : name);
  term      : 
    x:var {`Var x} 
  | "_"   {wildvar ()}
  | f:ident a:(-"(" !(Ostap.Util.list term) -")")? {
      `Functor (f, match a with Some a -> a | None -> [])
    }
  | "[" "]" {`Functor ("[]", [])}
  | "[" es:!(Ostap.Util.list term) t:(-"|" term)? "]" {
      List.fold_right 
        (fun e l -> `Functor ("::", [e; l])) 
        es
        (match t with None -> `Functor ("[]", []) | Some t -> t) 
    }
)

ostap (
  atom : f:ident a:(-"(" !(Ostap.Util.list term) -")")? {
    `Functor (f, match a with Some a -> a | None -> [])
  }
)

ostap (
body     : l:!(Ostap.Util.list (ostap(atom | "!" {`Cut} | t1:term "/=" t2:term {`Diseq (t1, t2)}))) {`Body l};
clause   : l:atom r:(-":-" body)? "." {
    match r with Some r -> `Clause (l, r) | _ -> `Clause (l, `Body [])
  }   
)

let help =
  "Commands:\n" ^
  "   help                --- print this help\n" ^
  "   quit                --- quit the interpreter\n" ^
  "   clear               --- remove all definitions\n" ^
  "   show                --- show current definitions\n" ^
  "   unify <term> <term> --- unify given terms\n" ^
  "   load  <string>      --- load definitions from file\n" ^
  "   trace [on | off]    --- enable/disable tracing\n" ^
  "   depth <positive>    --- enable iterative deepening with given\n" ^
  "                           initial depth\n" ^
  "   factor <positive>   --- set multiplicative factor for iterative\n" ^
  "                           deepening (default 2)\n" ^
  "   depth off           --- disable iterative deepening\n" ^                           
  "   depth               --- show current iterative deepening settings\n" ^
  "   <clause>            --- add a clause to current definitions\n" ^
  "   ? <query>           --- run a query\n"
ostap (
  main: i:item? EOF {match i with Some i -> i | _ -> `Empty};
  item:
    key["quit"]                    {`Quit}
  | key["help"]                    {`Help}
  | key["clear"]                   {`Clear}
  | key["show"]                    {`Show}
  | key["unify"] x:term y:term     {`Unify x y}
  | key["load"]  s:string          {`Load s}
  | key["trace"] key["on"]         {`TraceOn}
  | key["trace"] key["off"]        {`TraceOff}
  | key["factor"] n:literal        {`Factor n}
  | key["depth"] n:literal         {`Depth n}
  | key["depth"] key["off"]        {`DepthOff}
  | key["depth"]                   {`DepthShow}
  | c:clause                       {`Clause c}
  | "?" b:body                     {`Query b};
  spec: clause+ -EOF
)
