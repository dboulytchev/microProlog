open GT
open Ostap
   
exception User_interrupt
exception Error of Ostap.Msg.t

let parse p s =
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.string s             
       inherit Util.Lexers.lident PParser.Lexer.keywords s
       inherit Util.Lexers.uident PParser.Lexer.keywords s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n\r";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip.nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (p -EOF))

let _ =
  Printf.printf "MicroProlog. (C) Dmitry Boulytchev, SPbSU, 2016-2025.\n";
  Printf.printf "Type \"help\" to see the other commands.\n";
  Printf.printf "Hit \"Ctrl-C\" to break infinite search.\n";
  ignore @@ Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> raise User_interrupt));
  let env = new PEnv.c in
  let doCommand = function
  | `TraceOn     -> env#trace_on
  | `TraceOff    -> env#trace_off
  | `Factor n    -> env#set_factor n
  | `Depth n     -> env#set_depth n
  | `DepthOff    -> env#unset_depth
  | `DepthShow   -> Printf.printf "%s\n" env#show_depth
  | `Empty       -> () 
  | `Quit        -> exit 0
  | `Clear       -> env#clear
  | `Clause c    -> env#add c 
  | `Show        -> env#show
  | `Help        -> Printf.printf "%s" PParser.help
  | `Load f -> 
      (match parse PParser.spec (Util.read f) with
       | `Ok clauses -> List.iter env#add clauses 
       | `Fail m     -> Printf.printf "Syntax error: %s\n" m
      )
  | `Unify (x, y) -> 
      Printf.printf "%s\n" 
	(Ostap.Pretty.toString (Unify.pretty_subst (Unify.unify (Some Unify.empty) x y)))
  | `Query (`Body goal) ->
      let vars = Ast.S.elements @@ Ast.vars goal in 
      let rec iterate conf =
        match SLD.solve env conf with
        | `End -> Printf.printf "No (more) answers.\n%!"
        | `Answer (s, ctor, conf) ->
	    (match vars with
	     | [] -> Printf.printf "yes\n"
	     | _  -> 
		List.iter 
		   (fun x ->
                      let term  = Unify.apply s (`Var x)                       in
                      let sterm = Ostap.Pretty.toString (Ast.pretty_term term) in
                      let fv    = Ast.fv term                                  in
                      let ctor  = SLD.reify_ctor ctor fv                       in
                      match ctor with 
		      | []  -> Printf.printf "%s = %s\n" x sterm
                      | _   -> Printf.printf "%s = %s (%s)\n" x sterm
                                 (Ostap.Pretty.toString @@ SLD.pretty_ctor ctor)
                   ) 
		   vars
            );
            Printf.printf "Continue (y/n)? ";
            let a = read_line () in
	    if a = "y" || a = "Y" then iterate conf
      in iterate ([0, SLD.extend goal, Unify.empty, [], env#clauses], [])
  in
  while true do
    try
      Printf.printf "> ";
      match parse PParser.main (read_line ()) with
      | `Ok command -> doCommand command
      | `Fail m     -> Printf.printf "Syntax error: %s\n" m
    with
    | User_interrupt -> Printf.printf "Interrupted\n"
    | exc -> Printf.printf "%s\n" (Printexc.to_string exc)
  done

