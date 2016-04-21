open GT
open Checked

let _ = 
  let database = new Database.c in
  let doCommand = function
  | `Quit         -> exit 0
  | `Clear        -> database#clear
  | `Clause c     -> database#add c
  | `Show         -> database#show
  | `Unify (x, y) -> 
      Printf.printf "%s\n" 
	(Ostap.Pretty.toString (Unify.pretty_subst (Unify.unify (Some Unify.empty) x y)))
  | `Query goal   -> 
       let rec iterate stack =
	 match SLD.solve database stack with
	 | `End -> Printf.printf "No (more) answers.\n"
	 | `Answer (s, stack) ->
	     Printf.printf "%s\n" (Ostap.Pretty.toString (Unify.pretty_subst (Some s)));
	     Printf.printf "Continue (y/n)? ";
             let a = read_line () in
	     if a = "y" || a = "Y" then iterate stack
       in iterate [goal, Unify.empty]
  in
  while true do
    Printf.printf "> ";
    match Parser.Lexer.fromString Parser.main (read_line ()) with
    | Ok command -> doCommand command
    | Fail (m::_) -> 
	(match Ostap.Msg.loc m with
	 | Ostap.Msg.Locator.Point (1, n) -> 
             Printf.printf "%s^\n" (String.make (n-1) ' ')
	 | _ -> ()
	);
	Printf.printf "Syntax error: %s\n" (Ostap.Msg.toString m)
  done

