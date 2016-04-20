open GT
open Checked

let _ = 
  let database =
    let clauses : Ast.clause list ref = ref [] in
    object
      method add (c : Ast.clause) = clauses := c :: !clauses
      method show =
	List.iter (fun c -> Printf.printf "%s\n" (Ostap.Pretty.toString (Ast.pretty_clause c))) !clauses
      method clear = clauses := []
    end
  in
  let doCommand = function
  | `Quit         -> exit 0
  | `Clear        -> database#clear
  | `Clause c     -> database#add c
  | `Show         -> database#show
  | `Unify (x, y) -> 
      Printf.printf "%s\n" 
	(Ostap.Pretty.toString (Unify.pretty_subst (Unify.unify Unify.empty x y)))
  | `Query  _     -> ()
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

