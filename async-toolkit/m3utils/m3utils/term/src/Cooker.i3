INTERFACE Cooker;
IMPORT TextList;

(* expects you've already done "Term.MakeRaw(TRUE)" *)

TYPE
  Completer = OBJECT METHODS do(VAR input: TEXT); END;

PROCEDURE Input(prompt:=">"; c: Completer := NIL;
                previous: TextList.T := NIL; default:=""): TEXT;

PROCEDURE Print(t:="");

END Cooker. 
