INTERFACE Cooker;
IMPORT TextList;

(* expects you've already done "Term.MakeRaw(TRUE)" *)

TYPE
  Completer = OBJECT METHODS do(VAR input: TEXT); END;

PROCEDURE Input(prompt:=">";
                completer: Completer := NIL;
                previous: TextList.T := NIL;
                default:="";
                fatalControl:=TRUE): TEXT;

PROCEDURE Print(t:="");

END Cooker. 
