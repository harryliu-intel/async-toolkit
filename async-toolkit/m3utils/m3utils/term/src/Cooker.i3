INTERFACE Cooker;
IMPORT TextList;

TYPE
  Completer = OBJECT METHODS do(VAR input: TEXT); END;

PROCEDURE Input(prompt:="> ";
                completer: Completer := NIL;
                previous: TextList.T := NIL;
                default:="";
                fatalControl:=TRUE;
                emptySelectsLast:=FALSE): TEXT;

PROCEDURE Print(t:="");

END Cooker. 
