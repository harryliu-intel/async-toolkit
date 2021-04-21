INTERFACE HTML;
IMPORT Session;

TYPE
  Stuff       = REFANY;
  StuffVector = REF ARRAY OF Stuff;
  StuffMatrix = REF ARRAY OF Matrix;

  T <: Public;
  Vector = REF ARRAY OF T;
  Matrix = REF ARRAY OF ARRAY OF T;

TYPE

  (* all others inherit from this *)
  Public = OBJECT
  METHODS
    init(session : Session.T; up : T := NIL) : T ; (*do we want session here*)
    getSession() : Session.T;
    format() : TEXT;
  END;

(* print an error page.  if doQuit is TRUE, then abort the whole CGI *)

PROCEDURE Error(reason : TEXT; doQuit : BOOLEAN := FALSE);

(* the following procedures are used for wrapping TEXTs, ARRAYs of TEXTs, 
   and ARRAYs of ARRAYs of TEXTs *)
PROCEDURE Wrap(stuff : Stuff) : T;
PROCEDURE WrapVector(stuff : REFANY) : Vector;
PROCEDURE WrapMatrix(stuff : REFANY) : Matrix;

CONST Brand = "HTML";
      
END HTML.
