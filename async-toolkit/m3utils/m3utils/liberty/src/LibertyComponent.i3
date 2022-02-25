INTERFACE LibertyComponent;
IMPORT Wr, Thread;
IMPORT Word;
IMPORT Refany;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    write(wr : Wr.T; lineStart := "") RAISES { Wr.Failure, Thread.Alerted };
    getId() : CARDINAL;
  END;

CONST Brand = "LibertyComponent";

PROCEDURE Hash(t : T) : Word.T;

CONST Equal = Refany.Equal;

END LibertyComponent.
