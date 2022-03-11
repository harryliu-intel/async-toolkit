INTERFACE LibertyComponent;
IMPORT Wr, Thread;
IMPORT Word;
IMPORT Refany;

TYPE
  T <: Public;

  Public = OBJECT
    parent : T;
  METHODS
    write(wr : Wr.T; lineStart := "") RAISES { Wr.Failure, Thread.Alerted };
    getId() : CARDINAL;

    makeParentLinks();
    getParent() : T;
  END;

CONST Brand = "LibertyComponent";

PROCEDURE Hash(t : T) : Word.T;

CONST Equal = Refany.Equal;

END LibertyComponent.
