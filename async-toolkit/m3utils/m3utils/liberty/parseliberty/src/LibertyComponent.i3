INTERFACE LibertyComponent;
IMPORT Wr, Thread;
IMPORT Word;
IMPORT Refany;

TYPE
  T <: Public;

  Public = OBJECT
    parent     : T;
    decoration : REFANY (* clients can use this for any purpose *); 
  METHODS
    write(wr : Wr.T; lineStart := "") RAISES { Wr.Failure, Thread.Alerted };
    getId() : CARDINAL;

    makeParentLinks();
    getParent() : T;

    format() : TEXT; (* mainly for debugging -- uses write above *)
  END;

CONST Brand = "LibertyComponent";

PROCEDURE Hash(t : T) : Word.T;

CONST Equal = Refany.Equal;

END LibertyComponent.
