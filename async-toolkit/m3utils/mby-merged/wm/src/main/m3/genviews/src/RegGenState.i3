INTERFACE RegGenState;
IMPORT TextSet, Pathname;
IMPORT Wr, Thread, OSError;

TYPE
  T <: Public;

  Public = OBJECT
    dirPath  : Pathname.T;
  METHODS
    newSymbol(nm : TEXT) : BOOLEAN RAISES { OSError.E, Thread.Alerted, Wr.Failure };
    (* call this before starting output of a new symbol.  
       if it returns false, that symbol has already been generated.
       up to you to ensure that it's the "same thing" (i.e., the two are 
       not attempting to generate actually-different objects of the same name *)

    initF(from : T) : T;
    (* copy from t, will share symbol table *)

    init(dirPath : Pathname.T) : T;
  END;

CONST Brand = "RegGenState";

END RegGenState.
  
