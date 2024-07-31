INTERFACE LibertyBoolean;
IMPORT Wr;
IMPORT Thread;

(* booleans must be special because #f is used heavily in the Scheme for
   "no such object" *)

TYPE
  T = { F, T };

CONST Brand = "LibertyBoolean";

PROCEDURE Write(t : T; wr : Wr.T; pfx : TEXT)
  RAISES { Wr.Failure, Thread.Alerted };

CONST True  = T.T;
      False = T.F;
  
END LibertyBoolean.
