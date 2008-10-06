(* $Id$ *)

INTERFACE SchemeClass;
IMPORT Scheme, SchemeInputPort, Wr;
IMPORT SchemeJailBreak;

REVEAL Scheme.T <: Private;

TYPE
  Private = Scheme.Public OBJECT
    input : SchemeInputPort.T;
    output : Wr.T;
    
    jailBreak : SchemeJailBreak.T := NIL;
  END;

END SchemeClass.
