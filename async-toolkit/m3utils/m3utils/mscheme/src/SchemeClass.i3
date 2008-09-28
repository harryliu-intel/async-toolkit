(* $Id$ *)

INTERFACE SchemeClass;
IMPORT Scheme, SchemeInputPort, Wr;

REVEAL Scheme.T <: Private;

TYPE
  Private = Scheme.Public OBJECT
    input : SchemeInputPort.T;
    output : Wr.T;
  END;

END SchemeClass.
