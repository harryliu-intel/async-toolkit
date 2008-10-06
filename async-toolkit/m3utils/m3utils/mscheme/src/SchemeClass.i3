(* $Id$ *)


(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)
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
