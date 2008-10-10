(* $Id$ *)


(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)
INTERFACE SchemeClass;
IMPORT Scheme, SchemeInputPort, Wr;
IMPORT SchemeJailBreak, SchemeM3TableOps;

REVEAL Scheme.T <: Private;

TYPE
  Private = Scheme.Public OBJECT
    input : SchemeInputPort.T;
    output : Wr.T;
    
    jailBreak : SchemeJailBreak.T := NIL;
    m3TableOps : SchemeM3TableOps.T := NIL;
  METHODS
    setTableOps(to : SchemeM3TableOps.T);
  END;

END SchemeClass.
