(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeBoolean;
IMPORT SchemeObject;
FROM Scheme IMPORT E;

TYPE T = REF BOOLEAN;

PROCEDURE Truth(x : BOOLEAN) : T;

PROCEDURE TruthO(x : SchemeObject.T) : T RAISES { E };

PROCEDURE True() : T; 
  (* constant TRUE *)

PROCEDURE False() : T; 
  (* constant FALSE *)

CONST Brand = "SchemeBoolean";

END SchemeBoolean.
