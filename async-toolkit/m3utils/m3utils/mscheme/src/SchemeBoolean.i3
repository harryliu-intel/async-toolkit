(* $Id: SchemeBoolean.i3,v 1.7 2009/11/28 12:11:34 mika Exp $ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeBoolean;
IMPORT SchemeObject;

TYPE T <: REFANY;

PROCEDURE Truth(x : BOOLEAN) : T;

CONST FromBool = Truth;

PROCEDURE TruthO(x : SchemeObject.T) : BOOLEAN;
  (* x # False() *)

PROCEDURE True() : T; 
  (* constant TRUE *)

PROCEDURE False() : T; 
  (* constant FALSE *)

CONST Brand = "SchemeBoolean";

END SchemeBoolean.
