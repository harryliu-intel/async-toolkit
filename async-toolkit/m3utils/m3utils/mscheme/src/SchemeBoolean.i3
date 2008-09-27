(* $Id$ *)

INTERFACE SchemeBoolean;
IMPORT SchemeObject;

TYPE T = REF BOOLEAN;

PROCEDURE Truth(x : BOOLEAN) : T;

PROCEDURE TruthO(x : SchemeObject.T) : T;

PROCEDURE True() : T; 
  (* constant TRUE *)

PROCEDURE False() : T; 
  (* constant FALSE *)

CONST Brand = "SchemeBoolean";

END SchemeBoolean.
