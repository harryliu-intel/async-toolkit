(* $Id$ *)

INTERFACE SchemeBoolean;

TYPE T = REF BOOLEAN;

PROCEDURE Truth(x : BOOLEAN) : T;

PROCEDURE TruthO(x : REFANY) : T;

PROCEDURE True() : T; 
  (* constant TRUE *)

PROCEDURE False() : T; 
  (* constant FALSE *)

CONST Brand = "SchemeBoolean";

END SchemeBoolean.
