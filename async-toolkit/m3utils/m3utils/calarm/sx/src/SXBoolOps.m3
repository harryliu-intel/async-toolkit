(* $Id$ *)

MODULE SXBoolOps;
IMPORT SXBool AS Bool;
IMPORT SXBool_BoolFuncOps;

PROCEDURE NotB(a : BOOLEAN) : BOOLEAN =
  BEGIN RETURN NOT a END NotB;

PROCEDURE AndB  (a, b : BOOLEAN) : BOOLEAN =
  BEGIN RETURN a AND b END AndB;

PROCEDURE OrB   (a, b : BOOLEAN) : BOOLEAN =
  BEGIN RETURN a OR b END OrB;

PROCEDURE XorB  (a, b : BOOLEAN) : BOOLEAN =
  BEGIN RETURN a # b END XorB;

PROCEDURE EqualB  (a, b : BOOLEAN) : BOOLEAN =
  BEGIN RETURN a = b END EqualB;

(**********************************************************************)

PROCEDURE Not(a : Bool.T) : Bool.T =
  BEGIN RETURN SXBool_BoolFuncOps.UnaryFunc(a, NotB) END Not;

PROCEDURE Equal(a, b : Bool.T) : Bool.T =
  BEGIN RETURN SXBool_BoolFuncOps.BinaryFunc(a, b, EqualB) END Equal;

PROCEDURE And  (a, b : Bool.T) : Bool.T =
  BEGIN RETURN SXBool_BoolFuncOps.BinaryFunc(a, b, AndB) END And;

PROCEDURE Or   (a, b : Bool.T) : Bool.T =
  BEGIN RETURN SXBool_BoolFuncOps.BinaryFunc(a, b, OrB) END Or;

PROCEDURE Xor  (a, b : Bool.T) : Bool.T =
  BEGIN RETURN SXBool_BoolFuncOps.BinaryFunc(a, b, XorB) END Xor;

BEGIN END SXBoolOps.
