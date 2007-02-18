(* $Id$ *)

GENERIC MODULE SXIntOps(Elem,Elem_ElemFuncOps);

PROCEDURE DivB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a DIV b END DivB;

PROCEDURE Div(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,DivB) END Div;

BEGIN END SXIntOps.
