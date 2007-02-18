(* $Id$ *)

GENERIC MODULE SXRealOps(Elem, Elem_ElemFuncOps);

PROCEDURE DivB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a/b END DivB;

PROCEDURE Div(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,DivB) END Div;

BEGIN END SXRealOps.
