(* $Id: SXIntOps.mg,v 1.4 2010/11/05 12:52:16 mika Exp $ *)

GENERIC MODULE SXIntOps(Elem,Elem_ElemFuncOps);

PROCEDURE DivB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a DIV b END DivB;

PROCEDURE Div(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,DivB,"Div") END Div;

BEGIN 
  Zero   :=  0; 
  One    :=  1;
  NegOne := -1
END SXIntOps.
