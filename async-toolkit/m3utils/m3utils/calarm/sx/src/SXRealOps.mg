(* $Id$ *)

GENERIC MODULE SXRealOps(Elem, Elem_ElemFuncOps);

PROCEDURE DivB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a/b END DivB;

PROCEDURE Div(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,DivB,"Div") END Div;

PROCEDURE SumB(READONLY a : ARRAY OF Elem.Base) : Elem.Base =
  VAR 
    s : Elem.Base := FLOAT(0,Elem.Base); 
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      s := s + a[i]
    END;
    RETURN s
  END SumB;
    
PROCEDURE ProdB(READONLY a : ARRAY OF Elem.Base) : Elem.Base =
  VAR 
    s : Elem.Base := FLOAT(1,Elem.Base); 
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      s := s * a[i]
    END;
    RETURN s
  END ProdB;
    
BEGIN END SXRealOps.
