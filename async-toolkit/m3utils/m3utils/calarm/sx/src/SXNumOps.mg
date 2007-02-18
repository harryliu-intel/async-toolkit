(* $Id$ *)

GENERIC MODULE SXNumOps(Elem, Elem_ElemFuncOps, Elem_IntFuncOps, Elem_BoolFuncOps);
IMPORT SXBool;
IMPORT SXInt;

PROCEDURE TimesB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a*b END TimesB;

PROCEDURE ModB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a MOD b END ModB;

PROCEDURE PlusB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a+b END PlusB;

PROCEDURE MinusB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN a-b END MinusB;

PROCEDURE MaxB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN MAX(a,b) END MaxB;

PROCEDURE MinB(a, b : Elem.Base) : Elem.Base =
  BEGIN RETURN MIN(a,b) END MinB;

PROCEDURE GTB(a, b : Elem.Base) : BOOLEAN =
  BEGIN RETURN a>b END GTB;

PROCEDURE LTB(a, b : Elem.Base) : BOOLEAN =
  BEGIN RETURN a<b END LTB;

PROCEDURE GEB(a, b : Elem.Base) : BOOLEAN =
  BEGIN RETURN a>=b END GEB;

PROCEDURE LEB(a, b : Elem.Base) : BOOLEAN =
  BEGIN RETURN a<=b END LEB;

(**********************************************************************)

PROCEDURE Times(a, b : Elem.T) : Elem.T =
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,TimesB) END Times;

PROCEDURE Mod(a, b : Elem.T) : Elem.T =
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,ModB) END Mod;

PROCEDURE Plus(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,PlusB) END Plus;

PROCEDURE Minus(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,MinusB) END Minus;

PROCEDURE Min(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,MinB) END Min;

PROCEDURE Max(a, b : Elem.T) : Elem.T=
  BEGIN RETURN Elem_ElemFuncOps.BinaryFunc(a,b,MaxB) END Max;

PROCEDURE Equal(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, Elem.BaseEqual) END Equal;

PROCEDURE Compare(a, b : Elem.T) : SXInt.T =
  BEGIN RETURN Elem_IntFuncOps.BinaryFunc(a, b, Elem.BaseCompare) END Compare;

PROCEDURE GT(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, GTB) END GT;

PROCEDURE LT(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, LTB) END LT;

PROCEDURE GE(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, GEB) END GE;

PROCEDURE LE(a, b : Elem.T) : SXBool.T =
  BEGIN RETURN Elem_BoolFuncOps.BinaryFunc(a, b, LEB) END LE;


BEGIN END SXNumOps.
