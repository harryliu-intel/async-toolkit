(* $Id$ *)

MODULE Main;
IMPORT BoolInteger;
IMPORT Debug,Fmt;

VAR
  a := NEW(BoolInteger.T).init(2);
BEGIN
  Debug.Out("IsConstant(Zero) = " & Fmt.Bool(BoolInteger.Zero.isConstant()));
  Debug.Out("a.getMinValue() = " & Fmt.Int(a.getMinValue()));
  Debug.Out("a.getMaxValue() = " & Fmt.Int(a.getMaxValue()));
  Debug.Out("(a+a).getMaxValue() = " & 
    Fmt.Int(BoolInteger.Add(a,a).getMaxValue()));
END Main.
