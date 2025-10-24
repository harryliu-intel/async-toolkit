(* $Id: FactorialValues.m3,v 1.5 2009/11/27 13:31:26 mika Exp $ *)

MODULE FactorialValues;
IMPORT Fmt;
IMPORT TextUtils;

REVEAL
  T = Public BRANDED Brand OBJECT
  METHODS
    check(idx : CARDINAL) RAISES { IndexOutOfBounds } := Check;
  END;

  Int = PubInt BRANDED OBJECT
  OVERRIDES
    formatV := FI;
    toLongreal := LI;
  END;

  Bool = PubBool BRANDED OBJECT  
  OVERRIDES
    formatV := FB;
    toLongreal := LBT;
  END;

  LR = PubLR BRANDED OBJECT
  OVERRIDES
    formatV := FL;
    toLongreal := LL;
  END;

  TX = PubTX BRANDED OBJECT 
  OVERRIDES
    formatV := FT;
    toLongreal := LBT;
  END;

PROCEDURE Check(t : T; idx : CARDINAL) RAISES { IndexOutOfBounds } =
  BEGIN
    IF idx >= t.n THEN RAISE IndexOutOfBounds END
  END Check;

PROCEDURE LI(i : Int; idx : CARDINAL) : LONGREAL RAISES { IndexOutOfBounds } =
  BEGIN i.check(idx); RETURN FLOAT(i.v[idx],LONGREAL) END LI;

PROCEDURE LL(i : LR; idx : CARDINAL) : LONGREAL RAISES { IndexOutOfBounds } =
  BEGIN i.check(idx); RETURN i.v[idx] END LL;

PROCEDURE LBT(i : T; idx : CARDINAL) : LONGREAL RAISES { IndexOutOfBounds } =
  BEGIN i.check(idx); RETURN FLOAT(idx,LONGREAL) END LBT;

PROCEDURE FI(i : Int; idx : CARDINAL) : TEXT RAISES { IndexOutOfBounds } =
  BEGIN i.check(idx); RETURN Fmt.Int(i.v[idx]) END FI;

PROCEDURE FB(i : Bool; idx : CARDINAL) : TEXT RAISES { IndexOutOfBounds } =
  BEGIN i.check(idx); RETURN Fmt.Bool(i.v[idx]) END FB;

PROCEDURE FL(i : LR; idx : CARDINAL) : TEXT RAISES { IndexOutOfBounds } =
  BEGIN i.check(idx); RETURN Fmt.LongReal(i.v[idx]) END FL;

PROCEDURE FT(i : TX; idx : CARDINAL) : TEXT RAISES { IndexOutOfBounds } =
  BEGIN i.check(idx); RETURN "\"" & TextUtils.Replace(i.v[idx],"\n","\\n") & "\"" END FT;

PROCEDURE TypeOf(t : T) : Type =
  BEGIN
    TYPECASE t OF
      Int  => RETURN Type.Int
    | 
      Bool=> RETURN Type.Bool 
    |
      LR => RETURN Type.LR
    |
      TX  => RETURN Type.Text
    ELSE
      <* ASSERT FALSE *>
    END
  END TypeOf;

BEGIN END FactorialValues.
