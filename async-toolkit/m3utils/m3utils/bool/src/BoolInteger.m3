(* $Id$ *)

MODULE BoolInteger;
IMPORT Bool, BoolSet, Word, BoolSetDef;

TYPE Array = REF ARRAY OF Bool.T;

REVEAL 
  T = Public BRANDED Brand OBJECT
    (* two's complement *)
    bits : Array; (* CONST *)
  METHODS
    (* extend to width *)
    extend(nbits : CARDINAL) := SignExtend;

    (* pack into minimum req'd width; leave a "guard bit" so we know
       the sign of the number *)
    pack() := Pack;
  OVERRIDES
    getMinValue := GetMinValue;
    getMaxValue := GetMaxValue;
    isConstant := IsConstant;
    extract := Extract;
    init := Init;
    isEqual := IsEqual;
  END;

(* inefficient but simple first impl. *)
PROCEDURE IsEqual(a : T; n : INTEGER) : Bool.T = 
  BEGIN RETURN Equals(a, Constant(n)) END IsEqual;

PROCEDURE Init(res: T; maxValue : CARDINAL) : T =
  VAR
    bits : CARDINAL;
    inrange : Bool.T;
  BEGIN
    FOR i := 0 TO Word.Size - 1 DO
      IF Word.Shift(1,i) > maxValue THEN bits := i; EXIT END
    END;
    <* ASSERT bits < Word.Size *>

    (* make room for the sign bit *)
    INC(bits);

    res := NEW(T, bits := NEW(Array, bits));
    
    (* a bit goofy...

       first we make a bits-bit number with new bools in it.

       then we build the inrange expression

       then we And the MSB with the inrange expression

       We end up with a number that must be in range!
    *)

    FOR i := 0 TO bits - 2 DO res.bits[i] := Bool.New() END;

    res.bits[bits-1] := Bool.False();

    inrange := LessThanOrEqual(res,Constant(maxValue));

    res.bits[bits-2] := Bool.And(res.bits[bits-2],inrange);
    
    <* ASSERT GreaterThan(res, Constant(maxValue)) = Bool.False() AND
              LessThan(res, Zero) = Bool.False() *>
  
    RETURN res
  END Init;

PROCEDURE GetMinValue(a : T) : INTEGER =
  VAR
    r : CARDINAL;
  BEGIN
    a.pack();
    r := Word.Shift(1,NUMBER(a.bits^));
    FOR i := -r TO r DO
      IF Equals(Constant(i),a) # Bool.False() THEN RETURN i END
    END;
    <* ASSERT FALSE *>
  END GetMinValue;

PROCEDURE GetMaxValue(a : T) : INTEGER =
  VAR
    r : CARDINAL;
  BEGIN
    a.pack();
    r := Word.Shift(1,NUMBER(a.bits^));
    FOR i := r TO -r BY -1 DO
      IF Equals(Constant(i),a) # Bool.False() THEN RETURN i END
    END;
    <* ASSERT FALSE *>
  END GetMaxValue;

PROCEDURE IsConstant(a : T) : BOOLEAN = 
  BEGIN
    FOR i := FIRST(a.bits^) TO LAST(a.bits^) DO
      IF a.bits[i] # Bool.False() AND a.bits[i] # Bool.True() THEN 
        RETURN FALSE 
      END
    END;
    RETURN TRUE
  END IsConstant;

PROCEDURE Extract(a : T; bit : CARDINAL) : Bool.T = 
  BEGIN
    IF bit < NUMBER(a.bits^) THEN 
      RETURN a.bits[bit]
    ELSE
      RETURN a.bits[LAST(a.bits^)]
    END
  END Extract;

PROCEDURE SignExtend(self : T; nbits : CARDINAL) =
  VAR
    newBits := NEW(Array, nbits);
    top := self.bits[LAST(self.bits^)];
  BEGIN
    self.pack();
    <* ASSERT nbits >= NUMBER(self.bits^) *>
    SUBARRAY(newBits^,0,NUMBER(self.bits^)) := self.bits^;
    FOR i := LAST(self.bits^) + 1 TO LAST(newBits^) DO
      newBits[i] := top
    END;
    self.bits := newBits;
    <* ASSERT top = self.bits[LAST(self.bits^)] *>
  END SignExtend;

PROCEDURE Pack(self : T) =
  VAR
    top := self.bits[LAST(self.bits^)];
    msb : CARDINAL := LAST(self.bits^);
  BEGIN
    (* find largest necessary index *)
    FOR i := LAST(self.bits^) - 1 TO FIRST(self.bits^) BY -1 DO
      IF self.bits[i] # top THEN
        msb := i + 1;
        EXIT
      END
    END;
    IF msb # LAST(self.bits^) THEN
      VAR
        new := NEW(Array, msb + 1);
      BEGIN
        <* ASSERT NUMBER(new^) < NUMBER(self.bits^) *>
        new^ := SUBARRAY(self.bits^,FIRST(self.bits^), NUMBER(new^));
        <* ASSERT self.bits[LAST(self.bits^)] = new[LAST(new^)] *>
        self.bits := new
      END
    END;
    <* ASSERT top = self.bits[LAST(self.bits^)] *>
  END Pack;

PROCEDURE Add(a, b : T) : T =
  VAR
    bits := MAX(NUMBER(a.bits^),NUMBER(b.bits^)) + 2;
    res := NEW(T, bits := NEW(Array, bits));
    carry := Bool.False();
  BEGIN
    (* extend operands *)
    a.extend(bits); b.extend(bits);

    (* do the math *)
    FOR i := FIRST(res.bits^) TO LAST(res.bits^) DO
      res.bits[i] := Bool.Xor(Bool.Xor(carry,a.bits[i]),b.bits[i]);
      carry := Bool.Or(Bool.And(carry,a.bits[i]),Bool.Or(
                       Bool.And(carry,b.bits[i]),
                       Bool.And(a.bits[i],b.bits[i])))
    END;

    (* pack results *)
    a.pack(); b.pack(); res.pack();
    RETURN res
  END Add;

PROCEDURE Neg(a : T) : T =
  VAR
    res := NEW(T, bits := NEW(Array, NUMBER(a.bits^)));
  BEGIN
    FOR i := FIRST(a.bits^) TO LAST(a.bits^) DO
      res.bits[i] := Bool.Not(a.bits[i])
    END;
    RETURN Add(One,res)
  END Neg;
    
PROCEDURE Minus(a, b : T) : T =
  VAR
    bminus := Neg(b);
  BEGIN
    RETURN Add(a,bminus)
  END Minus;

PROCEDURE Equals(a, b : T) : Bool.T = 
  VAR
    t := Bool.True();
    max := MAX(NUMBER(a.bits^),NUMBER(b.bits^));
  BEGIN
    a.extend(max); b.extend(max);
    FOR i := 0 TO max - 1 DO
      t := Bool.And(t, Bool.Equivalent(a.bits[i],b.bits[i]))
    END;
    RETURN t
  END Equals;

PROCEDURE LessThanZero(a : T) : Bool.T =
  BEGIN RETURN a.bits[LAST(a.bits^)] END LessThanZero;

PROCEDURE GreaterThan(a, b : T) : Bool.T =
  BEGIN RETURN LessThanZero(Minus(b,a)) END GreaterThan;

PROCEDURE LessThan(a, b : T) : Bool.T =
  BEGIN RETURN LessThanZero(Minus(a,b)) END LessThan;

PROCEDURE GreaterThanOrEqual(a, b : T) : Bool.T =
  BEGIN 
    RETURN Bool.Or(LessThanZero(Minus(b,a)),Equals(a,b)) 
  END GreaterThanOrEqual;

PROCEDURE LessThanOrEqual(a, b : T) : Bool.T =
  BEGIN 
    RETURN Bool.Or(LessThanZero(Minus(a,b)),Equals(a,b)) 
  END LessThanOrEqual;

PROCEDURE Vars(a : T) : BoolSet.T = 
  VAR
    s := NEW(BoolSetDef.T).init();
  BEGIN
    FOR i := FIRST(a.bits^) TO LAST(a.bits^) DO
      EVAL s.unionD(Bool.Vars(a.bits[i]))
    END;
    RETURN s
  END Vars;

PROCEDURE BitwiseOp(a, b : T; op : PROCEDURE(a, b : Bool.T) : Bool.T) : T =
  VAR
    bits := MAX(NUMBER(a.bits^),NUMBER(b.bits^));
    res := NEW(T, bits := NEW(Array, bits));
  BEGIN
    a.extend(bits); b.extend(bits);
    FOR i := 0 TO bits - 1 DO
      res.bits[i] := op(a.bits[i],b.bits[i])
    END;
    res.pack(); a.pack(); b.pack();
    RETURN res
  END BitwiseOp;

PROCEDURE BitwiseAnd(a, b : T) : T =
  BEGIN RETURN BitwiseOp(a,b,Bool.And) END BitwiseAnd;

PROCEDURE BitwiseOr(a, b : T) : T =
  BEGIN RETURN BitwiseOp(a,b,Bool.Or) END BitwiseOr;

PROCEDURE BitwiseXor(a, b : T) : T =
  BEGIN RETURN BitwiseOp(a,b,Bool.Xor) END BitwiseXor;

PROCEDURE BitwiseNot(a : T) : T =
  BEGIN RETURN BitwiseOp(a,MinusOne,Bool.Xor) END BitwiseNot;

PROCEDURE ShiftLeft(a : T; sa : CARDINAL) : T =
  VAR
    bits := NUMBER(a.bits^) + sa;
    res := NEW(T, bits := NEW(Array, bits));
  BEGIN
    FOR i := 0 TO sa - 1 DO
      res.bits[i] := Bool.False();
    END;
    FOR i := sa TO bits DO
      res.bits[i] := a.bits[i - sa]
    END;
    RETURN res
  END ShiftLeft;

PROCEDURE UnsignedShiftRight(a : T; sa : CARDINAL) : T =
  VAR
    bits := NUMBER(a.bits^) - sa + 1;
    res := NEW(T, bits := NEW(Array, bits));
  BEGIN
    FOR i := 0 TO bits - 2 DO
      res.bits[i] := a.bits[i+sa]
    END;
    res.bits[bits - 1] := Bool.False();
    RETURN res
  END UnsignedShiftRight;

PROCEDURE SignedShiftRight(a : T; sa : CARDINAL) : T =
  VAR
    bits := NUMBER(a.bits^) - sa;
    res := NEW(T, bits := NEW(Array, bits));
  BEGIN
    FOR i := 0 TO bits - 1 DO
      res.bits[i] := a.bits[i+sa]
    END;
    RETURN res
  END SignedShiftRight;

PROCEDURE Constant(c : INTEGER) : T =
  VAR
    w : Word.T := c;
    res := NEW(T, bits := NEW(Array,Word.Size));
  BEGIN
    FOR i := 0 TO Word.Size - 1 DO 
      IF Word.Extract(w,i,1) = 1 THEN
        res.bits[i] := Bool.True()
      ELSE
        res.bits[i] := Bool.False()
      END
    END;
    res.pack();
    RETURN res
  END Constant;

(* dumb equal *)
PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Hash(a : T) : Word.T =
  VAR
    w := 0;
  BEGIN
    FOR i := FIRST(a.bits^) TO LAST(a.bits^) DO
      w := Word.Plus(Bool.Hash(a.bits[i]),w)
    END;
    RETURN w
  END Hash;

BEGIN 
  MinusOne := Constant(-1);
  One := Constant(1);
  Zero := Constant(0);
END BoolInteger.
