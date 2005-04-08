MODULE FmtScanVar;
IMPORT FloatMode;
IMPORT Fmt;
IMPORT Lex;
IMPORT Scan;




(*****************************************************************************
 *                                                                           *
 *                                   INTEGER                                 *
 *                                                                           *
 *****************************************************************************)

PROCEDURE Int(var: REF INTEGER; base: INTEGER) : T =
  BEGIN
    RETURN NEW(IntPrivate, var:=var, base:=base);
  END Int;

TYPE
  IntPrivate = T OBJECT
    var: REF INTEGER;
    base: CARDINAL;
  OVERRIDES
    fmt := FmtInt;
    scan := ScanInt;
  END;

PROCEDURE FmtInt(self: IntPrivate): TEXT =
  BEGIN
    RETURN Fmt.Int(self.var^, self.base);
  END FmtInt;

PROCEDURE ScanInt(self: IntPrivate; t: TEXT) RAISES {Error} =
  BEGIN
    TRY
      self.var^ := Scan.Int(t, self.base);
    EXCEPT Lex.Error, FloatMode.Trap =>
      RAISE Error;
    END;
  END ScanInt;




(*****************************************************************************
 *                                                                           *
 *                                   BOOLEAN                                 *
 *                                                                           *
 *****************************************************************************)


PROCEDURE Bool(var: REF BOOLEAN) : T =
  BEGIN
    RETURN NEW(BoolPrivate, var:=var);
  END Bool;

TYPE
  BoolPrivate = T OBJECT
    var: REF BOOLEAN;
  OVERRIDES
    fmt := FmtBool;
    scan := ScanBool;
  END;

PROCEDURE FmtBool(self: BoolPrivate): TEXT =
  BEGIN
    RETURN Fmt.Bool(self.var^);
  END FmtBool;

PROCEDURE ScanBool(self: BoolPrivate; t: TEXT) RAISES {Error} =
  BEGIN
    TRY
      self.var^ := Scan.Bool(t);
    EXCEPT Lex.Error =>
      RAISE Error;
    END;
  END ScanBool;




(*****************************************************************************
 *                                                                           *
 *                                  LONGREAL                                 *
 *                                                                           *
 *****************************************************************************)


PROCEDURE LongReal(var: REF LONGREAL; digits: CARDINAL; style: Fmt.Style) : T =
  BEGIN
    RETURN NEW(LongRealPrivate, var:=var, digits:=digits, style:=style);
  END LongReal;

TYPE
  LongRealPrivate = T OBJECT
    var: REF LONGREAL;
    digits: CARDINAL;
    style: Fmt.Style;
  OVERRIDES
    fmt := FmtLongReal;
    scan := ScanLongReal;
  END;

PROCEDURE FmtLongReal(self: LongRealPrivate): TEXT =
  BEGIN
    RETURN Fmt.LongReal(self.var^, self.style, self.digits);
  END FmtLongReal;

PROCEDURE ScanLongReal(self: LongRealPrivate; t: TEXT)
  RAISES {Error} =
  BEGIN
    TRY
      self.var^ := Scan.LongReal(t);
    EXCEPT Lex.Error, FloatMode.Trap =>
      RAISE Error;
    END;
  END ScanLongReal;


BEGIN
END FmtScanVar.
