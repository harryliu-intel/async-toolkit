MODULE LambModel;
IMPORT Lamb;
IMPORT X01;
IMPORT Bit;
IMPORT Valenv;
IMPORT Word;
IMPORT Dims;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Bool; IMPORT Fmt;

CONST LR = Fmt.LongReal;

REVEAL
  T = Public BRANDED OBJECT
    c    : Lamb.T;
    conf : REF ARRAY OF ARRAY OF X01.T;
    assertHoldFrac : LONGREAL;
    assertHoldTime : LONGREAL;
    cycleTime : LONGREAL;
  OVERRIDES
    init := Init;
    simStep := SimStep;
  END;

PROCEDURE Init(t : T;
               READONLY c : Lamb.T;
               cycleTime, assertHoldFrac, assertHoldTime : LONGREAL) : T = 
  BEGIN 
    t.c := c;
    <*ASSERT assertHoldFrac > -1.0d10 *>
    <*ASSERT assertHoldFrac < +1.0d10 *>
    <*ASSERT cycleTime > -1.0d10 *>
    <*ASSERT cycleTime < +1.0d10 *>

    t.assertHoldFrac := assertHoldFrac;
    t.assertHoldTime := assertHoldTime;
    t.cycleTime := cycleTime;
    t.conf := NEW(REF ARRAY OF ARRAY OF X01.T, 2*c.N, c.W);
    FOR i := FIRST(t.conf^) TO LAST(t.conf^) DO
      FOR j := FIRST(t.conf[i]) TO LAST(t.conf[i]) DO
        t.conf[i,j] := X01.T.VX
      END
    END;
    RETURN t 
  END Init;

PROCEDURE ToBool(x01 : X01.T) : BOOLEAN =
  BEGIN
    CASE x01 OF 
      X01.T.V0 => RETURN FALSE
    |
      X01.T.V1 => RETURN TRUE
    |
      X01.T.VX => <*ASSERT FALSE*>
    END
  END ToBool;

PROCEDURE ToCard(READONLY x : ARRAY OF X01.T) : CARDINAL =
  VAR
    w : Word.T := 0;
    b : Bit.T;
  BEGIN
    <*ASSERT NUMBER(x) <= BITSIZE(CARDINAL)-1*>
    FOR i := 0 TO NUMBER(x)-1 DO
      WITH x01 = x[i] DO
        CASE x01 OF 
          X01.T.V0 => b := 0
        |
          X01.T.V1 => b := 1
        |
          X01.T.VX => <*ASSERT FALSE*>
        END;
        w := Word.Insert(w, b, i, 1)
      END
    END;
    RETURN w
  END ToCard;

PROCEDURE FmtArry(READONLY a : ARRAY OF X01.T) : TEXT =
  VAR res := "";
  BEGIN
    FOR i := LAST(a) TO FIRST(a) BY -1 DO
      res := res & X01.Names[a[i]]
    END;
    RETURN res
  END FmtArry;

PROCEDURE SimStep(t : T; e : Valenv.T) =

  PROCEDURE C(nm : TEXT) : TEXT =
    BEGIN
      RETURN nm
    END C;
    
  VAR
    wen   :=     ToBool(e.getSngl(C("WEN"             )));
    ren   :=     ToBool(e.getSngl(C("REN"             )));
    radr  :=     ToCard(e.getArry(C("RADR"            ))^);
    wadr  :=     ToCard(e.getArry(C("WADR"            ))^);
    wdata :=            e.getArry(C("WDATA"           )  );

    q : BOOLEAN;

  PROCEDURE DoRen() =
    BEGIN
      WITH z = t.conf[radr] DO
        Produce(2, C("DOUT"), z, t.assertHoldFrac, t.assertHoldTime)
      END;
      Debug.Out(F("DoRen"));
      Debug.Out(F("radr %s", Int(radr)));
    END DoRen;

  PROCEDURE DoWen() =
    BEGIN
      t.conf[wadr] := wdata^;
      Debug.Out(F("DoWen"));
      Debug.Out(F("wadr  %s", Int(wadr)));
      Debug.Out(F("wdata %s", FmtArry(wdata^)));
    END DoWen;

  PROCEDURE Produce(dly : CARDINAL;
                    nm : TEXT;
                    READONLY val : ARRAY OF X01.T;
                    assertHoldFrac, assertHoldTime : LONGREAL) =
    BEGIN
      FOR i := FIRST(val) TO LAST(val) DO
        IF val[i] # X01.T.VX THEN
          Debug.Out(F("LambModel : t=%s clkOff %s : %s%s -> %s",
                      LR(e.getTime()),
                      Int(dly),
                      nm, Dims.Format(Dims.T { i }),
                      X01.Names[val[i]]));
          e.knownOutput(nm, Dims.T { i }, dly, X01.ToBit(val[i]), assertHoldFrac * t.cycleTime + assertHoldTime)
        END
      END
    END Produce;

  BEGIN
    Debug.Out(F("LambModel.SimStep @ %s wen %s ren %s",
                LR(e.getTime()),
                Bool(wen),
                Bool(ren)));

    q := FALSE;
    IF ren THEN DoRen(); q := TRUE END;
    IF wen THEN DoWen(); q := TRUE END;
  END SimStep;

BEGIN END LambModel.
