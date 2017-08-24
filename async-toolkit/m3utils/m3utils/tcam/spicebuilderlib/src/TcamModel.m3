MODULE TcamModel;
IMPORT Tcam;
IMPORT X01;
IMPORT Bit;
IMPORT Valenv;
IMPORT Word;
IMPORT Dims;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Bool; IMPORT Fmt;
IMPORT TextUtils;

CONST LR = Fmt.LongReal;

REVEAL
  T = Public BRANDED OBJECT
    c    : Tcam.T;
    conf : REF ARRAY OF ARRAY OF X01.T;
    assertHoldFrac : LONGREAL;
    assertHoldTime : LONGREAL;
    cycleTime : LONGREAL;
    nc : NamingConvention;
  OVERRIDES
    init := Init;
    simStep := SimStep;
  END;

PROCEDURE Init(t : T;
               READONLY c : Tcam.T;
               cycleTime, assertHoldFrac, assertHoldTime : LONGREAL;
               nc : NamingConvention) : T = 
  BEGIN 
    t.c := c;
    t.nc := nc;
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
      CASE t.nc OF
        NamingConvention.Andrew => RETURN nm
      |
        NamingConvention.SDG    => RETURN TextUtils.ToLower(nm)
      END
    END C;
    
  VAR
    wen   :=     ToBool(e.getSngl(C("WEN"             )));
    ren   :=     ToBool(e.getSngl(C("REN"             )));
    ken   :=     ToBool(e.getSngl(C("KEN"             )));
    rst   := NOT ToBool(e.getSngl(C("RESET_N"         )));

    addr  :=     ToCard(e.getArry(C("ADDR"            ))^);
    data  :=            e.getArry(C("DATA"            )  );
    slice : REF ARRAY OF X01.T;
    lhit  :=            e.getArry(C("LHIT"            )  );
    mask  :=            e.getArry(C("MASK"            )  );

    q : BOOLEAN;

  PROCEDURE DoRen() =
    BEGIN
      WITH z = t.conf[addr] DO
        Produce(2, C("READ_DATA"), z, t.assertHoldFrac, t.assertHoldTime)
      END
    END DoRen;

  PROCEDURE DoWen() =
    BEGIN
      t.conf[addr] := data^
    END DoWen;

  PROCEDURE DoKen() =
    VAR
      rhit := NEW(REF ARRAY OF X01.T, t.c.N);
    BEGIN
      FOR i := 0 TO t.c.N-1 DO
        rhit[i] := X01.T.VX;
        IF slice[i DIV t.c.SS] = X01.T.V1 THEN
          TryMatch(data^, mask^, t.conf[2*i], t.conf[2*i+1], lhit[i], rhit[i]);
          IF rhit[i] # X01.T.VX THEN
            Debug.Out(F("DoKen HIT %s rhit[%s] @ %s", X01.Names[rhit[i]],
                        Int(i), LR(e.getTime())));
            Debug.Out(F("data  %s", FmtArry(data^)));
            Debug.Out(F("mask  %s", FmtArry(mask^)));
            Debug.Out(F("conf0 %s", FmtArry(t.conf[2*i])));
            Debug.Out(F("conf1 %s", FmtArry(t.conf[2*i+1])));
          END
        END
      END;
      Produce(1, C("RHIT"), rhit^, t.assertHoldFrac, t.assertHoldTime)
    END DoKen;

  PROCEDURE Reset() = 
    BEGIN (* skip *) END Reset;

  PROCEDURE Produce(dly : CARDINAL;
                    nm : TEXT;
                    READONLY val : ARRAY OF X01.T;
                    assertHoldFrac, assertHoldTime : LONGREAL) =
    BEGIN
      FOR i := FIRST(val) TO LAST(val) DO
        IF val[i] # X01.T.VX THEN
          Debug.Out(F("TcamModel : t=%s clkOff %s : %s%s -> %s",
                      LR(e.getTime()),
                      Int(dly),
                      nm, Dims.Format(Dims.T { i }),
                      X01.Names[val[i]]));
          e.knownOutput(nm, Dims.T { i }, dly, X01.ToBit(val[i]), assertHoldFrac * t.cycleTime + assertHoldTime)
        END
      END
    END Produce;

  PROCEDURE WarnOnQ() =
    BEGIN
      IF q THEN
        Debug.Warning("ren/wen/ken active at the same time!")
      END
    END WarnOnQ;
    
  BEGIN
    IF t.c.SN = 1 AND t.nc = NamingConvention.SDG THEN
      (* note that the SDG single-slice TCAM uses a scalar name for slice_en *)
      slice := NEW(REF ARRAY OF X01.T, 1);
      slice[0] := e.getSngl(C("SLICE_EN"))
    ELSE
      slice :=            e.getArry(C("SLICE_EN"        )  )
    END;
      
    Debug.Out(F("TcamModel.SimStep @ %s wen %s ren %s ken %s",
                LR(e.getTime()),
                Bool(wen),
                Bool(ren),
                Bool(ken)));

    IF rst THEN 
      Reset() 
    ELSE
      q := FALSE;
      IF ren THEN WarnOnQ(); DoRen(); q := TRUE END;
      IF wen THEN WarnOnQ(); DoWen(); q := TRUE END;
      IF ken THEN WarnOnQ(); DoKen(); q := TRUE END;
    END
  END SimStep;

  PROCEDURE TryMatch(READONLY x, mask, p0, p1 : ARRAY OF X01.T;
                     READONLY lhit            :          X01.T;
                     VAR      res             :          X01.T) =
    TYPE
      Q = X01.T;
    BEGIN
      <*ASSERT NUMBER(x)  = NUMBER(p0)*>
      <*ASSERT NUMBER(p1) = NUMBER(p0)*>
      FOR i := FIRST(x) TO LAST(x) DO
        (* skip Xs *)
        IF lhit = Q.VX OR 
           mask[i] = Q.VX OR x[i] = Q.VX OR p0[i] = Q.VX OR p1[i] = Q.VX THEN
          res := Q.VX; 
          RETURN
        ELSIF lhit = Q.V0 THEN
          res := Q.V0;
          RETURN
        ELSIF mask[i] = Q.V1 AND (x[i] = Q.V0 AND p0[i] = Q.V0) OR 
                                 (x[i] = Q.V1 AND p1[i] = Q.V0) THEN
          res := Q.V0;
          RETURN
        END
      END;
      res := Q.V1
    END TryMatch;

BEGIN END TcamModel.
