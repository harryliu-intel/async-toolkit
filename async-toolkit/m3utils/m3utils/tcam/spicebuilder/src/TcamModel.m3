MODULE TcamModel;
IMPORT Tcam;
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
    c    : Tcam.T;
    conf : REF ARRAY OF ARRAY OF X01.T;
  OVERRIDES
    init := Init;
    simStep := SimStep;
  END;

PROCEDURE Init(t : T; READONLY c : Tcam.T) : T = 
  BEGIN 
    t.c := c; 
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
  VAR
    wen   :=     ToBool(e.getSngl("WEN"             ));
    ren   :=     ToBool(e.getSngl("REN"             ));
    ken   :=     ToBool(e.getSngl("KEN"             ));
    rst   := NOT ToBool(e.getSngl("RESET_N"         ));

    addr  :=     ToCard(e.getArry("ADDR"            )^);
    data  :=            e.getArry("DATA"              );
    slice :=            e.getArry("SLICE_EN"          );
    lhit  :=            e.getArry("LHIT"              );
    mask  :=            e.getArry("MASK"              );

    q : BOOLEAN;

  PROCEDURE DoRen() =
    BEGIN
      WITH z = t.conf[addr] DO
        Produce(2, "READ_DATA", z)
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
      Produce(1, "RHIT", rhit^)
    END DoKen;

  PROCEDURE Reset() = 
    BEGIN (* skip *) END Reset;

  PROCEDURE Produce(dly : CARDINAL; nm : TEXT; READONLY val : ARRAY OF X01.T) =
    BEGIN
      FOR i := FIRST(val) TO LAST(val) DO
        IF val[i] # X01.T.VX THEN
          Debug.Out(F("TcamModel : t=%s clkOff %s : %s%s -> %s",
                      LR(e.getTime()),
                      Int(dly),
                      nm, Dims.Format(Dims.T { i }),
                      X01.Names[val[i]]));
          e.knownOutput(nm, Dims.T { i }, dly, X01.ToBit(val[i]))
        END
      END
    END Produce;

  BEGIN
    Debug.Out(F("TcamModel.SimStep @ %s wen %s ren %s ken %s",
                LR(e.getTime()),
                Bool(wen),
                Bool(ren),
                Bool(ken)));

    IF rst THEN 
      Reset() 
    ELSE
      q := FALSE;
      IF ren THEN <*ASSERT NOT q*> DoRen(); q := TRUE END;
      IF wen THEN <*ASSERT NOT q*> DoWen(); q := TRUE END;
      IF ken THEN <*ASSERT NOT q*> DoKen(); q := TRUE END;
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
