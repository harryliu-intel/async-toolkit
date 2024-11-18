MODULE Main;

IMPORT ParseParams;
IMPORT Stdio;
IMPORT IO;
IMPORT Math;
IMPORT Debug;
IMPORT Params;
IMPORT Fmt; FROM Fmt IMPORT LongReal, F;
IMPORT FileWr;
IMPORT Wr;
IMPORT Random;
IMPORT NormalDeviate;
  
CONST LR = LongReal;
      
CONST Usage = "exampleprog usage wrong!";

PROCEDURE MakeNoise() : LONGREAL =
  VAR
    err : LONGREAL;
  BEGIN
    IF    nominal THEN
      err := 0.0d0
    ELSIF quadstats THEN
      err := NormalDeviate.Get(rand, vdd * vdd + mean, delp * delp + sdev)
    ELSIF varstats THEN
      err := NormalDeviate.Get(rand, vdd + mean, delp + sdev)
    ELSE
      err := NormalDeviate.Get(rand, mean, sdev)
    END;
    RETURN err
  END MakeNoise;
  
VAR
  pp                          := NEW(ParseParams.T).init(Stdio.stderr);

  temp, vdd, delp, deln : LONGREAL;

  val : LONGREAL;
  method : [ 0..3 ] := 1;
  
  rand  := NEW(Random.Default).init();
  samples : CARDINAL;
  mean  := 1.0d0; (* mean of offset from nominal *)
  sdev  := 1.0d0; (* sdev of offset from nominal *)
  nominal : BOOLEAN;
  varstats : BOOLEAN;
  quadstats : BOOLEAN;
  t := ARRAY [0..2] OF LONGREAL { 0.0d0, .. };
  q := t;
BEGIN
  TRY
    nominal := pp.keywordPresent("-nominal");
    varstats := pp.keywordPresent("-varstats");
    quadstats := pp.keywordPresent("-quadstats");
    
    IF pp.keywordPresent("-method") THEN
      method := pp.getNextInt()
    END;
    IF pp.keywordPresent("-temp") THEN
      temp := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-vdd") THEN
      vdd := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-delp") THEN
      delp := pp.getNextLongReal()
    END; 
    IF pp.keywordPresent("-deln") THEN
      deln := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-mean") THEN
      mean := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-sdev") THEN
      sdev := pp.getNextLongReal()
    END;
    IF pp.keywordPresent("-sweeps") THEN
      samples := MAX(1, pp.getNextInt())
    END
  EXCEPT
    ParseParams.Error => Debug.Error("Can't parse command-line parameters\nUsage: " & Params.Get(0) & " " & Usage)
  END;
  
  CASE method OF
    
    0, 2 =>
    WITH dv = vdd  - temp,
         dp = delp - 2.0d0,
         dn = deln - 3.0d0 DO
         
      t[0] := dv * dv * dv * dv;
      t[1] := dp * dp * dp * dp;
      t[2] := dn * dn * dn * dn

    END;

    IF method = 0 AND vdd < 0.0d0 THEN
      Debug.Error("vdd out of range")
    END
  |
    1 =>
    WITH r = 1.0d0,
         k = 3.0d0,

         rfactor = Math.pow(vdd - r,  2.0d0 * k) + 1.0d0,

         zfactor = (delp * delp + 1.0d0),
         
         sqrt2 = Math.sqrt(2.0d0),

         rosqrt2 = r / sqrt2,

         dx = vdd - rosqrt2,

         dy = deln - rosqrt2,

         xyfactor = (dx * dx + dy * dy + 1.0d0)
     DO
      t[0] :=  rfactor * zfactor * xyfactor 
    END
  |
    3 =>
    t[0] := vdd * vdd; t[1] := delp * delp; t[2] :=  deln * deln
  END;

  Debug.Out(F("exampleprog : vdd %s delp %s deln %s ; val %s",
              LR(vdd), LR(delp), LR(deln), LR(val)));
  
  WITH wr = FileWr.Open("example.out") DO
    FOR i := 0 TO samples - 1 DO
      
      (* add some noise! *)
      WITH nf = FLOAT(NUMBER(t), LONGREAL) DO
        FOR i := FIRST(t) TO LAST(t) DO
          q[i] := t[i] + 1.0d0/nf * MakeNoise()
        END
      END;
      
      val := q[0] + q[1] + q[2];
      
      IO.Put(LR(val) & "\n");
      
      Wr.PutText(wr, F("%s,%s,%s,%s\n",
                       LR(val), LR(q[0]), LR(q[1]), LR(q[2])));
    END;
    Wr.Close(wr)
  END
END Main.
    
    
