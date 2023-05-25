MODULE Main;
IMPORT Params;
IMPORT Wr;
IMPORT Scan;
FROM Fmt IMPORT Int, F, FN;
IMPORT Stdio;
IMPORT Thread;
IMPORT FloatMode, Lex;

<*FATAL Thread.Alerted, Wr.Failure*>
<*FATAL FloatMode.Trap, Lex.Error*>

(* Vector latch netlister.  Usage : 

   $0 <subckt-name> <threshold> <N>

*)

TYPE
  Pol = { N, P };

CONST
  PolNames = ARRAY Pol OF TEXT { "n", "p" };
  PolPlug  = ARRAY Pol OF TEXT { "vss", "vcc" };
  TranStr  = "hp";
  Vss      = "vssx";
  Vdd      = "vcc";
  
PROCEDURE Emit(polarity : Pol;
               name     : TEXT;
               d, g, s  : TEXT;
               w        : CARDINAL;
               m        : CARDINAL) =
  BEGIN
    Wr.PutText(wr,
               FN("M%s %s %s %s %s %s%s%s w=%s l=14e-9 m=%s nf=1\n",
                  ARRAY OF TEXT {
                  name,

                  d, g, s,
                  PolPlug [polarity],

                  PolNames[polarity],
                  TranStr,
                  vt,

                  Int(w),
                  Int(m)}))
  END Emit;

PROCEDURE EmitInverter(name : TEXT; i, o : TEXT; w, m : CARDINAL) =
  BEGIN
    Emit(Pol.N, name & "n", o, i, Vss, w, m);
    Emit(Pol.P, name & "p", o, i, Vdd, w, m);
  END EmitInverter;

PROCEDURE EmitPassgate(name : TEXT; i, clk, clkb, o : TEXT; w, m : CARDINAL) =
  BEGIN
    Emit(Pol.N, name & "n",    o,  clk, i, w, m);
    Emit(Pol.P, name & "p",    o, clkb, i, w, m);
  END EmitPassgate;

PROCEDURE EmitTri(name : TEXT; i, clk, clkb, o : TEXT; w, m : CARDINAL) =
  VAR
    nInt := GetInt();
    pInt := GetInt();
  BEGIN
    Emit(Pol.N, name & "n0", nInt,    i,  Vss, w, m);
    Emit(Pol.N, name & "n1",    o,  clk, nInt, w, m);
    
    Emit(Pol.P, name & "p0", pInt,    i,  Vdd, w, m);
    Emit(Pol.P, name & "p1",    o, clkb, pInt, w, m);
  END EmitTri;

PROCEDURE EmitTinyLatch(name : TEXT; d, clk, clkb, q : TEXT; w, m : CARDINAL) =
  VAR
    pd := name & "_" & GetInt() & "pd";
    qq := name & "_" & GetInt() & "qq";
  BEGIN
    EmitPassgate(name & "_pg"  ,  d,  clk, clkb, pd, w, m);
    EmitInverter(name & "_inv0", pd,   qq,           w, m);
    EmitInverter(name & "_invq", qq,    q,           w, m);
    EmitTri     (name & "_tri" , qq, clkb,  clk, pd, w, m);
  END EmitTinyLatch;

PROCEDURE EmitVectorLatch(name : TEXT; N : CARDINAL) =
  BEGIN
    Wr.PutText(wr, F(".subckt %s clk ", name));
    FOR i := 1 TO N DO
      Wr.PutText(wr, "d" & Int(i));
      Wr.PutChar(wr, ' ')
    END;
    FOR i := 1 TO N DO
      Wr.PutText(wr, "q" & Int(i));
      Wr.PutChar(wr, ' ')
    END;
    Wr.PutText(wr, "vcc vssx\n");

    EmitInverter("clkinv", "clk", "clkb", 2, 1);

    FOR i := 1 TO N DO
      WITH sfx = Int(i) DO
        EmitTinyLatch("tny" & sfx, "d" & sfx, "clk", "clkb", "q" & sfx, 2, 1)
      END
    END;

    Wr.PutText(wr, F(".ends %s\n", name))
  END EmitVectorLatch;
  
VAR
  i := 0;

PROCEDURE GetInt() : TEXT =
  BEGIN
    TRY
      RETURN "int" & Int(i)
    FINALLY
      INC(i)
    END
  END GetInt;
  
VAR
  wr := Stdio.stdout;
  vt := "svt";

BEGIN
  EmitVectorLatch(Params.Get(1), Scan.Int(Params.Get(2)))
END Main.
