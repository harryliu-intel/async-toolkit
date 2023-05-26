MODULE Main;
IMPORT Params;
IMPORT Wr;
IMPORT Scan;
FROM Fmt IMPORT Int, F, FN, Pad, Align;
IMPORT Stdio;
IMPORT Thread;
IMPORT FloatMode, Lex;
IMPORT TextSet, TextSetDef;
IMPORT TextWr;
IMPORT Text;
IMPORT Debug;
IMPORT Wx;

<*FATAL Thread.Alerted, Wr.Failure*>
<*FATAL FloatMode.Trap, Lex.Error*>

(* Vector latch netlister.  Usage : 

   $0 <subckt-name> <N>

*)

TYPE
  Pol      = { N, P };
  IntfMode = { Input, Output, Inout };
  
CONST
  PolNames = ARRAY Pol OF TEXT { "n", "p" };
  PolPlug  = ARRAY Pol OF TEXT { "vssx", "vcc" };
  TranStr  = "hp";
  Vss      = "vssx";
  Vdd      = "vcc";

VAR
  gates     := NEW(TextSetDef.T).init();
  srcdrains := NEW(TextSetDef.T).init();
  overrides := ARRAY IntfMode OF TextSet.T { NEW(TextSetDef.T).init(),
                                             NEW(TextSetDef.T).init(),
                                             NEW(TextSetDef.T).init() };
  
PROCEDURE Emit(polarity : Pol;
               name     : TEXT;
               d, g, s  : TEXT;
               w        : CARDINAL;
               m        : CARDINAL) =
  BEGIN

    EVAL gates.insert(g);
    EVAL srcdrains.insert(d);
    EVAL srcdrains.insert(s);
    
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

PROCEDURE EmitVectorLatch(typeName : TypeName; N : CARDINAL) =
  VAR
    typeTxt := Text.FromChars(typeName);
  BEGIN
    Wr.PutText(wr, F(".subckt %s clk ", typeTxt));
    FOR i := 1 TO N DO
      WITH iName =  "d" & Int(i) DO
        Wr.PutText(wr, iName);
        Wr.PutChar(wr, ' ');
        EVAL overrides[IntfMode.Input].insert(iName)
      END
    END;
    FOR i := 1 TO N DO
      Wr.PutText(wr, "q" & Int(i));
      Wr.PutChar(wr, ' ')
    END;
    Wr.PutText(wr, "vcc vssx\n");

    NextSection();

    EmitInverter("clkinv", "clk", "clkb", 2, 1);

    FOR i := 1 TO N DO
      WITH sfx = Int(i) DO
        EmitTinyLatch("tny" & sfx, "d" & sfx, "clk", "clkb", "q" & sfx, 2, 1)
      END
    END;

    Wr.PutText(wr, F(".ends %s\n", typeTxt))
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

CONST TypeLen = 17;
TYPE  TypeName = ARRAY [ 0 .. TypeLen - 1 ] OF CHAR;
  
PROCEDURE PrepareTypeName(fname : TEXT) : TypeName =
  CONST
    Lib           = "i0s";
    FLen          = 6;
    SchematicVar  = 'a';
    VoltageVar    = 'c'; (* svt *)
    LayoutVar     = '1';
    RowPitch      = 'n';
    Strength      = "02x5";

  VAR
    res : TypeName;
    p := 0;

  PROCEDURE CopyChars(txt : TEXT) =
    BEGIN
      FOR i := 0 TO Text.Length(txt) - 1 DO
        WITH c = Text.GetChar(txt, i) DO
          Char(c)
        END
      END
    END CopyChars;

  PROCEDURE Char(c : CHAR) =
    BEGIN
      res[p] := c;
      INC(p)
    END Char;
    
  BEGIN
    IF Text.Length(fname) > FLen THEN
      WITH trunc = Text.Sub(fname, 0, FLen) DO
        Debug.Warning(F("Function name \"%s\" is too long (> %s), truncating to \"%s\"", fname, Int(FLen), trunc));
        fname := trunc
      END
    END;

    fname := Pad(fname, FLen, padChar := '0', align := Align.Left);

    CopyChars(Lib);
    CopyChars(fname);
    Char     (SchematicVar);
    Char     (VoltageVar);
    Char     (LayoutVar);
    Char     (RowPitch);
    CopyChars(Strength);

    <*ASSERT p = TypeLen*>
    
    RETURN res
  END PrepareTypeName;

PROCEDURE NextSection() =
  BEGIN
    FOR i := FIRST(wrs) TO LAST(wrs) DO
      IF wr = wrs[i] THEN
        wr := wrs[i + 1];
        RETURN
      END
    END
  END NextSection;

PROCEDURE SetList(s : TextSet.T) : TEXT =
  (* print list of strings from set, with leading space *)
  VAR
    wx := Wx.New();
    iter := s.iterate();
    t : TEXT;
  BEGIN
    WHILE iter.next(t) DO
      Wx.PutChar(wx, ' ');
      Wx.PutText(wx, t)
    END;
    RETURN Wx.ToText(wx)
  END SetList;

PROCEDURE InoutList() : TEXT =
  VAR
    set := NEW(TextSetDef.T).init();
  BEGIN
    EVAL set.insert(Vdd);
    EVAL set.insert(Vss);
    set := set.union(overrides[IntfMode.Inout]);
    set := set.diffD(overrides[IntfMode.Input]);
    set := set.diffD(overrides[IntfMode.Output]);
    RETURN SetList(set)
  END InoutList;
  
PROCEDURE InputList() : TEXT =
  VAR
    set := gates.diff(srcdrains);
  BEGIN
   EVAL set.delete(Vss); 
   EVAL set.delete(Vdd);
   set := set.diffD(overrides[IntfMode.Inout]);
   set := set.union(overrides[IntfMode.Input]);
   set := set.diffD(overrides[IntfMode.Output]);
   RETURN SetList(set)
  END InputList;

PROCEDURE OutputList() : TEXT =
  VAR
    set :=  srcdrains.diff(gates);
  BEGIN
   EVAL set.delete(Vss); 
   EVAL set.delete(Vdd);
   set := set.diffD(overrides[IntfMode.Inout]);
   set := set.diffD(overrides[IntfMode.Input]);
   set := set.union(overrides[IntfMode.Output]);
   RETURN SetList(set)
  END OutputList;
  
VAR
  wrs := ARRAY [0..1] OF TextWr.T { TextWr.New(), TextWr.New() };
  wr  := wrs[0];
  vt  := "svt";

  cmdName  := Params.Get(1);
  N        := Scan.Int(Params.Get(2));
  typeName := PrepareTypeName(cmdName);
  oWr      := Stdio.stdout;
  
BEGIN
  EmitVectorLatch(typeName, N);

  Debug.Out("gates     " & SetList(gates));
  Debug.Out("srcdrains " & SetList(srcdrains));
  
  Wr.PutText(oWr,   "*  Library name: i0sdocssux1pp50\n");
  Wr.PutText(oWr,   "*  Cell name: " & Text.FromChars(typeName) & "\n");
  Wr.PutText(oWr,   "*  View name: schematic\n");
  Wr.PutText(oWr, TextWr.ToText(wrs[0]));
  Wr.PutText(oWr, F("*  INOUT:%s\n", InoutList()));
  Wr.PutText(oWr, F("*  INPUT:%s\n", InputList()));
  Wr.PutText(oWr, F("*  OUTPUT:%s\n", OutputList()));
  Wr.PutText(oWr, TextWr.ToText(wrs[1]));
  Wr.PutText(oWr, F("*  End of subcircuit definition.\n"))
END Main.
