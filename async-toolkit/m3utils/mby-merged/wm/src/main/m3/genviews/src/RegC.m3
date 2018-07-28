MODULE RegC EXPORTS RegC, RegCGenerators;
IMPORT GenViewsC;
IMPORT RegReg, RegGenState, RegRegfile, RegAddrmap;
IMPORT OSError, Thread, Wr;
IMPORT Pathname, RegCGenState;
FROM RegCGenState IMPORT Section;
IMPORT Wx;
IMPORT RdlArray, BigInt;
FROM Compiler IMPORT ThisFile, ThisLine;
IMPORT Fmt;
FROM Fmt IMPORT Int, F;
IMPORT RegComponent;
FROM RegCConstants IMPORT IdiomName;
IMPORT Debug;
IMPORT TextRefTbl;
IMPORT TextTopoSort;
IMPORT FileWr;
IMPORT RegContainer;
IMPORT TextSeq;

<*FATAL BigInt.OutOfRange*>

VAR doDebug := Debug.DebugThis("C");

REVEAL
  T = GenViewsC.Compiler BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
  END;

PROCEDURE DefTypes(wr : Wr.T) RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  BEGIN
    FOR i := 1 TO 64 DO
      CASE i OF
        1..8 => Wr.PutText(wr, "typedef unsigned char ")
      |
        9..16 => Wr.PutText(wr, "typedef unsigned short ")
      |
        17..32 => Wr.PutText(wr, "typedef unsigned int ")
      |
        33..64 => Wr.PutText(wr, "typedef unsigned long ")
      END;
      Wr.PutText(wr, F("uint%s;\n", Int(i)))
    END
  END DefTypes;
  
PROCEDURE Write(t : T; dirPath : Pathname.T; <*UNUSED*>phase : Phase)
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    wxTbl := NEW(TextRefTbl.Default).init();
    gs : GenState := RegGenState.T.init(NEW(GenState,
                                            wxTbl := wxTbl,
                                            map := t.map,
                                            topo := NEW(TextTopoSort.T).init()), dirPath);
    intfNm := t.map.intfName(gs);
    fn := intfNm & ".h";
    path := dirPath & "/" & fn;
  BEGIN
    FOR ph := FIRST(GenPhase) TO LAST(GenPhase) DO
      EVAL RegGenState.T.init(gs, gs.dirPath); (* clear symbol table *)
      genPhase := ph;
      t.map.generate(gs);

      CASE ph OF
        0 =>
        Debug.Out("Copying output to " & path);
        
        (* perform topo sort and produce output in that order *)
        WITH seq = gs.topo.sort(),
             wr  = FileWr.Open(path) DO
          Wr.PutText(wr, F("#ifndef %s_INCLUDED\n#define %s_INCLUDED\n\n",
                           intfNm, intfNm));
         DefTypes(wr);
          FOR i := 0 TO seq.size()-1 DO
            Debug.Out(F("Emit wx[%s]",seq.get(i)));
            VAR
              wx : REFANY;
              hadIt := wxTbl.get(seq.get(i),wx);
            BEGIN
              <*ASSERT hadIt*>
              Wr.PutText(wr, Wx.ToText(wx))
            END
          END;
          Wr.PutText(wr, F("#endif /* !%s_INCLUDED */\n", intfNm));
          Wr.Close(wr)
        END;
        gs.curWx := Wx.New()
      |
        1 =>
        WITH fn = intfNm & ".c",
             path = dirPath & "/" & fn,
             wr = FileWr.Open(path) DO
          Wr.PutText(wr, F("#include \"%s.h\"\n\n", intfNm));
          Wr.PutText(wr, Wx.ToText(gs.curWx));
          Wr.Close(wr)
        END
      END
    END;

    WITH fn = intfNm & "_main.h",
         path = dirPath & "/" & fn,
         wr = FileWr.Open(path) DO
      Wr.PutText(wr, F("#ifndef %s_main_INCLUDED\n#define %s_main_INCLUDED\n\n",
                       intfNm, intfNm));
      Wr.PutText(wr, F("\nvoid\n%s_SendPacket(const  %s *r, const %s__addr *w, int port, unsigned char *packet, unsigned int length);\n", intfNm, intfNm, intfNm));
      Wr.PutText(wr, F("\nvoid %s_build(void (*f)(void *)); /* called from Modula-3 */\n", intfNm));
      Wr.PutText(wr, F("#endif /* !%s_main_INCLUDED */\n", intfNm));
      Wr.Close(wr)
    END;

    WITH fn = intfNm & "_c.i3",
         path = dirPath & "/" & fn,
         wr = FileWr.Open(path) DO
      Wr.PutText(wr, F("INTERFACE %s_c;\n\n", intfNm));
      Wr.PutText(wr, F("IMPORT Ctypes;\n"));
      Wr.PutText(wr, F("TYPE CallBackProc = PROCEDURE(addr : ADDRESS);\n\n"));
      Wr.PutText(wr, F("<*EXTERNAL %s_build*>\n", intfNm));
      Wr.PutText(wr, F("PROCEDURE BuildMain(cb : CallBackProc);\n\n"));
      Wr.PutText(wr, F("<*EXTERNAL %s_SendPacket*>\n", intfNm));
      Wr.PutText(wr, F("PROCEDURE SendPacket(r, w : Ctypes.void_star; port : Ctypes.int; packet : Ctypes.char_star; length : Ctypes.unsigned_int);\n\n"));
      Wr.PutText(wr, F("END %s_c.\n", intfNm));
      Wr.Close(wr)
    END;

    WITH fn = intfNm & "_build.c",
         path = dirPath & "/" & fn,
         wr = FileWr.Open(path) DO
      Wr.PutText(wr, F("#include \"%s.h\"\n", intfNm));
      Wr.PutText(wr, F("#include <stdlib.h>\n\n"));     
      Wr.PutText(wr, F("void\n"));
      Wr.PutText(wr, F("%s_build(void (*f)(void *))\n", intfNm));
      Wr.PutText(wr, F("{\n"));
      Wr.PutText(wr, F("  %s       *r;\n", intfNm));
      Wr.PutText(wr, F("  %s__addr *w;\n\n", intfNm));
      Wr.PutText(wr, F("  r = (%s       *)malloc(sizeof(%s      ));\n", intfNm, intfNm));
      Wr.PutText(wr, F("  w = (%s__addr *)malloc(sizeof(%s__addr));\n\n", intfNm, intfNm));
      Wr.PutText(wr, F("  %s__init(r, w, f);\n", intfNm));
      Wr.PutText(wr, F("}\n"));
      Wr.Close(wr)
    END;
    
    WITH fn = "m3makefile",
         path = dirPath & "/" & fn,
         wr = FileWr.Open(path) DO
      Wr.PutText(wr, F("SYSTEM_CC = SYSTEM_CC & \" -std=gnu99\"\n"));
      Wr.PutText(wr, F("import(\"libm3\")\n"));
      Wr.PutText(wr, F("c_source(\"%s\")\n",intfNm));
      Wr.PutText(wr, F("c_source(\"%s_build\")\n",intfNm));
      Wr.PutText(wr, F("Interface(\"%s_c\")\n", intfNm));
      Wr.PutText(wr, F("library(\"%s_cmodel\")\n",intfNm));
      Wr.Close(wr)
    END;
    
  END Write;

  (**********************************************************************)
  
TYPE
  GenState = RegCGenState.T OBJECT
    map : RegAddrmap.T; (* do we really need this? could refer to T instead *)
    curSym : TEXT := NIL;
    curWx : Wx.T := NIL;
    wxTbl : TextRefTbl.T;
    topo : TextTopoSort.T;
  METHODS
    p(sec : Section; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsP;
    main(fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsMain;
    noteDep(to : TEXT) :=  NoteDep;
  OVERRIDES
    put := PutGS;
    newSymbol := NewSymbol;
  END;

PROCEDURE NoteDep(gs : GenState; toSym : TEXT) =
  BEGIN
    (* note that curSym depends on toSym *)
    <*ASSERT gs.curSym # NIL*>
    Debug.Out(F("%s depends on %s", gs.curSym, toSym));
    gs.topo.addDependency(toSym, gs.curSym)
  END NoteDep;
      
PROCEDURE NewSymbol(gs : GenState; nm : TEXT) : BOOLEAN
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    res : BOOLEAN;
  BEGIN
    (* is it OK to generate this symbol?  
       if we had it before -> NOT OK
       if we did not have it before -> OK
     *)
    res := RegGenState.T.newSymbol(gs, nm);

    IF res AND genPhase = 0 THEN
      gs.curWx := Wx.New();
      WITH hadIt = gs.wxTbl.put(nm, gs.curWx) DO <*ASSERT NOT hadIt*> END
    END;
    gs.curSym := nm;
    
    RETURN res
  END NewSymbol;
  
PROCEDURE PutGS(gs : GenState; sec : Section; txt : TEXT) =
  BEGIN
    Wx.PutText(gs.curWx, txt)
  END PutGS;

PROCEDURE GsP(gs  : GenState;
              sec : Section;
              fmt : TEXT;
              t1, t2, t3, t4, t5 : TEXT) =
  BEGIN gs.put(sec, F(fmt, t1, t2, t3, t4, t5)) END GsP;

PROCEDURE GsMain(gs : GenState; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL)= 
  BEGIN gs.p(Section.Maintype, fmt, t1, t2, t3, t4, t5) END GsMain;

  (**********************************************************************)

PROCEDURE FmtArrSz(xDecls : TextSeq.T; a : RdlArray.Single; nm : TEXT) =
  BEGIN
    IF a = NIL THEN
      RETURN
    ELSE
      xDecls.addhi(F("static const int %s__n = %s", nm, BigInt.Format(a.n.x)))
    END
  END FmtArrSz;

PROCEDURE PutXDecls(gs : GenState; xDecls : TextSeq.T) =
  BEGIN
    FOR i := 0 TO xDecls.size()-1 DO
      gs.main(xDecls.get(i)); gs.main(";\n")
    END;
    gs.main("\n")
  END PutXDecls;

TYPE
  Variant = RECORD ptr, sfx : TEXT END;

CONST
  Phases = ARRAY OF Variant { Variant {  "",       "" },
                              Variant { "*", "__addr" } };

PROCEDURE GenRegStruct(r : RegReg.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    gs : GenState := genState;
    myTn := r.typeName(gs);

  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    Debug.Out("Generating code for " & myTn);
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    FOR p := FIRST(Phases) TO LAST(Phases) DO
      WITH v = Phases[p] DO
        gs.main("typedef struct {\n");
        FOR i := 0 TO r.fields.size()-1 DO
          WITH f  = r.fields.get(i),
               nm = f.name(debug := FALSE) DO
            gs.main("  uint%s %s%s;\n", Int(f.width), v.ptr, nm);
          END
        END;
        gs.main("} %s%s;\n\n", myTn, v.sfx)
      END
    END;
    GenProto(r, gs);
    gs.main(";\n\n");
  END GenRegStruct;

  (* the way this is coded, GenRegfile and GenAddrmap could be merged into
     one routine, viz., GenContainer *)
  
PROCEDURE GenContainerStruct(rf       : RegContainer.T;
                     genState : RegGenState.T) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    gs : GenState := genState;
    myTn := rf.typeName(gs);
    xDecls := NEW(TextSeq.T).init();
  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    FOR p := FIRST(Phases) TO LAST(Phases) DO
      WITH v = Phases[p] DO
        gs.main("typedef struct {\n");
        FOR i := 0 TO rf.children.size()-1 DO
          WITH c  = rf.children.get(i),
               tn = ComponentTypeName(c.comp,gs),
               nm = IdiomName(c.nm, FALSE) DO
            IF c.array = NIL THEN
              gs.main("  %s%s %s;\n", tn, v.sfx, nm)
            ELSE
              gs.main("  %s%s %s[%s];\n", tn, v.sfx, nm, Int(ArrSz(c.array)));
              IF p = 0 THEN FmtArrSz(xDecls, c.array, myTn & "_" & nm) END;
            END;
            gs.noteDep(tn);
            IF rf.children.size() = 1 THEN (* ? *)
            END;
          END
        END;
        gs.main("} %s%s;\n\n", myTn, v.sfx);
      END
    END;
    GenProto(rf, gs);
    gs.main(";\n\n");

    PutXDecls(gs, xDecls);
    
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c = rf.children.get(i) DO
        <*ASSERT c.comp # NIL*>
        c.comp.generate(gs)
      END
    END
   END GenContainerStruct;
  (**********************************************************************)

PROCEDURE GenProto(  r : RegComponent.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    gs : GenState := genState;
    myTn := r.typeName(gs);
  BEGIN
    gs.main("\nvoid\n%s__init(\n", myTn);
    FOR p := FIRST(Phases) TO LAST(Phases) DO
      gs.main("  %s%s *p%s,\n", myTn, Phases[p].sfx, Int(ORD(p)));
    END;
    gs.main("  void (*f)(void *)\n");
    gs.main(")");
  END GenProto;

  (**********************************************************************)

PROCEDURE GenRegInit(r : RegReg.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    gs : GenState := genState;
    myTn := r.typeName(gs);

  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));

    GenProto(r, genState);
    gs.main("{\n");
    
    FOR i := 0 TO r.fields.size()-1 DO
      WITH f  = r.fields.get(i),
           nm = f.name(debug := FALSE) DO
        gs.main("  p1->%s = &(p0->%s);\n", nm, nm);
        gs.main("  f(&(p0->%s));\n",nm)
      END
    END;
    gs.main("}\n\n")
  END GenRegInit;

PROCEDURE GenContainerInit(rf       : RegContainer.T;
                     genState : RegGenState.T) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    gs : GenState := genState;
    myTn := rf.typeName(gs);
  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    GenProto(rf, gs);
    gs.main("{\n");
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c  = rf.children.get(i),
           tn = ComponentTypeName(c.comp, gs),
           nm = IdiomName(c.nm,FALSE) DO
        IF c.array = NIL THEN
          gs.main("  %s__init(&(p0->%s),&(p1->%s),f);\n", tn, nm, nm)
        ELSE
          gs.main("  for (int i=0; i<%s; ++i)\n", Int(ArrSz(c.array)));
          gs.main("    %s__init(&(p0->%s[i]),&(p1->%s[i]),f);\n", tn, nm, nm)
        END
      END
    END;
    gs.main("}\n\n");
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c = rf.children.get(i) DO
        <*ASSERT c.comp # NIL*>
        c.comp.generate(gs)
      END
    END;
  END GenContainerInit;

  (**********************************************************************)

PROCEDURE GenReg(r : RegReg.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  BEGIN
    CASE genPhase OF
      0 =>  GenRegStruct(r, genState)
    |
      1 =>  GenRegInit(r, genState)
    END
  END GenReg;

PROCEDURE GenAddrmap(r : RegAddrmap.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  BEGIN
    CASE genPhase OF
      0 =>  GenContainerStruct(r, genState)
    |
      1 =>  GenContainerInit(r, genState)
    END
  END GenAddrmap;

PROCEDURE GenRegfile(r : RegRegfile.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  BEGIN
    CASE genPhase OF
      0 =>  GenContainerStruct(r, genState)
    |
      1 =>  GenContainerInit(r, genState)
    END
  END GenRegfile;

  (**********************************************************************)

TYPE GenPhase = [0..1];
     
VAR genPhase : GenPhase;
    
PROCEDURE ArrSz(a : RdlArray.Single) : CARDINAL =
  BEGIN
    IF a = NIL THEN
      RETURN 1
    ELSE
      RETURN BigInt.ToInteger(a.n.x)
    END
  END ArrSz;
  
PROCEDURE ComponentTypeName(c : RegComponent.T; gs : GenState) : TEXT =
  BEGIN
    TYPECASE c OF
      RegAddrmap.T(a) =>
      RETURN a.intfName(gs) 
    ELSE
      RETURN c.typeName(gs)
    END
  END ComponentTypeName;

BEGIN END RegC.
