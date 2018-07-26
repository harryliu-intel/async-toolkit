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

<*FATAL BigInt.OutOfRange*>

VAR doDebug := Debug.DebugThis("C");

REVEAL
  T = GenViewsC.Compiler BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
  END;

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
    t.map.generate(gs);

    Debug.Out("Copying output to " & path);

    (* perform topo sort and produce output in that order *)
    WITH seq = gs.topo.sort(),
         wr  = FileWr.Open(path) DO
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
      Wr.Close(wr)
    END
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
    
    gs.curWx := Wx.New();
    gs.curSym := nm;
    EVAL gs.wxTbl.put(nm, gs.curWx);
    
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

TYPE
  Variant = RECORD ptr, sfx : TEXT END;

CONST
  Phases = ARRAY OF Variant { Variant {  "",       "" },
                              Variant { "*", "__addr" } };

PROCEDURE GenReg(r : RegReg.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    gs : GenState := genState;
    myTn := r.typeName(gs);

  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    FOR i := FIRST(Phases) TO LAST(Phases) DO
      WITH v = Phases[i] DO
        gs.main("typedef struct {\n");
        FOR i := 0 TO r.fields.size()-1 DO
          WITH f  = r.fields.get(i),
               nm = f.name(debug := FALSE) DO
            gs.main("  uint%s %s%s;\n", Int(f.width), v.ptr, nm);
          END
        END;
        gs.main("} %s%s;\n", myTn, v.sfx)
      END
    END
  END GenReg;

  (* the way this is coded, GenRegfile and GenAddrmap could be merged into
     one routine, viz., GenContainer *)
  
PROCEDURE GenRegfile(rf       : RegRegfile.T;
                     genState : RegGenState.T) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    gs : GenState := genState;
    myTn := rf.typeName(gs);
  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    FOR i := FIRST(Phases) TO LAST(Phases) DO
      WITH v = Phases[i] DO
        gs.main("typedef struct {\n");
        FOR i := 0 TO rf.children.size()-1 DO
          WITH c  = rf.children.get(i),
               tn = ComponentTypeName(c.comp,gs) DO
            gs.main("  %s%s %s[%s];\n", tn, v.sfx,
                    IdiomName(c.nm,FALSE), Int(ArrSz(c.array)));
            gs.noteDep(tn);
            IF rf.children.size() = 1 THEN
            END
          END
        END;
        gs.main("} %s%s;\n", myTn, v.sfx);
      END
    END;
    
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c = rf.children.get(i) DO
        <*ASSERT c.comp # NIL*>
        c.comp.generate(gs)
      END
    END
   END GenRegfile;

PROCEDURE GenAddrmap(map     : RegAddrmap.T; gsF : RegGenState.T) 
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    gs : GenState := gsF;
    myTn := map.typeName(gs);  
  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;

    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    FOR i := FIRST(Phases) TO LAST(Phases) DO
      WITH v = Phases[i] DO
        gs.main("typedef struct {\n");
        FOR i := 0 TO map.children.size()-1 DO
          WITH c  = map.children.get(i),
               tn = ComponentTypeName(c.comp,gs) DO
            IF (c.array = NIL) THEN
              gs.main("  %s%s %s;\n",
                      tn, v.sfx, IdiomName(c.nm,FALSE))
            ELSE
              gs.main("  %s%s %s[%s];\n",
                      tn, v.sfx, IdiomName(c.nm,FALSE), Int(ArrSz(c.array)))
            END;
            gs.noteDep(tn);
          END
        END;
        gs.main("} %s;\n", myTn);
      END
    END;

    FOR i := 0 TO map.children.size()-1 DO
      WITH c = map.children.get(i) DO
        <*ASSERT c.comp # NIL*>
        c.comp.generate(gs)
      END
    END
  END GenAddrmap;

  (**********************************************************************)
  
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
