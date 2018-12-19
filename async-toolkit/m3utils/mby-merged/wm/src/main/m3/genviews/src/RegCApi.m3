MODULE RegCApi EXPORTS RegCApi, RegCApiGenerators;
IMPORT GenViewsCApi;
IMPORT RegReg, RegGenState, RegRegfile, RegAddrmap;
IMPORT OSError, Thread, Wr;
IMPORT Pathname, RegCGenState;
IMPORT Wx;
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
FROM GenCUtils IMPORT FmtConstant, PutXDecls, FmtArrSz, Variant, FieldType,
                      FmtFieldType, FmtFieldModifier, ArrSz;

TYPE CGPhase = RegCGenState.Phase;

VAR doDebug := Debug.DebugThis("REGCAPI");

REVEAL
  T = GenViewsCApi.Compiler BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
  END;

PROCEDURE Write(t : T; dirPath : Pathname.T; <*UNUSED*>phase : Phase)
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    wxTbl := NEW(TextRefTbl.Default).init();
    gs : RegCGenState.T := NEW(RegCGenState.T,
                               wxTbl := wxTbl,
                               map := t.map,
                               topo := NEW(TextTopoSort.T).init()).init(dirPath);
    intfNm := t.map.intfName(gs);
    fn := intfNm & ".h";
    path := dirPath & "/" & fn;
  BEGIN
    FOR ph := FIRST(CGPhase) TO LAST(CGPhase) DO
      EVAL RegGenState.T.init(gs, gs.dirPath); (* clear symbol table *)
      gs.phase := ph;
      t.map.generate(gs);

      CASE ph OF
        0 =>
        Debug.Out("Copying output to " & path);
        
        (* perform topo sort and produce output in that order *)
        WITH seq = gs.topo.sort(),
             wr  = FileWr.Open(path) DO
          Wr.PutText(wr, F("#ifndef %s_INCLUDED\n#define %s_INCLUDED\n\n",
                           intfNm, intfNm));
          (*
          DefTypes(wr);
          *)
          Wr.PutText(wr, "#include <assert.h>\n");
          Wr.PutText(wr, "#include \"uint.h\"\n");
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

  END Write;

  (**********************************************************************)

CONST
  Phases = ARRAY OF Variant { Variant { FieldType.UInt,       "" } };

PROCEDURE GenRegStruct(r : RegReg.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    gs : RegCGenState.T := genState;
    myTn                := r.typeName(gs);
    xDecls              := NEW(TextSeq.T).init();
  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    Debug.Out("Generating code for " & myTn);
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    FOR p := FIRST(Phases) TO LAST(Phases) DO
      WITH v = Phases[p] DO
        gs.main("typedef struct {\n");
        FOR i := 0 TO r.fields.size()-1 DO
          WITH f  = r.fields.get(i),
               nm = f.name(debug := FALSE),
               tn = F("uint%s", Int(f.width)) DO
            gs.main("  %s %s%s;\n",
                    FmtFieldType(tn,v.ptr),
                    FmtFieldModifier(v.ptr),
                    nm);
            IF p = FIRST(Phases) THEN
              FmtConstant(xDecls, Int(f.width), F("%s_%s", myTn, nm), "n");
              xDecls.addhi(F("typedef %s %s_%s_t;\n", tn, myTn, nm))
            END
          END
        END;
        gs.main("  uint8 __sync;\n");
        gs.main("} %s%s;\n\n", myTn, v.sfx)
      END
    END;
    GenProto(r, gs);
    gs.main(";\n\n");
    GenSyncProto(r, gs);
    gs.main(";\n\n");

    PutXDecls(gs, xDecls)
  END GenRegStruct;

PROCEDURE GenContainerStruct(rf       : RegContainer.T;
                             genState : RegGenState.T) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    gs : RegCGenState.T := genState;
    myTn := rf.typeName(gs);
    xDecls := NEW(TextSeq.T).init();
    skipArc := rf.skipArc();
  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    FOR p := FIRST(Phases) TO LAST(Phases) DO
      WITH v = Phases[p] DO
        (* skipArc : an array type 
           else    : a struct type *)
        gs.main("typedef ");
        IF NOT skipArc THEN gs.main("struct {\n") END;
        FOR i := 0 TO rf.children.size()-1 DO
          WITH c  = rf.children.get(i),
               tn = ComponentTypeName(c.comp,gs),
               nm = IdiomName(c.nm, FALSE) DO
            IF skipArc THEN
              <*ASSERT i=0*>
              gs.main("%s%s %s%s[%s];\n\n",
                      tn, v.sfx, myTn, v.sfx, Int(ArrSz(c.array)));
            ELSIF c.array = NIL THEN
              gs.main("  %s%s %s;\n", tn, v.sfx, nm)
            ELSE
              gs.main("  %s%s %s[%s];\n", tn, v.sfx, nm, Int(ArrSz(c.array)));
            END;
            IF c.array # NIL THEN
              (* constants for the array size *)
              IF p = 0 THEN FmtArrSz(xDecls, c.array, myTn & "_" & nm) END
            END;
            IF p = 0 AND NOT skipArc THEN
              (* convenience typedef *)
              IF c.array = NIL THEN
                xDecls.addhi(F("typedef %s %s_%s_t;\n", tn, myTn, nm))
              ELSE
                xDecls.addhi(F("typedef %s %s_%s_t[%s];\n", tn, myTn, nm,
                               Int(ArrSz(c.array))))
              END
            END;
            gs.noteDep(tn);
          END
        END;
        IF NOT skipArc THEN
          gs.main("  uint8 __sync;");
          gs.main("} %s%s;\n\n", myTn, v.sfx)
        END;
      END
    END;
    GenProto(rf, gs);
    gs.main(";\n\n");
    GenSyncProto(rf, gs);
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
  RAISES { } =
  CONST
    const = ""; (* or "const " *)
  VAR
    gs : RegCGenState.T := genState;
    myTn := r.typeName(gs);
  BEGIN
    gs.main("\n%svoid *\n%s__getptr(\n", const, myTn);
    FOR p := FIRST(Phases) TO LAST(Phases) DO
      gs.main("  %s%s%s *p%s,\n", const, myTn, Phases[p].sfx, Int(ORD(p)));
    END;
    gs.main("  const int *rp\n");
    gs.main(")");
  END GenProto;

  (**********************************************************************)

PROCEDURE GenRegGetptr(r : RegReg.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    gs : RegCGenState.T := genState;
    myTn := r.typeName(gs);

  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));

    GenProto(r, genState);
    gs.main("{\n");
    gs.main("  switch(*rp) {\n");
    gs.main("    case -1:\n");
    gs.main("      return p0;\n");
    gs.main("      break;\n");
    
    FOR i := 0 TO r.fields.size()-1 DO
      WITH f  = r.fields.get(i),
           nm = f.name(debug := FALSE) DO
        gs.main("    case %s:\n",Int(i));
        gs.main("      return &(p0->%s);\n",nm);
        gs.main("      break;\n");
      END
    END;
    gs.main("    default:\n");
    gs.main("      assert(0);\n");
    gs.main("  }\n");
    gs.main("}\n\n")
  END GenRegGetptr;

PROCEDURE GenContainerGetptr(rf       : RegContainer.T;
                           genState : RegGenState.T) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    gs : RegCGenState.T := genState;
    myTn := rf.typeName(gs);
    skipArc := rf.skipArc();
  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    GenProto(rf, gs);
    gs.main("{\n");
    gs.main("  switch(*rp){\n");
    gs.main("    case -1:\n");
    gs.main("      return p0;\n");
    gs.main("      break;\n");
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c  = rf.children.get(i),
           tn = ComponentTypeName(c.comp, gs),
           nm = IdiomName(c.nm,FALSE) DO
        IF skipArc THEN
          <*ASSERT i=0*>
          gs.main("    default:\n");
          gs.main("      assert(0 <= *rp && *rp < %s);\n", Int(ArrSz(c.array)));
          gs.main("      %s__getptr(&((*p0)[*rp]),rp+1);\n", tn);
        ELSIF c.array = NIL THEN
          gs.main("    case %s:\n",Int(i));
          gs.main("      return %s__getptr(&(p0->%s),rp+1);\n", tn, nm)
        ELSE
          gs.main("    case %s:\n", Int(i));
          gs.main("      assert(*rp<%s);\n", Int(ArrSz(c.array)));
          gs.main("      return %s__getptr(&(p0->%s[*rp]),rp+1);\n",
                  tn, nm);
        END;
        gs.main("      break;\n");
      END
    END;
    IF NOT skipArc THEN
      gs.main("    default:\n");
      gs.main("      assert(0);\n");
      gs.main("      break;\n");
    END;
    gs.main("  }\n");
    gs.main("}\n\n");
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c = rf.children.get(i) DO
        <*ASSERT c.comp # NIL*>
        c.comp.generate(gs)
      END
    END;
  END GenContainerGetptr;

  (**********************************************************************)

PROCEDURE GenSyncProto(  r : RegComponent.T; genState : RegGenState.T)
  RAISES { } =
  VAR
    gs : RegCGenState.T := genState;
    myTn := r.typeName(gs);
  BEGIN
    gs.main("\nconst uint8 *\n%s__sync(\n", myTn);
    FOR p := FIRST(Phases) TO LAST(Phases) DO
      gs.main("  const %s%s *p%s,\n", myTn, Phases[p].sfx, Int(ORD(p)));
    END;
    gs.main("  const int *rp\n");
    gs.main(")");
  END GenSyncProto;

  (**********************************************************************)

PROCEDURE GenRegSync(r : RegReg.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    gs : RegCGenState.T := genState;
    myTn := r.typeName(gs) & "__sync";

  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));

    GenSyncProto(r, genState);
    gs.main("{\n");
    gs.main("  switch(*rp) {\n");
    gs.main("    case -1:\n");
    gs.main("      return &(p0->__sync);\n");
    gs.main("      break;\n");
    gs.main("    default:\n");
    gs.main("      assert(0);\n");
    gs.main("  }\n");
    gs.main("}\n\n")
  END GenRegSync;

PROCEDURE GenContainerSync(rf       : RegContainer.T;
                           genState : RegGenState.T) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    gs : RegCGenState.T := genState;
    myTn := rf.typeName(gs) & "__sync";
    skipArc := rf.skipArc();
  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    GenSyncProto(rf, gs);
    gs.main("{\n");
    gs.main("  switch(*rp){\n");
    gs.main("    case -1:\n");
    IF skipArc THEN
      gs.main("      assert(0); /* p0 is an array */\n")
    ELSE
      gs.main("      return &(p0->__sync); /* p0 is not an array */\n")
    END;
    gs.main("      break;\n");
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c  = rf.children.get(i),
           tn = ComponentTypeName(c.comp, gs),
           nm = IdiomName(c.nm,FALSE) DO
        IF skipArc THEN
          <*ASSERT i=0*>
          gs.main("    default:\n");
          gs.main("      assert(0 <= *rp && *rp < %s);\n", Int(ArrSz(c.array)));
          gs.main("      %s__sync(&((*p0)[*rp]),rp+1);\n", tn);
        ELSIF c.array = NIL THEN
          gs.main("    case %s:\n",Int(i));
          gs.main("      return %s__sync(&(p0->%s),rp+1);\n", tn, nm)
        ELSE
          gs.main("    case %s:\n", Int(i));
          gs.main("      assert(*rp<%s);\n", Int(ArrSz(c.array)));
          gs.main("      return %s__sync(&(p0->%s[*rp]),rp+1);\n",
                  tn, nm);
        END;
        gs.main("      break;\n");
      END
    END;
    IF NOT skipArc THEN
      gs.main("    default:\n");
      gs.main("      assert(0);\n");
      gs.main("      break;\n");
    END;
    gs.main("  }\n");
    gs.main("}\n\n");
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c = rf.children.get(i) DO
        <*ASSERT c.comp # NIL*>
        c.comp.generate(gs)
      END
    END;
  END GenContainerSync;

  (**********************************************************************)

PROCEDURE GenReg(r : RegReg.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  BEGIN
    CASE NARROW(genState, RegCGenState.T).phase OF
      0 =>  GenRegStruct(r, genState)
    |
      1 =>  GenRegGetptr(r, genState);
            GenRegSync(r, genState)
    END
  END GenReg;

PROCEDURE GenAddrmap(r : RegAddrmap.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  BEGIN
    CASE NARROW(genState, RegCGenState.T).phase OF
      0 =>  GenContainerStruct(r, genState)
    |
      1 =>  GenContainerGetptr(r, genState);
            GenContainerSync(r, genState)
    END
  END GenAddrmap;

PROCEDURE GenRegfile(r : RegRegfile.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  BEGIN
    CASE NARROW(genState, RegCGenState.T).phase OF
      0 =>  GenContainerStruct(r, genState)
    |
      1 =>  GenContainerGetptr(r, genState);
            GenContainerSync(r, genState)
    END
  END GenRegfile;

  (**********************************************************************)

BEGIN END RegCApi.
