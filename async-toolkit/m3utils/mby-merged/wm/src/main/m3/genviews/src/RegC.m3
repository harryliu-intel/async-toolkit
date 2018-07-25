MODULE RegC EXPORTS RegC, RegCGenerators;
IMPORT GenViewsC;
IMPORT RegReg, RegGenState, RegRegfile, RegAddrmap, RegField;
IMPORT OSError, Thread, Wr;
IMPORT Pathname, RegCGenState;
FROM RegCGenState IMPORT Section;
IMPORT Wx;
IMPORT RdlArray, BigInt;
FROM Compiler IMPORT ThisFile, ThisLine;
IMPORT Fmt;
FROM Fmt IMPORT Int, F;
IMPORT RegComponent;
IMPORT RegChildSeq;
IMPORT RegChild;
IMPORT CompAddr, CompRange;
FROM CompRange IMPORT Prop, PropNames;
FROM RegCConstants IMPORT IdiomName;
IMPORT RegFieldArraySort, RegFieldSeq;
IMPORT Debug;
IMPORT AtomList, Atom;
IMPORT Text;
IMPORT TextRefTbl;
IMPORT TextTopoSort;
IMPORT FileWr;

(* this stuff really shouldnt be in this module but in Main... *)
IMPORT RdlProperty, RdlExplicitPropertyAssign;
IMPORT RdlPropertyRvalueKeyword;
FROM RegProperty IMPORT GetKw, GetNumeric;

(* stuff inherited from m3 *)
FROM RegModula3Utils IMPORT CopyWx, DefVal;

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

PROCEDURE PushPendingOutput(gs : GenState)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  (* this routine is used to push out the pending output in case of
     "IndividualTypeFiles" *)
  VAR
    path : Pathname.T;
  BEGIN
    (* is it the first symbol, in that case there is no output yet *)
    IF gs.curSym = NIL THEN RETURN END;
      
    path := gs.dirPath & "/" & gs.curSym & ".scala";
    TRY
      Debug.Out("Copying output to " & path);
      CopyWx(gs.wx, path)
    EXCEPT
      OSError.E(x) => x := AtomList.Append(x,
                                           AtomList.List1(
                                               Atom.FromText(": " & path)));
      RAISE OSError.E(x)
    END
  END PushPendingOutput;

PROCEDURE ComponentInitName(c : RegComponent.T; gs : GenState) : TEXT =
  BEGIN
    RETURN "init__" & c.typeName(gs)
  END ComponentInitName;

  
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

(* addr map generation-related routines: *)

PROCEDURE FmtArr(a : RdlArray.Single) : TEXT =
  BEGIN
    IF a = NIL THEN
      RETURN ""
    ELSE
      RETURN F("ARRAY [0..%s-1] OF ",BigInt.Format(a.n.x))
     END
  END FmtArr;

PROCEDURE FmtArrFor(a : RdlArray.Single) : TEXT =
  BEGIN
    RETURN F("for( int i = 0 ; i < %s ; ++i ) {", BigInt.Format(a.n.x))
  END FmtArrFor;

VAR
  props : ARRAY Prop OF RdlProperty.T := MakeProps(PropNames)^;

PROCEDURE MakeProps(READONLY z : ARRAY OF TEXT) : REF ARRAY OF RdlProperty.T =
  VAR
    res := NEW(REF ARRAY OF RdlProperty.T, NUMBER(z));
  BEGIN
    FOR i := FIRST(z) TO LAST(z) DO
      res[i] := RdlProperty.Make(z[i])
    END;
    RETURN res
  END MakeProps;

CONST AddressingType = "Addressing";
      
CONST
   DefProp = ARRAY Prop OF TEXT {
    "32",
    "None",
    "32",
    AddressingType & ".Regalign"
  };

(* our C lib has meaningful memory units, so we can check them at generation-time *)
PROCEDURE FormatMemory(bits : CARDINAL) : TEXT =
  BEGIN
    IF bits MOD 8 = 0 THEN
      RETURN Fmt.Int(bits DIV 8) & ".bytes"
    ELSE
      RETURN Fmt.Int(bits) & ".bits"
    END
  END FormatMemory;

PROCEDURE GetPropText(prop : Prop; comp : RegComponent.T) : TEXT =
  VAR
    q : RdlExplicitPropertyAssign.T := comp.props.lookup(props[prop]);
  BEGIN
    IF q = NIL THEN
      (* return default *)
      RETURN DefProp[prop]
    ELSE
      (* parse result *)

      CASE prop OF 
        Prop.Addressing =>
        VAR
          a : CompAddr.Addressing;
        BEGIN
          CASE GetKw(q.rhs) OF
            RdlPropertyRvalueKeyword.T.compact =>
            a := CompAddr.Addressing.Compact
          |
            RdlPropertyRvalueKeyword.T.regalign =>
            a := CompAddr.Addressing.Regalign
          |
            RdlPropertyRvalueKeyword.T.fullalign =>
            a := CompAddr.Addressing.Fullalign
          ELSE
            <*ASSERT FALSE*>
          END;
          RETURN F("%s.%s", AddressingType, CompAddr.AddressingNames[a])
        END
      ELSE
        RETURN FormatMemory(GetNumeric(q.rhs))
      END
    END
  END GetPropText;
  
  (**********************************************************************)

CONST StdFieldAttrs = "RdlField with HardwareReadable with HardwareWritable with HardwareResetable";

PROCEDURE GenReg(r : RegReg.T; genState : RegGenState.T)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    gs : GenState := genState;
    myTn := r.typeName(gs);

  PROCEDURE PutFields( (* could restrict here *) ) =
    BEGIN
      FOR i := 0 TO r.fields.size()-1 DO
        WITH nm = r.fields.get(i).name(debug := FALSE) DO
          gs.main(nm);
          IF i # r.fields.size()-1 THEN gs.main(", ") END
        END
      END
    END PutFields;
    
  BEGIN
    IF NOT gs.newSymbol(myTn) THEN RETURN END;
    gs.main("\n/* %s:%s */\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.main("typedef struct {\n");
    FOR i := 0 TO r.fields.size()-1 DO
      WITH f  = r.fields.get(i),
           nm = f.name(debug := FALSE),
           dv = DefVal(f.defVal) DO
        gs.main("  uint%s %s;\n", Int(f.width), nm);
      END
    END;
    gs.main("} %s;\n\n", myTn);
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
    gs.main("typedef struct {\n");
    IF rf.children.size() = 1 THEN
      WITH c = rf.children.get(0),
           tn = ComponentTypeName(c.comp,gs) DO
      END
    END;
    gs.main("{\n");
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c  = rf.children.get(i),
           tn = ComponentTypeName(c.comp,gs) DO
        gs.main("  %s %s[%s];\n", tn, 
                IdiomName(c.nm,FALSE), Int(ArrSz(c.array)));
        gs.noteDep(tn);
        IF rf.children.size() = 1 THEN
        END
      END
    END;

    gs.main("} %s;\n", myTn);
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
    gs.main("typedef struct {\n");
    FOR i := 0 TO map.children.size()-1 DO
      WITH c  = map.children.get(i),
           tn = ComponentTypeName(c.comp,gs) DO
        IF (c.array = NIL) THEN
          gs.main("  %s %s;\n",
                  tn, IdiomName(c.nm,FALSE))
        ELSE
          gs.main("  %s %s[%s];\n",
                  tn, IdiomName(c.nm,FALSE), Int(ArrSz(c.array)))
        END;
        gs.noteDep(tn);
      END
    END;
    gs.main("} %s;\n", myTn);

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
