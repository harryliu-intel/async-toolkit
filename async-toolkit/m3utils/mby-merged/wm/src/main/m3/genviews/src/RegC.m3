<<<<<<< HEAD
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
    gs : GenState := RegGenState.T.init(NEW(GenState, map := t.map), dirPath);
    intfNm := t.map.intfName(gs);
    fn := intfNm & ".h";
    path := dirPath & "/" & fn;
  BEGIN
    FOR i := FIRST(gs.wx) TO LAST(gs.wx) DO
      gs.wx[i] := Wx.New()
    END;

    t.map.generate(gs);

    (* do the actual output *)
    IF IndividualTypeFiles THEN
      (* this is the last pending symbol .. *)
      PushPendingOutput(gs)
    ELSE
      Debug.Out("Copying output to " & path);
      CopyWx(gs.wx, path)
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
  METHODS
    p(sec : Section; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsP;
    main(fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsMain;
  OVERRIDES
    put := PutGS;
    newSymbol := NewSymbol;
  END;

CONST IndividualTypeFiles = FALSE;
      
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

    (* if we are using "IndividualTypeFiles" we push out the output
       from the previous work before starting the next type, and do so
       into its own file. 

       if we are NOT using "IndividualTypeFiles" we instead accumulate
       all the output of ALL the types in a single big output stream
       and dump it at the end in Write().

       This slightly screwy design allows us to share more code with
       the Modula-3 generator, which generates each addrmap into its
       own file, but not every single individual type.
    *)
    
    IF IndividualTypeFiles AND res THEN

      PushPendingOutput(gs);

      gs.curSym := nm
    END;
    RETURN res
  END NewSymbol;
  
PROCEDURE PutGS(gs : GenState; sec : Section; txt : TEXT) =
  BEGIN
    Wx.PutText(gs.wx[sec], txt)
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
           nm = f.name(debug := TRUE),
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
                IdiomName(c.nm), Int(ArrSz(c.array)));
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
                  tn, IdiomName(c.nm))
        ELSE
          gs.main("  %s %s[%s];\n",
                  tn, IdiomName(c.nm), Int(ArrSz(c.array)))
        END
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
||||||| parent of 9af4afb... next version of C code
=======
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
    gs : GenState := RegGenState.T.init(NEW(GenState, map := t.map), dirPath);
    intfNm := t.map.intfName(gs);
    fn := intfNm & ".h";
    path := dirPath & "/" & fn;
  BEGIN
    FOR i := FIRST(gs.wx) TO LAST(gs.wx) DO
      gs.wx[i] := Wx.New()
    END;

    t.map.generate(gs);

    (* do the actual output *)
    IF IndividualTypeFiles THEN
      (* this is the last pending symbol .. *)
      PushPendingOutput(gs)
    ELSE
      Debug.Out("Copying output to " & path);
      CopyWx(gs.wx, path)
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
  METHODS
    p(sec : Section; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsP;
    main(fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsMain;
  OVERRIDES
    put := PutGS;
    newSymbol := NewSymbol;
  END;

CONST IndividualTypeFiles = FALSE;
      
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

    (* if we are using "IndividualTypeFiles" we push out the output
       from the previous work before starting the next type, and do so
       into its own file. 

       if we are NOT using "IndividualTypeFiles" we instead accumulate
       all the output of ALL the types in a single big output stream
       and dump it at the end in Write().

       This slightly screwy design allows us to share more code with
       the Modula-3 generator, which generates each addrmap into its
       own file, but not every single individual type.
    *)
    
    IF IndividualTypeFiles AND res THEN

      PushPendingOutput(gs);

      gs.curSym := nm
    END;
    RETURN res
  END NewSymbol;
  
PROCEDURE PutGS(gs : GenState; sec : Section; txt : TEXT) =
  BEGIN
    Wx.PutText(gs.wx[sec], txt)
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
    gs.main("package switch_wm.csr\n");
    gs.main("import switch_wm.csr.Memory._\n");
    gs.main("import switch_wm.csr.Memory.ImplicitConversions._\n");
    gs.main("import switch_wm.PrimitiveTypes._\n");
    gs.main("\n// %s:%s\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.main("class %s(parent : RdlHierarchy) extends RdlRegister[%s.Underlying](parent) {\n", myTn, myTn);
    FOR i := 0 TO r.fields.size()-1 DO
      WITH f  = r.fields.get(i),
           nm = f.name(debug := TRUE),
           dv = DefVal(f.defVal) DO
        gs.main("  def %s = new %s {\n", nm, StdFieldAttrs);
        gs.main("    val r = %s until %s\n", Int(f.lsb), Int(f.lsb+f.width));
        gs.main("    val resetValue = 0x%sl\n", BigInt.Format(dv,base:=16));
        gs.main("  }\n");
      END
    END;
    gs.main("  def fields = List("); PutFields(); gs.main(")\n");
    gs.main("  def resetableFields : List[HardwareResetable] = List("); PutFields(); gs.main(")\n");
    gs.main("  protected var underlyingState = 0xDEADBEEFl\n");
    gs.main("\n");

    (* GenRegInit(r, gs); *)
    gs.main("}\n\n");
    
    PutRegObject(myTn, gs);
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
    gs.main("\n// %s:%s\n", ThisFile(), Fmt.Int(ThisLine()));
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
                IdiomName(c.nm), Int(ArrSz(c.array)));
        IF rf.children.size() = 1 THEN
        END
      END
    END;

    gs.main("}\n");
    PutObject(myTn, gs);
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

    gs.main("\n// %s:%s\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.main("typedef struct {\n");
    FOR i := 0 TO map.children.size()-1 DO
      WITH c  = map.children.get(i),
           tn = ComponentTypeName(c.comp,gs) DO
        IF (c.array = NIL) THEN
          gs.main("  %s %s;\n",
                  tn, IdiomName(c.nm))
        ELSE
          gs.main("  %s %s[%s];\n",
                  tn, IdiomName(c.nm), Int(ArrSz(c.array)), tn)
        END
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

PROCEDURE PutObject(tn : TEXT; gs : GenState) =
  BEGIN
    gs.main("object %s {\n", tn);
    gs.main("  def apply(parent : RdlHierarchy) : %s = apply(Some(parent))\n", tn);
    gs.main("  def apply(parent : Option[RdlHierarchy] = None) : %s = {\n", tn);
    gs.main("    new %s(parent)\n", tn);
    gs.main("  }\n");
    (* what's that implicit def stuff in Michael's email? *)
    gs.main("}\n");
  END PutObject;

  PROCEDURE PutRegObject(tn : TEXT; gs : GenState) =
    BEGIN
      gs.main("object %s {\n", tn);
      gs.main("  def apply(parent : RdlHierarchy) : %s = {\n", tn);
      gs.main("    new %s(parent)\n", tn);
      gs.main("  }\n");
      gs.main("  type Underlying = U64\n");

      (* what's that implicit def stuff in Michael's email? *)
      gs.main("}\n");
    END PutRegObject;

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
>>>>>>> 9af4afb... next version of C code
