MODULE RegScala EXPORTS RegScala, RegScalaGenerators;
IMPORT GenViewsScala;
IMPORT RegReg, RegGenState, RegRegfile, RegAddrmap;
IMPORT OSError, Thread, Wr;
IMPORT Pathname, RegScalaGenState;
FROM RegScalaGenState IMPORT Section;
IMPORT Wx;
IMPORT RdlArray, BigInt;
FROM Compiler IMPORT ThisFile, ThisLine;
IMPORT Fmt;
FROM Fmt IMPORT Int, F;
IMPORT RegComponent;
FROM RegScalaConstants IMPORT IdiomName;
IMPORT Debug;
IMPORT AtomList, Atom;

(* stuff inherited from m3 *)
FROM RegModula3Utils IMPORT CopyWx, DefVal;

REVEAL
  T = GenViewsScala.Compiler BRANDED Brand OBJECT
  OVERRIDES
    write := Write;
  END;

PROCEDURE Write(t : T; dirPath : Pathname.T; phase : Phase)
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    gs : GenState := RegGenState.T.init(NEW(GenState, map := t.map), dirPath);
    intfNm := t.map.intfName(gs);
    fn := intfNm & ".scala";
    path := dirPath & "/" & fn;
  BEGIN
    gs.defFn := fn;
    FOR i := FIRST(gs.wx) TO LAST(gs.wx) DO
      gs.wx[i] := Wx.New()
    END;

    gs.main("package switch_wm\n");
    gs.main("import PrimitiveTypes._\n");

    t.map.generate(gs);

    (* do the actual output *)
    IF IndividualTypeFiles THEN
      (* the last object started *)
      path := gs.dirPath & "/" & gs.curSym & ".scala"
    END;
    
    Debug.Out("Copying output to " & path);
    CopyWx(gs.wx, path)
  END Write;

  (**********************************************************************)
  
TYPE
  GenState = RegScalaGenState.T OBJECT
    map : RegAddrmap.T; (* do we really need this? could refer to T instead *)
    curSym : TEXT := NIL;
    defFn : Pathname.T := NIL;
  METHODS
    p(sec : Section; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsP;
    main(fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsMain;
  OVERRIDES
    put := PutGS;
    newSymbol := NewSymbol;
  END;

CONST IndividualTypeFiles = TRUE;
      
PROCEDURE NewSymbol(gs : GenState; nm : TEXT) : BOOLEAN
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    res : BOOLEAN;
    path : Pathname.T;
  BEGIN
    res := RegGenState.T.newSymbol(gs, nm);

    IF IndividualTypeFiles AND res THEN
      IF gs.curSym = NIL THEN
        path := gs.dirPath & "/" & gs.defFn
      ELSE
        path := gs.dirPath & "/" & nm & ".scala"
      END;
      TRY
        Debug.Out("Copying output to " & path);
        CopyWx(gs.wx, path)
      EXCEPT
        OSError.E(x) => x := AtomList.Append(x,
                                             AtomList.List1(
                                                 Atom.FromText(": " & path)));
        RAISE OSError.E(x)
      END;

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
    gs.main("package switch_wm\n");
    gs.main("import PrimitiveTypes._\n");
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
    gs.main("package switch_wm\n");
    gs.main("import PrimitiveTypes._\n");
    gs.main("\n// %s:%s\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.main("class %s(parent : Option[RdlHierarchy]) extends RdlRegisterFile(parent) ", myTn);
    IF (rf.children.size() = 1) THEN
      WITH c = rf.children.get(0),
           tn = ComponentTypeName(c.comp,gs) DO
           gs.main("with DegenerateHierarchy[IndexedSeq[%s]]", tn);
      END
    END;
    gs.main("{\n");
    FOR i := 0 TO rf.children.size()-1 DO
      WITH c  = rf.children.get(i),
           tn = ComponentTypeName(c.comp,gs) DO
        gs.main("  val %s = IndexedSeq.fill[%s](%s)(%s(this))\n",
                IdiomName(c.nm), tn, Int(ArrSz(c.array)), tn);
        IF rf.children.size() = 1 THEN
          gs.main("  def next : IndexedSeq[%s] = %s\n", tn, IdiomName(c.nm));
        END
      END
    END;
    gs.main("  def children =");
        FOR i := 0 TO rf.children.size()-1 DO
              WITH c  = rf.children.get(i),
                   tn = ComponentTypeName(c.comp,gs) DO
                gs.main(" %s ::", IdiomName(c.nm))
              END
        END;
            gs.main("Nil\n");
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
    gs.main("package switch_wm\n");
    gs.main("import PrimitiveTypes._\n");
    gs.main("\n// %s:%s\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.main("class %s(parent : Option[RdlHierarchy]) extends RdlAddressMap(parent) {\n", myTn);
    FOR i := 0 TO map.children.size()-1 DO
      WITH c  = map.children.get(i),
           tn = ComponentTypeName(c.comp,gs) DO
        IF (c.array = NIL) THEN
          gs.main("  val %s = DegenerateIndexedSeq[%s](%s(this))\n",
                  IdiomName(c.nm), tn, tn)
        ELSE
          gs.main("  val %s = IndexedSeq.fill[%s](%s)(%s(this))\n",
                  IdiomName(c.nm), tn, Int(ArrSz(c.array)), tn)
        END
      END
    END;
    gs.main("  def children =");
    FOR i := 0 TO map.children.size()-1 DO
          WITH c  = map.children.get(i),
               tn = ComponentTypeName(c.comp,gs) DO
            gs.main(" %s ::", IdiomName(c.nm))
          END
    END;
    gs.main("Nil\n");
    gs.main("}\n");

    PutObject(myTn, gs);
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
    gs.main("  def apply() : %s = apply(None)\n", tn);
    gs.main("  def apply(parent : Option[RdlHierarchy]) : %s = {\n", tn);
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

BEGIN END RegScala.
