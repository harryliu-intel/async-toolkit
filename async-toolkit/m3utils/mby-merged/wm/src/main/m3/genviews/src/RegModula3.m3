MODULE RegModula3 EXPORTS RegModula3, RegModula3Generators;
IMPORT Wr, Thread, RegAddrmap;
IMPORT BigInt;
FROM Fmt IMPORT F;
IMPORT Pathname, OSError;
IMPORT FileWr;
IMPORT RegReg, RegRegfile, RegField;
IMPORT Fmt, Word, Debug;
IMPORT Wx;
IMPORT RdlArray;
IMPORT TextSetDef;
IMPORT RegComponent;
IMPORT RegModula3Naming AS Naming;
FROM RegModula3Naming IMPORT M3Camel;
IMPORT RegGenState;
IMPORT RegChild;
IMPORT CompAddr, CompRange;
FROM CompRange IMPORT Prop, PropNames;
FROM Compiler IMPORT ThisLine, ThisFile;
IMPORT RegFieldArraySort, RegFieldSeq;
FROM RegModula3GenState IMPORT Section;
IMPORT RegModula3GenState;
IMPORT TextSet;
IMPORT RegContainer;
IMPORT CardSet, CardSetDef;

(* this stuff really shouldnt be in this module but in Main... *)
IMPORT RdlProperty, RdlExplicitPropertyAssign;
IMPORT RdlPropertyRvalueKeyword;
FROM RegProperty IMPORT GetKw, GetNumeric;

<*FATAL BigInt.OutOfRange*>

CONST LastISection = Section.ITrailer;
      FirstMSection = Section.MImport;

  (**********************************************************************)

  (* move Genstate into its own files...? *)
      
TYPE
  GenState = RegModula3GenState.T OBJECT
    map         : RegAddrmap.T;
    fieldWidths : CardSet.T;
  METHODS
    init(o : GenState) : GenState := InitGS;
    p(sec : Section; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsP;
    mdecl(fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsMdecl;
    imain(fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL) := GsImain;

    defProc(c : RegComponent.T; ofType : ProcType; VAR pnm : TEXT; intf := TRUE) : BOOLEAN := DefProc;
    (* if returns FALSE, do not generate code! *)
    
  OVERRIDES
    put  := PutGS;
  END;

CONST DeclFmt = ARRAY ProcType OF TEXT {
  "PROCEDURE %s(VAR t : %s; READONLY a : %s; VAR op : CsrOp.T)",
  "PROCEDURE %s(READONLY a : %s) : CompRange.T"
  };
(* we could extend this pattern to other procedure definitions ... *)
  
PROCEDURE DefProc(gs     : GenState;
                  c      : RegComponent.T;
                  ofType : ProcType;
                  VAR pnm: TEXT;
                  intf   : BOOLEAN) : BOOLEAN =
  VAR
    ttn := ComponentTypeNameInHier(c, gs, TypeHier.Read);
    atn := ComponentTypeNameInHier(c, gs, TypeHier.Addr);
  BEGIN
    pnm := ComponentName[ofType](c, gs);
    IF gs.dumpSyms.insert(pnm) THEN RETURN FALSE END;

    CASE ofType OF
      ProcType.Csr =>
      gs.mdecl(DeclFmt[ofType] & "= \n", pnm, ttn, atn);
      IF intf THEN gs
        .imain(DeclFmt[ofType] & ";\n", pnm, ttn, atn);
      END
    |
      ProcType.Range =>
      gs.mdecl(DeclFmt[ofType] & "= \n", pnm, atn);
      IF intf THEN gs
        .imain(DeclFmt[ofType] & ";\n", pnm, atn);
      END
    END;
    RETURN TRUE
  END DefProc;

PROCEDURE GsP(gs : GenState;
              sec : Section;
              fmt : TEXT;
              t1, t2, t3, t4, t5 : TEXT) =
  BEGIN gs.put(sec, F(fmt, t1, t2, t3, t4, t5)) END GsP;
  
PROCEDURE GsMdecl(gs : GenState; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL)=
  BEGIN gs.p(Section.MDecl, fmt, t1, t2, t3, t4, t5) END GsMdecl;
  
PROCEDURE GsImain(gs : GenState; fmt : TEXT; t1, t2, t3, t4, t5 : TEXT := NIL)=
  BEGIN gs.p(Section.IMaintype, fmt, t1, t2, t3, t4, t5) END GsImain;
  

PROCEDURE InitGS(n, o : GenState) : GenState =
  BEGIN
    n.map := o.map;
    n.wx := o.wx;
    n.dumpSyms := o.dumpSyms;
    n.rw := o.rw;
    n.dirPath := o.dirPath;
    n.th := o.th;
    RETURN n
  END InitGS;

PROCEDURE PutGS(gs : GenState; sec : Section; txt : TEXT) =
  BEGIN
    Wx.PutText(gs.wx[sec], txt)
  END PutGS;

  (**********************************************************************)

  (* basic idea:

     an addrmap can "contain" other addrmaps, regfiles, and regs.

     an addrmap corresponds to a Modula-3 interface (actually two), 
     with three types.

     T : a RECORD that matches the RDL, for read-only use (state -> WM)

     U : a RECORD that matches the RDL, for write-only use (WM -> state)

     A : a RECORD that matches the RDL, with addressing info

     Normal white-model use can be expected to rely on T (mainly) and
     U (for updates).

     regfiles and regs tend to be RECORDS, but there is an exception
     for regfiles that have only a single (syntactic) member: these
     are ARRAYs (see skipArc in the code).

     There are a few matching codes generated...

     Init : initialize the addresses in the A RECORD

     UpdateInit : initialize the updaters in the U RECORD

     CsrAccess : push a write down the tree to the leaves of the T
                 record.  This is how a write into U gets reflected in
                 T.  It also allows for "software writes" using
                 regular memory addressing with the addresses per the
                 RDL definitions used.

     The two interfaces appear in the code as "RW.R" (for the XXX_map.i3)
     and "RW.W" (for the XXX_map_addr.i3).

     XXX_map.T is in XXX_map.

     XXX_map_addr.U and XXX_map_addr.A are in XXX_map_addr.

     T, U, and A appear as TypeHier.Read, TypeHier.Update, and
     TypeHier.Addr, respectively 
  *)

VAR mapsDone := NEW(TextSetDef.T).init();
    (* global set of addrmaps that we have generated so far *)

VAR doDebug := Debug.DebugThis("RegModula3");
    
REVEAL
  T = Public BRANDED Brand OBJECT
    map  : RegAddrmap.T;
    addr : BigInt.T;
  OVERRIDES
    init  := Init;
    write := Write;
  END;

PROCEDURE Init(t : T; map : RegAddrmap.T) : T =
  BEGIN
    t.map := map;
    t.addr := BigInt.Zero;
    RETURN t
  END Init;

PROCEDURE Write(t : T; dirPath : Pathname.T; rw : RW) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
  VAR
    gs := NEW(GenState,
              dumpSyms    := NIL, (* ? *)
              rw          := rw,
              dirPath     := dirPath,
              map         := t.map,
              i3imports   := NEW(TextSetDef.T).init(),
              m3imports   := NEW(TextSetDef.T).init(),
              fieldWidths := NEW(CardSetDef.T).init()
              );
    intfNm := t.map.intfName(gs);
    iPath := dirPath & "/" & intfNm & ".i3";
    mPath := dirPath & "/" & intfNm & ".m3";
  BEGIN
    IF mapsDone.insert(intfNm) THEN RETURN END;
    FOR i := FIRST(gs.wx) TO LAST(gs.wx) DO
      gs.wx[i] := Wx.New()
    END;
    gs.put(Section.IImport, F("INTERFACE %s;\n", intfNm));
    gs.put(Section.IComponents, F("CONST Brand = \"%s\";\n", intfNm));

    gs.put(Section.MImport, F("MODULE %s;\n", intfNm));

    CASE rw OF
      RW.W  =>
      EVAL gs.i3imports.insert(Naming.MapIntfNameRW(t.map, RW.R));
      EVAL gs.i3imports.insert("CompRange");
      EVAL gs.i3imports.insert("CompPath");
      EVAL gs.i3imports.insert("CsrOp");
      EVAL gs.i3imports.insert("CompAddr");
      
      EVAL gs.m3imports.insert("Word");
      EVAL gs.m3imports.insert("CsrOp");
      EVAL gs.m3imports.insert("CompAddr");
      EVAL gs.m3imports.insert(Naming.MapIntfNameRW(t.map, RW.R));
      EVAL gs.m3imports.insert("CompRange");
      EVAL gs.m3imports.insert("CompPath");
      EVAL gs.m3imports.insert("CompMemory");
      EVAL gs.m3imports.insert("Debug");
      EVAL gs.m3imports.insert("CompMemoryListener");
    |
      RW.R =>
    END;

    gs.put(Section.MImport, F("\n"));

    gs.put(Section.MCode, "BEGIN\n");
    
    FOR th := FIRST(TypeHier) TO LAST(TypeHier) DO
      IF TypePhase[th] = gs.rw THEN
        gs.th := th;
        (* set the hierarchy, this is how children know which type to dump *)
        
        t.map.generate(gs)
      END
    END;
    gs.put(Section.ITrailer,
           F("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine())));

    gs.put(Section.MImport, F("\n"));
    gs.put(Section.ITrailer, F("END %s.\n", intfNm));
    gs.put(Section.MTrailer, F("END %s.\n", intfNm));


    DumpImports(gs.i3imports, gs, Section.IImport);
    DumpImports(gs.m3imports, gs, Section.MImport);

    gs.put(Section.IImport, F("\n"));
    gs.put(Section.MImport, F("\n"));

    CopyWx(SUBARRAY(gs.wx,
                    ORD(FIRST(gs.wx)),
                    ORD(LastISection)-ORD(FIRST(gs.wx))+1),
           iPath);
    CopyWx(SUBARRAY(gs.wx,
                    ORD(FirstMSection),
                    ORD(LAST(gs.wx))-ORD(FirstMSection)+1),
           mPath);

    WITH m3mWr = FileWr.OpenAppend(dirPath & "/m3makefile.maps") DO
      Wr.PutText(m3mWr, F("module(\"%s\")\n",intfNm));
      Wr.Close(m3mWr)
    END
  END Write;

PROCEDURE DumpImports(set : TextSet.T; gs : GenState; sec : Section) =
  VAR
    iter := set.iterate();
    t : TEXT;
  BEGIN
    WHILE iter.next(t) DO
      gs.put(sec, F("IMPORT %s;\n", t))
    END
  END DumpImports;

PROCEDURE CopyWx(READONLY wx : ARRAY OF Wx.T; to : Pathname.T)
  (* copy an array of Wx.Ts in order to an output file *)
  RAISES { OSError.E, Wr.Failure, Thread.Alerted } =
  VAR
    wr := FileWr.Open(to);
  BEGIN
    FOR i := FIRST(wx) TO LAST(wx) DO
      Wr.PutText(wr, Wx.ToText(wx[i]))
    END;
    Wr.Close(wr)
  END CopyWx;

  (**********************************************************************)
  
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
    RETURN F("FOR i := 0 TO %s-1 DO", BigInt.Format(a.n.x))
  END FmtArrFor;
  
  (**********************************************************************)

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

PROCEDURE GetPropText(prop : Prop; comp : RegComponent.T) : TEXT =
  VAR
    q : RdlExplicitPropertyAssign.T := comp.props.lookup(props[prop]);
  BEGIN
    IF q = NIL THEN
      (* return default *)
      RETURN CompRange.DefProp[prop]
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
          RETURN F("CompAddr.Addressing.%s", CompAddr.AddressingNames[a])
        END
      ELSE
        RETURN Fmt.Int(GetNumeric(q.rhs))
      END
    END
  END GetPropText;

  (* <--- ugly to have this twice ---> *)
  
PROCEDURE GetAddressingProp(comp : RegComponent.T) : CompAddr.Addressing =
  VAR
    q : RdlExplicitPropertyAssign.T :=
        comp.props.lookup(props[CompRange.Prop.Addressing]);
  BEGIN
    IF q = NIL THEN
      (* return default *)
      RETURN CompAddr.Addressing.Regalign
    ELSE
      (* parse result *)

      CASE GetKw(q.rhs) OF
        RdlPropertyRvalueKeyword.T.compact =>
        RETURN CompAddr.Addressing.Compact
      |
        RdlPropertyRvalueKeyword.T.regalign =>
        RETURN CompAddr.Addressing.Regalign
      |
        RdlPropertyRvalueKeyword.T.fullalign =>
        RETURN CompAddr.Addressing.Fullalign
      ELSE
        <*ASSERT FALSE*>
      END
    END
  END GetAddressingProp;
  
PROCEDURE GetPropTexts(c : RegComponent.T) : ARRAY Prop OF TEXT =
  VAR
    res : ARRAY Prop OF TEXT;
  BEGIN
    FOR i := FIRST(Prop) TO LAST(Prop) DO
      res[i] := GetPropText(i, c)
    END;
    RETURN res
  END GetPropTexts;

PROCEDURE FormatPropArgs(READONLY args : ARRAY Prop OF TEXT) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := FIRST(args) TO LAST(args) DO
      res := res & F(", %s := %s", PropNames[i], args[i])
    END;
    RETURN res
  END FormatPropArgs;
  
PROCEDURE GenChildInit(e          : RegChild.T;
                       gs         : GenState;
                       addressing : CompAddr.Addressing;
                       skipArc := FALSE) =
  VAR
    childArc : TEXT;
    atS      : TEXT;
  BEGIN
    (* special case for array with only one child is that it is NOT
       a record *)
    IF skipArc THEN
      childArc := "";
    ELSE
      childArc := "." & M3Camel(e.nm,debug := FALSE);
    END;

    IF doDebug THEN
      Debug.Out("GenChildInit " & e.nm & " -> \"" & childArc & "\"")
    END;
    
    FOR i := FIRST(props) TO LAST(props) DO
      VAR q   := e.comp.props.lookup(props[i]);
          dbg : TEXT := "**NIL**";
      BEGIN
        IF q # NIL THEN
          dbg := F("{ %s }", RdlExplicitPropertyAssign.Format(q))
        END;
        IF doDebug THEN
          Debug.Out(F("RdlProperty %s = %s",
                      RdlProperty.Format(props[i]),
                      dbg))
        END
      END
    END;
    
    IF e.at = RegChild.Unspecified AND e.mod = RegChild.Unspecified THEN
      atS := "at"
    ELSIF e.at # RegChild.Unspecified THEN
      atS := F("CompAddr.PlusBytes(base,16_%s)",
               Fmt.Int(BigInt.ToInteger(e.at.x), base := 16))
    ELSIF e.mod # RegChild.Unspecified THEN
      atS := F("CompAddr.ModAlign(at, 16_%s)",
               Fmt.Int(BigInt.ToInteger(e.mod.x), base := 16))
    END;
    
    IF e.array = NIL THEN
      gs.mdecl(
             "    at := mono.increase(at,%s(x%s, %s, CompPath.Cat(path,\"%s\")));\n",
               ComponentInitName(e.comp,gs),
               childArc,
               atS,
               childArc);
      IF NOT skipArc THEN
        gs.mdecl("    x.tab[c] := at; INC(c);\n");
      END
    ELSE
      (* e.array # NIL *)
      gs.mdecl("    VAR\n");
      gs.mdecl("      q := %s;\n", atS);
      gs.mdecl("    BEGIN\n");

      IF addressing = CompAddr.Addressing.Fullalign THEN
        (* cases : 
           (0) calc overriden by @ or %= 
           (1) stride given, then that is what we use to align
           (2) stride not given, then we need to calculate size of
               element
        *)
        IF e.at # RegChild.Unspecified OR e.mod # RegChild.Unspecified THEN
          (* skip , fall back to not using fullalign *)
        ELSIF e.stride # RegChild.Unspecified THEN
          WITH alignTo = BigInt.ToInteger(BigInt.Mul(e.array.n.x,
                                                     e.stride.x)) DO
            IF e.at = RegChild.Unspecified AND e.mod = RegChild.Unspecified THEN
              gs.mdecl("      q := CompAddr.Align(at,%s);\n",
                                     Fmt.Int(alignTo))
            END
          END
        ELSE
          (* fullalign given, stride not given, mod not given, at not given *)
          (* make a throwaway "first" and "second", measure distance between,
             then align at to that and proceed *)
          gs.mdecl("      VAR first, second : CompRange.T; BEGIN\n");
          
          gs.mdecl("        first := %s(x%s[0], CompAddr.Zero, NIL);\n",
                                 ComponentInitName(e.comp,gs),
                                 childArc);
          gs.mdecl("        second := %s(x%s[1], CompRange.Lim(first), NIL);\n",
                                 ComponentInitName(e.comp,gs),
                                 childArc);
          gs.mdecl("        <*ASSERT first # second*>\n");
          gs.mdecl("        WITH len = CompAddr.DeltaBytes(CompRange.Lim(second),CompRange.Lim(first)) DO\n");
          gs.mdecl("          at := CompAddr.ModAlign(at, CompAddr.NextPower(len));\n");
          gs.mdecl("          q := at\n");
          gs.mdecl("        END\n");
          gs.mdecl("      END;\n")
        END
      END;
      
      gs.mdecl("      %s\n",FmtArrFor(e.array));
      gs.mdecl("        at := mono.increase(at,%s(x%s[i], q, CompPath.CatArray(path,\"%s\",i)));\n",
               ComponentInitName(e.comp,gs),
               childArc,
               childArc);
      IF NOT skipArc THEN
        gs.mdecl("        x.tab[c] := at; INC(c);\n");
      END;
      IF e.stride # RegChild.Unspecified THEN
        gs.mdecl("        q := CompAddr.PlusBytes(q,16_%s);\n",
                               Fmt.Int(BigInt.ToInteger(e.stride.x), base := 16))
      ELSE
        gs.mdecl("        q := at;\n")
      END;
      gs.mdecl("      END\n");
      gs.mdecl("    END;\n")
    END
  END GenChildInit;
  
  (**********************************************************************)

PROCEDURE GenAddrmapRecord(map            : RegContainer.T;
                           gs             : GenState)
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =
  VAR
    ccnt : CARDINAL := 0;
    file := ThisFile(); line := Fmt.Int(ThisLine());
  BEGIN
    IF gs.dumpSyms.insert(map.typeName(gs)) THEN RETURN END;
    gs.put(Section.IMaintype, "\n");
    gs.put(Section.IMaintype, "TYPE\n");
    gs.put(Section.IMaintype,
           F("  %s = RECORD (* %s:%s *)\n",
             MainTypeName[gs.th],
             file,
             line)
          );

    (* header for main type *)
    FOR i := 0 TO map.children.size()-1 DO
      WITH e = map.children.get(i) DO
        TYPECASE e.comp OF
          RegAddrmap.T(map) =>

          WITH iNm = map.intfName(gs)DO
            EVAL gs.i3imports.insert(iNm);
            CASE gs.rw OF RW.W =>  EVAL gs.m3imports.insert(iNm) ELSE END
          END;
         
          WITH sub = NEW(T).init(map) DO
            sub.write(gs.dirPath, gs.rw)
          END
        ELSE
          e.comp.generate(gs)
        END;
        gs.put(Section.IMaintype, F("    %s : %s%s;\n",
                                    M3Camel(e.nm),
                                    FmtArr(e.array),
                                    ComponentTypeName(e.comp,
                                                      gs)));
        INC(ccnt,ArrayCnt(e.array));

      END
    END;

    CASE gs.th OF
      TypeHier.Addr =>
      gs.put(Section.IMaintype, F("    tab : ARRAY[0..%s+1-1] OF CompAddr.T;\n",
                                  Fmt.Int(ccnt)));
      gs.put(Section.IMaintype, F("    nonmono := FALSE;\n"));
      gs.put(Section.IMaintype, F("    monomap : REF ARRAY OF CARDINAL;\n"));
      gs.put(Section.IMaintype, F("    min, max: CompAddr.T;\n"));
    |
      TypeHier.Read => (* skip *)
    |
      TypeHier.Update =>          gs.put(Section.IMaintype, F("    updater : %s;\n",
                                      Updater(
                                          ComponentTypeNameInHier(map,
                                                                  gs,
                                                                  TypeHier.Read)
        )
        ))
     END;

    gs.put(Section.IMaintype, "  END;\n");
    gs.put(Section.IMaintype, "\n")
  END GenAddrmapRecord;
  
PROCEDURE GenAddrmap(map     : RegAddrmap.T; gsF : RegGenState.T) 
  RAISES { OSError.E, Thread.Alerted, Wr.Failure } =

  (* 
     generate a 

       TYPE HIERARCHY

     starting from an addrmap 
  *)

  VAR
    gs : GenState := gsF;
  BEGIN
    gs.put(Section.IMaintype, F("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine())));
    
    gs.dumpSyms := NEW(TextSetDef.T).init();
    (* clear symbols dumped, so we can re-generate the entire hier *)
    
    GenAddrmapRecord(map, gs);
    (* generate types and dependencies, starting from addrmap *)
    
    (* last, generate procedures to deal with the top-level type *)
    CASE gs.th OF
      TypeHier.Addr =>  GenAddrmapInit(map, gs)
    |
      TypeHier.Read =>  
    |
      TypeHier.Update =>
      GenAddrmapGlobal    (map, gs);
      GenAddrmapUpdateInit(map, gs);
      GenAddrmapCsr       (map, gs);
      GenAddrmapRanger    (map, gs)
    END
  END GenAddrmap;

PROCEDURE ArrayCnt(a : RdlArray.Single) : CARDINAL =
  BEGIN
    IF a = NIL THEN RETURN 1 ELSE RETURN BigInt.ToInteger(a.n.x) END
  END ArrayCnt;

PROCEDURE GenAddrmapInit(map : RegAddrmap.T; gs : GenState) =
  (* generate interface for address map for struct *)
  BEGIN
    gs.put(Section.IMaintype,
           F("PROCEDURE Init(VAR x : %s; at : CompAddr.T; path : CompPath.T) : CompRange.T;\n", MainTypeName[gs.th]));
    gs.put(Section.IMaintype, "\n");
    
    gs.mdecl(
           F("PROCEDURE Init(VAR x : %s; at : CompAddr.T; path : CompPath.T) : CompRange.T =\n", MainTypeName[gs.th]));
    gs.mdecl("  (* %s:%s *)\n",ThisFile(),Fmt.Int(ThisLine()));

    gs.mdecl("  VAR\n");
    gs.mdecl("    base := at;\n");
    gs.mdecl("    c := 0;\n");
    gs.mdecl("    mono := NEW(CompRange.Monotonic).init();\n");
    gs.mdecl("  BEGIN\n");
    gs.mdecl("    x.tab[c] := at; INC(c);\n");

    FOR i := 0 TO map.children.size()-1 DO
      GenChildInit(map.children.get(i),
                   gs, 
                   GetAddressingProp(map)
                   )
    END;
    BuildTab(gs, map.intfName(gs));
    gs.mdecl("    RETURN CompRange.From2(base,at)\n");
    gs.mdecl("  END Init;\n");
    gs.mdecl("\n");
  END GenAddrmapInit;

PROCEDURE GenAddrmapUpdateInit(map : RegAddrmap.T; gs : GenState) =
  BEGIN
    gs.put(Section.IMaintype,
           F("PROCEDURE UpdateInit(VAR x : %s; READONLY a : %s; m : CompMemory.T);\n",
             MainTypeName[TypeHier.Update],
             MainTypeName[TypeHier.Addr]));
    gs.mdecl(
           F("PROCEDURE UpdateInit(VAR x : %s; READONLY a : %s; m : CompMemory.T) =\n",
             MainTypeName[TypeHier.Update],
             MainTypeName[TypeHier.Addr]));
    gs.mdecl("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.mdecl("  BEGIN\n");

    FOR i := 0 TO map.children.size()-1 DO
      GenChildUpdateInit(map.children.get(i), gs, FALSE)
    END;
    gs.mdecl("  END UpdateInit;\n");
    gs.mdecl("\n");

    (* generate field updaters *)
    GenFieldUpdaters(gs)
  END GenAddrmapUpdateInit;

PROCEDURE GenFieldUpdaters(gs : GenState) =
  VAR
    iter := gs.fieldWidths.iterate();
    c : CARDINAL;
  BEGIN
    (* generate declarations for interface file *)
    (* and code for implementation *)
    gs.mdecl("\n");
    WHILE iter.next(c) DO GenFieldUpdater(c,gs) END
  END GenFieldUpdaters;

PROCEDURE GenFieldUpdater(c : CARDINAL; gs : GenState) =
  VAR
    cs   := Fmt.Int(c);
    type := M3FieldWidthType(c,TypeHier.Read,gs);
  BEGIN
    gs.put(Section.IComponents, F("TYPE\n"));
    gs.put(Section.IComponents, F("  UObj%s = OBJECT METHODS\n", cs));
    gs.put(Section.IComponents, F("    u(READONLY x : %s);\n", type));
    gs.put(Section.IComponents, F("    updater() : UObj%s;\n", cs));
    gs.put(Section.IComponents, F("  END;\n"));
    gs.put(Section.IComponents, F("\n"));

    gs.mdecl("TYPE\n");
    gs.mdecl("  UObjConcrete%s= UObj%s OBJECT\n", cs, cs);
    gs.mdecl("    m    : CompMemory.T;\n");
    gs.mdecl("    addr : CompAddr.T;\n");
    gs.mdecl("    h    : H;\n");
    gs.mdecl("  OVERRIDES\n");
    gs.mdecl("    u := UpdateField%s;\n", cs);
    gs.mdecl("    updater := ReturnMe%s;\n", cs);
    gs.mdecl("  END;\n");
    gs.mdecl("\n");
    gs.mdecl("PROCEDURE ReturnMe%s(o : UObjConcrete%s) : UObj%s = \n", cs, cs, cs);
    gs.mdecl("  BEGIN RETURN o END ReturnMe%s;\n", cs);
    gs.mdecl("\n");
    gs.mdecl("PROCEDURE UpdateField%s(o : UObjConcrete%s; READONLY x : %s) =\n",
                            cs, cs, type);
    gs.mdecl("  VAR\n");
    IF c > BITSIZE(Word.T) THEN
      gs.mdecl("    op := CsrOp.MakeWideWrite(o.addr, x);\n")
    ELSE
      gs.mdecl("    op := CsrOp.MakeWrite(o.addr, %s, x);\n",cs)
    END;
    gs.mdecl("  BEGIN\n");
    gs.mdecl("    EVAL o.m.csrOp(op)\n");
    gs.mdecl("  END UpdateField%s;\n", cs);
    gs.mdecl("\n");
  END GenFieldUpdater;

PROCEDURE GenChildUpdateInit(e          : RegChild.T;
                             gs         : GenState;
                             skipArc := FALSE) =
  VAR
    childArc : TEXT;
  BEGIN
    (* special case for array with only one child is that it is NOT
       a record *)
    IF skipArc THEN
      childArc := "";
    ELSE
      childArc := "." & M3Camel(e.nm,debug := FALSE);
    END;

    IF e.array = NIL THEN
      gs.mdecl(
               "    %s(x%s,a%s,m);\n",
               ComponentInitName(e.comp,gs),
               childArc,
               childArc )
    ELSE
      gs.mdecl("    %s\n",FmtArrFor(e.array));
      gs.mdecl(
               "      %s(x%s[i],a%s[i],m);\n",
               ComponentInitName(e.comp,gs),
               childArc,
               childArc );
      gs.mdecl( "    END;\n");
   END
  END GenChildUpdateInit;

PROCEDURE GenRegfileUpdateInit(rf : RegRegfile.T; gs : GenState) =
  VAR
    iNm := ComponentInitName(rf, gs);
    skipArc := rf.children.size() = 1;
    utn := ComponentTypeNameInHier(rf, gs, TypeHier.Update);
    atn := ComponentTypeNameInHier(rf, gs, TypeHier.Addr);
  BEGIN
    gs.mdecl(
             "PROCEDURE %s(VAR x : %s; READONLY a : %s; m : CompMemory.T) =\n",
             iNm,
             utn, atn);

    gs.mdecl("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.mdecl("  BEGIN\n");

    FOR i := 0 TO rf.children.size()-1 DO
      GenChildUpdateInit(rf.children.get(i), gs, skipArc)
    END;
    gs.mdecl("  END %s;\n",iNm);
    gs.mdecl("\n");
  END GenRegfileUpdateInit;
  
PROCEDURE GenRegUpdateInit(r : RegReg.T; gs : GenState) =
  VAR
    iNm := ComponentInitName(r, gs);
    utn := ComponentTypeNameInHier(r, gs, TypeHier.Update);
    atn := ComponentTypeNameInHier(r, gs, TypeHier.Addr);
  BEGIN
    gs.mdecl(
             "PROCEDURE %s(VAR x : %s; READONLY a : %s; m : CompMemory.T) =\n", iNm, utn, atn);
    gs.mdecl("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.mdecl("  BEGIN\n");

    FOR i := 0 TO r.fields.size()-1 DO
      WITH f  = r.fields.get(i),
           ws = Fmt.Int(f.width),
           nm = f.name(debug := FALSE) DO

        IF f.width = BITSIZE(Word.T) THEN
          EVAL gs.i3imports.insert("Word")
        END;

        gs.mdecl(
                 "    x.%s := NEW(UObjConcrete%s, addr := a.%s.pos, m := m);\n", nm, ws, nm);
        EVAL gs.fieldWidths.insert(f.width)
      END
    END;
    gs.mdecl("  END %s;\n", iNm);
    gs.mdecl("\n");
    
  END GenRegUpdateInit;

  (**********************************************************************)
  
PROCEDURE GenAddrmapGlobal(map : RegAddrmap.T; gs : GenState) =
  VAR
    qmtn := Naming.MapIntfNameRW(map, RW.R) & "." & MainTypeName[TypeHier.Read];
  BEGIN
    EVAL gs.i3imports.insert("CompMemory");
    gs.put(Section.IMaintype,
           F("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine())));
    gs.put(Section.IMaintype, "TYPE\n");
    gs.put(Section.IMaintype, "  U = Update;\n");
    gs.put(Section.IMaintype, "  H <: PublicH;\n");
    gs.put(Section.IMaintype, "\n");
    gs.put(Section.IMaintype, "  PublicH = CompMemory.T OBJECT\n");
    gs.put(Section.IMaintype,
                            F("    read   : %s;\n",qmtn));
    gs.put(Section.IMaintype, "    update : U;\n");
    gs.put(Section.IMaintype, "    a      : A;\n");
    gs.put(Section.IMaintype, "  METHODS\n");
    gs.put(Section.IMaintype, "    init(base : CompAddr.T) : H;\n");
    gs.put(Section.IMaintype, "  END;\n");
    
    gs.put(Section.IMaintype, "\n");

    (**********************************************************************)
    
   gs.mdecl(
                       "  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
   gs.mdecl(
           "REVEAL\n" &
           "  H = PublicH BRANDED Brand & \"H\" OBJECT\n" &
           "  OVERRIDES\n" &
           "    init := InitH;\n" &
           "  END;\n" &
           "\n"                 
    );
   gs.mdecl(
           "TYPE\n" &
           "  Callback = CompMemoryListener.T OBJECT\n" &
           "    h : H;\n" &
           "  OVERRIDES\n" &
           "    callback := CallbackCallback;\n" &
           "    hash     := CallbackHash;\n" &
           "    equal    := CallbackEqual;\n" &
           "  END;\n" &
           "\n"                 
    );
   gs.mdecl(
          "PROCEDURE CallbackCallback(cb : Callback; op : CsrOp.T) =\n" &
          "  (* can only do writes since no VAR *)\n" & 
          "  BEGIN\n" &
          "    CsrAccess(cb.h.read, cb.h.a, op)\n" &
          "  END CallbackCallback;\n" &
          "\n" &

          "PROCEDURE CallbackHash(<*UNUSED*>cb : Callback) : Word.T =\n" &
          "  BEGIN\n" &
          "    RETURN 16_c0edbabe\n" &
          "  END CallbackHash;\n" &
          "\n" &

          "PROCEDURE CallbackEqual(cb : Callback; q : CompMemoryListener.T) : BOOLEAN =\n" &
          "  BEGIN\n" &
          "    RETURN cb = q\n" &
          "  END CallbackEqual;\n" &
          "\n" 
    );
    gs.mdecl(
           "PROCEDURE InitH(h : H; base : CompAddr.T) : H =\n" &
           "  VAR\n" &
           "    range : CompRange.T;\n"&
           "  BEGIN\n" &
         F("    range := Init(h.a, base, \"ROOT\");\n") &
           "    EVAL CompMemory.T.init(h, range);\n" &
           "    UpdateInit(h.update, h.a, h);\n" &                             
           "    h.registerListener(range,NEW(Callback, h := h));\n"&
           "    RETURN h\n" &                             
           "  END InitH;\n" &
           "\n"
    );
  END GenAddrmapGlobal;

PROCEDURE GenAddrmapCsr(map : RegAddrmap.T; gs : GenState) =
  (* generate interface for CSR write by address *)
  VAR
    qmtn := Naming.MapIntfNameRW(map, RW.R) & "." & MainTypeName[TypeHier.Read];
    ccnt : CARDINAL := 0;
  BEGIN
    gs.put(Section.IMaintype,
                       F("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine())));
    gs.put(Section.IMaintype,
                       F("PROCEDURE CsrAccess(VAR t : %s; READONLY a : A; VAR op : CsrOp.T);\n", qmtn));

    gs.mdecl(
                       "PROCEDURE CsrAccess(VAR t : %s; READONLY a : A; VAR op : CsrOp.T) =\n", qmtn);
    gs.mdecl(
                       "  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.mdecl("\n");
    gs.mdecl("  PROCEDURE DoChild(c : [0..NUMBER(a.tab)-2]) =\n");
    gs.mdecl("    BEGIN\n");
    gs.mdecl("      CASE c OF\n");
    FOR i := 0 TO map.children.size()-1 DO
      GenChildCsr(map.children.get(i), gs, ccnt, FALSE)
    END;
    gs.mdecl("      END\n");
    gs.mdecl("    END DoChild;\n");
    gs.mdecl("\n");

    MainBodyCsr(gs, "CsrAccess");
    
    gs.mdecl("\n");

    FOR i := 0 TO map.children.size()-1 DO
      GenCompProc(map.children.get(i).comp, gs, ProcType.Csr)
    END;
  END GenAddrmapCsr;

PROCEDURE GenAddrmapRanger(map : RegAddrmap.T; gs : GenState) =
  (* generate interface for CSR write by address *)
  BEGIN
    gs.imain("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.imain("PROCEDURE Range(READONLY a : A) : CompRange.T;\n");

    gs.mdecl("PROCEDURE Range(READONLY a : A) : CompRange.T =\n");
    gs.mdecl("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    gs.mdecl("\n");
    
    MainBodyRange(gs, "Range");
    
    gs.mdecl("\n");

    FOR i := 0 TO map.children.size()-1 DO
      GenCompProc(map.children.get(i).comp, gs, ProcType.Range)
    END
  END GenAddrmapRanger;
  
PROCEDURE MainBodyRange(gs : GenState; pnm : TEXT) =
  BEGIN
    gs.mdecl("  BEGIN\n");
    gs.mdecl("    RETURN CompRange.From2(a.min, a.max)\n");
    gs.mdecl("  END %s;\n",pnm);
  END MainBodyRange;

  (**********************************************************************)

PROCEDURE DoSimpleBinarySearch(gs : GenState) =
  BEGIN
    gs.mdecl("      WITH start = CompAddr.Find(a.tab,lo) DO\n");
    gs.mdecl("        FOR i := MAX(start,0) TO NUMBER(a.tab)-2 DO\n");
    gs.mdecl("          IF CompAddr.Compare(a.tab[i],hi) > -1 THEN EXIT END;\n");
    gs.mdecl("          DoChild(i)\n");
    gs.mdecl("        END\n");
    gs.mdecl("      END\n");
  END DoSimpleBinarySearch;

PROCEDURE DoIndirectBinarySearch(gs : GenState) =
  BEGIN
    gs.mdecl("      WITH start = CompAddr.FindIndirect(SUBARRAY(a.tab,0,NUMBER(a.monomap^)),a.monomap^,lo) DO\n");
    gs.mdecl("        FOR i := MAX(start,0) TO NUMBER(a.tab)-2 DO\n");
    gs.mdecl("          IF CompAddr.Compare(a.tab[a.monomap[i]],hi) > -1 THEN EXIT END;\n");
    gs.mdecl("          DoChild(a.monomap[i])\n");
    gs.mdecl("        END\n");
    gs.mdecl("      END\n");
  END DoIndirectBinarySearch;

TYPE ProcType = { Csr, Range };
     
PROCEDURE GenCompProc(c     : RegComponent.T;
                      gs    : GenState;
                      whch  : ProcType) =
  BEGIN
    CASE
      whch OF
      ProcType.Csr =>
      TYPECASE c OF
        RegAddrmap.T => (* skip, generated in its own file *)
      |
        RegRegfile.T => GenRegfileCsr(c, gs)
      |
        RegReg.T     => GenRegCsr    (c, gs)
      ELSE
        <*ASSERT FALSE*>
      END
    |
      ProcType.Range =>
      TYPECASE c OF
        RegAddrmap.T => (* skip, generated in its own file *)
      |
        RegRegfile.T => GenRegfileRanger(c, gs)
      |
        RegReg.T     => GenRegRanger    (c, gs)
      ELSE
        <*ASSERT FALSE*>
      END
    END
  END GenCompProc;

  (**********************************************************************)

PROCEDURE GenRegfileRanger(rf : RegRegfile.T; gs : GenState) =
  VAR
    pnm : TEXT;
  BEGIN
    IF NOT gs.defProc(rf, ProcType.Range, pnm) THEN RETURN END;

    gs.mdecl("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    IF rf.children.size() = 1 THEN
      (* just one member *)
      WITH chld = rf.children.get(0),
           nm   = M3Camel(chld.nm,debug := FALSE),
           rnm  = ComponentRangeName(chld.comp,gs) DO
        gs.mdecl("  BEGIN\n");
        IF chld.array = NIL THEN
          gs.mdecl("    RETURN %s(a.%s)\n",
                   rnm, nm
                   )
        ELSE
          gs.mdecl("    RETURN CompRange.From2(%s(a[0]).pos, CompRange.Lim(%s(a[%s-1])))\n",
                   rnm, rnm, BigInt.Format(chld.array.n.x))
        END;
        gs.mdecl("  END %s;\n",pnm)
      END
    ELSE
      MainBodyRange(gs,pnm);
   END;
   gs.mdecl("\n");
   FOR i := 0 TO rf.children.size()-1 DO
     GenCompProc(rf.children.get(i).comp, gs, ProcType.Range)
   END;
 END GenRegfileRanger;

PROCEDURE GenRegRanger(r : RegReg.T; gs : GenState) =
  VAR
    pnm := ComponentRangeName(r, gs);
  BEGIN
    IF NOT gs.defProc(r, ProcType.Range, pnm) THEN RETURN END;

    gs.mdecl("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    MainBodyRange(gs,pnm);

   gs.mdecl("\n");
 END GenRegRanger;

  (**********************************************************************)

PROCEDURE GenRegfileCsr(rf : RegRegfile.T; gs : GenState) =
  VAR
    pnm := ComponentCsrName(rf, gs);
    ttn := ComponentTypeNameInHier(rf, gs, TypeHier.Read);
    atn := ComponentTypeNameInHier(rf, gs, TypeHier.Addr);
    ccnt : CARDINAL := 0;
  BEGIN
    IF gs.dumpSyms.insert(pnm) THEN RETURN END;
    gs.mdecl( "PROCEDURE %s(VAR t : %s; READONLY a : %s; VAR op : CsrOp.T) =\n",
               pnm,
               ttn,
               atn);
    gs.mdecl("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    IF rf.children.size() = 1 THEN
       gs.mdecl("  BEGIN\n");
       GenChildCsr(rf.children.get(0), gs, ccnt, skipArc := TRUE);
       gs.mdecl("  END %s;\n",pnm);
    ELSE

      gs.mdecl("  PROCEDURE DoChild(c : [0..NUMBER(a.tab)-2]) =\n");
      gs.mdecl("    BEGIN\n");
      gs.mdecl("      CASE c OF\n");
      FOR i := 0 TO rf.children.size()-1 DO
        GenChildCsr(rf.children.get(i), gs, ccnt, skipArc := FALSE)
      END;
      gs.mdecl("      END\n");
      gs.mdecl("    END DoChild;\n");
      gs.mdecl("\n");
      MainBodyCsr(gs,pnm);
   END;
   gs.mdecl("\n");
   FOR i := 0 TO rf.children.size()-1 DO
     GenCompProc(rf.children.get(i).comp, gs, ProcType.Csr)
   END;
 END GenRegfileCsr;

PROCEDURE GenRegCsr(r  : RegReg.T;
                    gs : GenState) =
  VAR
    pnm := ComponentCsrName(r, gs);
    ttn := ComponentTypeNameInHier(r, gs, TypeHier.Read);
    atn := ComponentTypeNameInHier(r, gs, TypeHier.Addr);
  BEGIN
    IF gs.dumpSyms.insert(pnm) THEN RETURN END;
    gs.mdecl(
           "PROCEDURE %s(VAR t : %s; READONLY a : %s; VAR op : CsrOp.T) =\n",
           pnm, ttn, atn);
    gs.mdecl("  (* %s:%s *)\n",ThisFile(),Fmt.Int(ThisLine()));
    gs.mdecl("  PROCEDURE DoChild(c : [0..NUMBER(a.tab)-2]) =\n");
    gs.mdecl("    BEGIN\n");
    gs.mdecl("      CASE c OF\n");

    FOR i := 0 TO r.fields.size()-1 DO
      WITH f  = r.fields.get(i),
           nm = f.name(debug := FALSE) DO
        IF f.width <= BITSIZE(Word.T) THEN
          gs.mdecl("    | %s => t.%s := CsrOp.DoField(op, t.%s, a.%s);\n",
                                  Fmt.Int(i), nm, nm, nm)
        ELSE
          gs.mdecl("    | %s => CsrOp.DoWideField(op, t.%s, a.%s);\n",
                                  Fmt.Int(i), nm, nm)
        END;
      END
    END;
    gs.mdecl("      END\n");
    gs.mdecl("    END DoChild;\n");
    gs.mdecl("\n");
    MainBodyCsr(gs,pnm);
    gs.mdecl("\n");
  END GenRegCsr;

PROCEDURE MainBodyCsr(gs : GenState; pnm : TEXT) =
  BEGIN
    gs.mdecl("  VAR\n");
    gs.mdecl("    lo := CompAddr.T { op.at, op.fv };\n");
    gs.mdecl("    hi := op.hi;\n");
    gs.mdecl("  BEGIN\n");

    gs.mdecl("    IF a.min.word > hi.word THEN RETURN END;\n");
    gs.mdecl("    IF a.max.word < lo.word THEN RETURN END;\n");    
    gs.mdecl("    IF a.nonmono THEN\n");
    DoIndirectBinarySearch(gs);
    gs.mdecl("    ELSE\n");
    DoSimpleBinarySearch(gs);
    gs.mdecl("    END\n");
    gs.mdecl("  END %s;\n",pnm);
  END MainBodyCsr;

PROCEDURE GenChildCsr(e          : RegChild.T;
                      gs         : GenState;
                      VAR ccnt   : CARDINAL;
                      skipArc := FALSE) =
  CONST
    MaxFullIter = 4;
  VAR
    childArc : TEXT;
  BEGIN
    (* special case for array with only one child is that it is NOT
       a record *)
    IF skipArc THEN
      childArc := "";
    ELSE
      childArc := "." & M3Camel(e.nm,debug := FALSE);
    END;

    gs.mdecl("  (* %s:%s *)\n",ThisFile(),Fmt.Int(ThisLine()));
    IF skipArc THEN

      (* this is "the array special case" --

         in this case, the current node of the type tree is not a RECORD,
         but an ARRAY.

         therefore, we CANNOT store auxiliary information in it.
         
         therefore, we have to do a bit more work at runtime: 
         
         we evaluate the base of elements 0 and 1 in the array

         use that to compute the stride (in bytes)

         use the byte offset of the location we want to read or write into
         the array, DIV to find the array element of the base of the write.

         then continue reading/writing by scanning array elements in
         turn until we are past the operated-on region
      *)

      IF BigInt.ToInteger(e.array.n.x) > MaxFullIter THEN
        gs.mdecl("  (* %s:%s *)\n",ThisFile(),Fmt.Int(ThisLine()));
        WITH rnm = ComponentRangeName(e.comp,gs) DO
          gs.mdecl("    VAR\n");
          gs.mdecl("      r0 := %s(a[0]).pos;\n",rnm);
          gs.mdecl("      r1 := %s(a[1]).pos;\n",rnm);
          gs.mdecl("      opLo := CsrOp.LowAddr(op);\n");
          gs.mdecl("      offB : CARDINAL;\n");
          gs.mdecl("      start : CARDINAL;\n");
          gs.mdecl("      stride := CompAddr.DeltaBytes(r1,r0);\n");
          gs.mdecl("    BEGIN\n");
          gs.mdecl("      IF CompAddr.Compare(opLo,r0)<1 THEN\n");
          gs.mdecl("        start := 0\n");
          gs.mdecl("      ELSE\n");
          gs.mdecl("        offB := CompAddr.DeltaBytes(opLo,r0,truncOK := TRUE);\n");
          gs.mdecl("        start := offB DIV stride\n");
          gs.mdecl("      END;\n");
          gs.mdecl("      FOR i := start TO %s-1 DO\n", BigInt.Format(e.array.n.x));
          gs.mdecl("        IF CompAddr.Compare(%s(a[i]).pos,op.hi) >= 0 THEN RETURN END;\n", rnm);

          IF FALSE THEN
            gs.mdecl("      Debug.Out(Fmt.Int(i));\n");
            EVAL gs.m3imports.insert("Fmt");
          END;
          
          gs.mdecl("        %s(t[i],a[i],op)\n", ComponentCsrName(e.comp,gs));
          gs.mdecl("      END\n");
          gs.mdecl("    END\n")
        END
      ELSE
        gs.mdecl("  (* %s:%s *)\n",ThisFile(),Fmt.Int(ThisLine()));
        gs.mdecl("    %s\n",FmtArrFor(e.array));
        gs.mdecl("      %s(t[i],a[i],op)\n", ComponentCsrName(e.comp,gs));
        gs.mdecl("    END\n")
      END;
    ELSE
     gs.mdecl("  (* %s:%s *)\n",ThisFile(),Fmt.Int(ThisLine()));
     IF e.array = NIL THEN
        gs.mdecl(
               "      | %s => %s(t%s,a%s,op);\n",
                 Fmt.Int(ccnt),
                 ComponentCsrName(e.comp,gs),
                 childArc,
                 childArc )
      ELSE
        gs.mdecl(
               F("      | %s..%s =>  ",
                   Fmt.Int(ccnt),
                   Fmt.Int(ccnt+ArrayCnt(e.array)-1)) &
               F("%s(t%s[c-%s],a%s[c-%s],op);\n",
                   ComponentCsrName(e.comp,gs),
                   childArc,
                   Fmt.Int(ccnt),
                   childArc,
                   Fmt.Int(ccnt)));
      END;
      INC(ccnt,ArrayCnt(e.array))
    END
  END GenChildCsr;

  (**********************************************************************)

PROCEDURE GenRegfile(rf       : RegRegfile.T;
                     genState : RegGenState.T) 
  RAISES { Wr.Failure, Thread.Alerted, OSError.E } =
 (* dump a regfile type defn *)
  VAR
    ccnt : CARDINAL := 0;
    gs : GenState := genState;
  BEGIN
    IF gs.dumpSyms.insert(rf.typeName(gs)) THEN RETURN END;
    gs.put(Section.IComponents, F("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine())));
    gs.put(Section.IComponents, "TYPE\n");
    gs.put(Section.IComponents, F("  %s = ", rf.typeName(gs)));

    IF rf.children.size() # 1 THEN
      gs.put(Section.IComponents, F("RECORD (* %s:%s *)\n",ThisFile(),Fmt.Int(ThisLine())));
      FOR i := 0 TO rf.children.size()-1 DO
        WITH r = rf.children.get(i) DO
          gs.put(Section.IComponents, F("    %s : %s%s;\n",
                                        M3Camel(r.nm),
                                        FmtArr(r.array),
                                        ComponentTypeName(r.comp, gs)));
          INC(ccnt,ArrayCnt(r.array));
        END
      END;
      CASE gs.th OF
        TypeHier.Addr =>
        gs.put(Section.IComponents, F("    tab : ARRAY[0..%s+1-1] OF CompAddr.T;\n",
                                      Fmt.Int(ccnt)));
        gs.put(Section.IComponents, F("    mono := NEW(CompRange.Monotonic).init();\n"));
      |
        TypeHier.Update =>
        gs.put(Section.IComponents, F("    u : %s;\n",
                                      Updater(
                                          ComponentTypeNameInHier(rf,
                                                                  gs,
                                                                  TypeHier.Read)
        )
        ))
      |
        TypeHier.Read =>
      END;
      gs.put(Section.IComponents, "END;\n") 
    ELSE
      WITH r = rf.children.get(0) DO
        gs.put(Section.IComponents, F("%s%s;\n", FmtArr(r.array),
                                      ComponentTypeName(r.comp, gs)))
      END
    END;
    gs.put(Section.IComponents, "\n");
    FOR i := 0 TO rf.children.size()-1 DO
      rf.children.get(i).comp.generate(gs)
    END;

    CASE gs.th OF
      TypeHier.Addr =>
      GenRegfileInit(rf, gs)
    |
      TypeHier.Read =>  (* skip *)
    |
      TypeHier.Update => GenRegfileUpdateInit(rf, gs)
    END
  END GenRegfile;

 PROCEDURE GenRegfileInit(rf : RegRegfile.T; gs : GenState) =
  VAR
    iNm := ComponentInitName(rf, gs);
    skipArc := rf.children.size() = 1;
  BEGIN
    gs.mdecl(
           
             "PROCEDURE %s(VAR x : %s; at : CompAddr.T; path : CompPath.T) : CompRange.T =\n",
             iNm,
             rf.typeName(gs));
    gs.mdecl("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    
    gs.mdecl("  VAR\n");
    gs.mdecl("    base := at;\n");
    gs.mdecl("    mono := NEW(CompRange.Monotonic).init();\n");
    IF skipArc THEN
      gs.mdecl("  BEGIN\n");
    ELSE
      gs.mdecl("    c := 0;\n");
      gs.mdecl("  BEGIN\n");
      gs.mdecl("    x.tab[c] := at; INC(c);\n");
    END;

    FOR i := 0 TO rf.children.size()-1 DO
      (* special case:
         if RF has a single member, it is not a record, instead it
         is (a) an array of the child type (if an array)
         OR (b) a copy of the child type (if a scalar)
      *)
      GenChildInit(rf.children.get(i),
                   gs,
                   GetAddressingProp(rf),
                   skipArc := skipArc);
    END;
    IF NOT skipArc THEN BuildTab(gs, iNm) END;

    gs.mdecl("    RETURN CompRange.From2(base,at)\n");
    gs.mdecl("  END %s;\n",iNm);
    gs.mdecl("\n");
  END GenRegfileInit;

 PROCEDURE BuildTab(gs : GenState; iNm : TEXT) =
   BEGIN
     gs.mdecl("    <*ASSERT c = NUMBER(x.tab)*>\n");
     gs.mdecl("    x.nonmono := NOT mono.isok();\n");
     SetTabEnds(gs);
     gs.mdecl("    IF x.nonmono THEN\n");
     gs.mdecl("      x.monomap := mono.indexArr();\n");
     gs.mdecl("      Debug.Warning(\"Nonmono in %s\");\n",
                             iNm);
     gs.mdecl("    END;\n");
  END BuildTab;
   
 PROCEDURE SetTabEnds(gs : GenState) =
   (* update the tab so that min is least and max is most *)
   BEGIN
     gs.mdecl("    mono.setRange(x.min,x.max);\n")
  END SetTabEnds;
   
  (**********************************************************************)

PROCEDURE GenReg(r : RegReg.T;genState : RegGenState.T) =
  (* dump a reg type defn *)
  VAR
    gs : GenState := genState;
    ccnt := 0;
  BEGIN
    (* check if already dumped *)
    IF gs.dumpSyms.insert(r.typeName(gs)) THEN RETURN END;
    
    gs.put(Section.IComponents, F("TYPE\n"));
    gs.put(Section.IComponents, F("  %s = RECORD (* %s:%s *)\n", r.typeName(gs),ThisFile(),Fmt.Int(ThisLine())));
    FOR i := 0 TO r.fields.size()-1 DO
      WITH f = r.fields.get(i) DO
        IF f.width = BITSIZE(Word.T) THEN
          EVAL gs.i3imports.insert("Word")
        END;

        gs.put(Section.IComponents, F("    %s : %s;\n",
                                      f.name(), M3FieldType(f, gs.th, gs)));
        INC(ccnt)
      END
    END;
    CASE gs.th OF
      TypeHier.Addr =>
      gs.put(Section.IComponents, F("    tab : ARRAY[0..%s+1-1] OF CompAddr.T;\n",
                                  Fmt.Int(ccnt)));
      gs.put(Section.IComponents, F("    nonmono := FALSE;\n"));
      gs.put(Section.IComponents, F("    monomap : REF ARRAY OF CARDINAL;\n"));
      gs.put(Section.IComponents, F("    min, max: CompAddr.T;\n"));
   |
      TypeHier.Update =>
      gs.put(Section.IComponents, F("    u : %s;\n",
                                    Updater(
                                        ComponentTypeNameInHier(r,
                                                                gs,
                                                                TypeHier.Read)
                                           )
      ))
    |
      TypeHier.Read =>
    END;
    gs.put(Section.IComponents, F("  END;\n"));
    gs.put(Section.IComponents, F("\n"));
    CASE gs.th OF
      TypeHier.Addr =>  GenRegInit(r, gs)
    |
      TypeHier.Read =>  (* skip *)
    |
      TypeHier.Update => GenRegUpdateInit(r, gs)
    END
  END GenReg;

PROCEDURE GenRegInit(r : RegReg.T; gs : GenState) =
  VAR
    iNm := ComponentInitName(r, gs);
    props := GetPropTexts(r);
    haveUnspecLsb, haveSpecLsb := FALSE;
  BEGIN
    gs.mdecl(
           "PROCEDURE %s(VAR x : %s; at : CompAddr.T; path : CompPath.T) : CompRange.T =\n",
           iNm,
           r.typeName(gs));
    gs.mdecl("  (* %s:%s *)\n", ThisFile(), Fmt.Int(ThisLine()));
    
    gs.mdecl("  VAR\n");
    gs.mdecl("    base := at;\n");
    gs.mdecl("    range : CompRange.T;\n");
    gs.mdecl("    c := 0;\n");
    gs.mdecl("    mono := NEW(CompRange.Monotonic).init();\n");
    gs.mdecl("  BEGIN\n");
    gs.mdecl("    range := CompRange.PlaceReg(at%s);\n",
                            FormatPropArgs(props));
    gs.mdecl("    at  := range.pos;\n");
    gs.mdecl("    x.tab[c] := at; INC(c);\n");

    SortFieldsIfAllSpecified(r.fields);
    FOR i := 0 TO r.fields.size()-1 DO
      WITH f = r.fields.get(i) DO
        <*ASSERT f.width # RegField.Unspecified*>
        IF f.lsb = RegField.Unspecified THEN
          haveUnspecLsb := TRUE;
          gs.mdecl(
                 "    x.%s := CompRange.MakeField(at,%s);\n",
                   f.name(debug := FALSE),
                   Fmt.Int(f.width));
        ELSE
          haveSpecLsb := TRUE;
          gs.mdecl(
                  "    x.%s := CompRange.MakeField(CompAddr.PlusBits(range.pos,%s),%s);\n",
                   f.name(debug := FALSE),
                   Fmt.Int(f.lsb),
                   Fmt.Int(f.width));
        END;
        gs.mdecl(
                 "    at := mono.increase(at,x.%s);\n",
                   f.name(debug := FALSE));
        gs.mdecl(
               "    INC(CompAddr.initCount);\n");
      END;
      gs.mdecl("    x.tab[c] := at; INC(c);\n");
    END;

    BuildTab(gs, iNm);
    
    IF haveSpecLsb AND haveUnspecLsb THEN
      Debug.Error("Can't handle both specified and unspecified bit fields in a single register: " & r.typeName(gs))
    END;
    gs.mdecl("    CompPath.Debug(path,range);\n");
    gs.mdecl("    RETURN CompRange.From2(base,at)\n");
    gs.mdecl("  END %s;\n",iNm);
    gs.mdecl("\n");
  END GenRegInit;

PROCEDURE SortFieldsIfAllSpecified(seq : RegFieldSeq.T) =
  VAR
    arr : REF ARRAY OF RegField.T;
  BEGIN
    FOR i := 0 TO seq.size()-1 DO
      IF seq.get(i).lsb = RegField.Unspecified THEN
        RETURN (* can't sort *)
      END
    END;
    arr := NEW(REF ARRAY OF RegField.T, seq.size());
    FOR i := 0 TO seq.size()-1 DO
      arr[i] := seq.get(i)
    END;
    RegFieldArraySort.Sort(arr^);
    FOR i := 0 TO seq.size()-1 DO
      seq.put(i,arr[i])
    END
  END SortFieldsIfAllSpecified;

  (**********************************************************************)

PROCEDURE ComponentTypeNameInHier(c : RegComponent.T;
                                  gs : GenState;
                                  th : TypeHier) : TEXT =
  VAR
    gsC := NEW(GenState, init := InitGS).init(gs);
    prefix : TEXT;
  BEGIN
    gsC.th := th;
    IF gs.rw # TypePhase[th] THEN
      (* requesting a type from another module, need to qualify it *)
      prefix := Naming.MapIntfNameRW(gs.map, TypePhase[th]) & "."
    ELSE
      prefix := ""
    END;
    IF ISTYPE(c, RegAddrmap.T) THEN
      RETURN prefix & MainTypeName[th] (* this is bad *)
    ELSE
      RETURN prefix & c.typeName(gsC)
    END
  END ComponentTypeNameInHier;

  (**********************************************************************)

PROCEDURE ComponentTypeName(c : RegComponent.T; gs : GenState) : TEXT =
  BEGIN
    TYPECASE c OF
      RegAddrmap.T(a) =>
      RETURN a.intfName(gs) & "." & MainTypeName[gs.th]
    ELSE
      RETURN c.typeName(gs)
    END
  END ComponentTypeName;

PROCEDURE ComponentInitName(c : RegComponent.T; gs : GenState) : TEXT =
  BEGIN
    TYPECASE c OF
      RegAddrmap.T(a) =>
      RETURN a.intfName(gs) & "." & InitProcName[gs.th]
    ELSE
      RETURN "Init_" & c.typeName(gs)
    END
  END ComponentInitName;

PROCEDURE ComponentCsrName(c : RegComponent.T; gs : GenState) : TEXT =
  VAR
    gsC := NEW(GenState, init := InitGS).init(gs);
  BEGIN
    gsC.th := TypeHier.Read;
    TYPECASE c OF
      RegAddrmap.T(a) =>
      RETURN a.intfName(gs) & ".CsrAccess" 
    ELSE
      RETURN "Csr__" & c.typeName(gsC)
    END
  END ComponentCsrName;
  
PROCEDURE ComponentRangeName(c : RegComponent.T; gs : GenState) : TEXT =
  VAR
    gsC := NEW(GenState, init := InitGS).init(gs);
  BEGIN
    gsC.th := TypeHier.Read;
    TYPECASE c OF
      RegAddrmap.T(a) =>
      RETURN a.intfName(gs) & ".Range" 
    ELSE
      RETURN "Range__" & c.typeName(gsC)
    END
  END ComponentRangeName;

CONST ComponentName = ARRAY ProcType OF PROCEDURE(c : RegComponent.T; gs : GenState) : TEXT { ComponentCsrName, ComponentRangeName };

  (**********************************************************************)
  
PROCEDURE M3FieldType(f : RegField.T; th : TypeHier; gs : GenState) : TEXT =
  BEGIN
    RETURN M3FieldWidthType(f.width, th, gs, f.nm)
  END M3FieldType;

PROCEDURE M3FieldWidthType(c : CARDINAL; th : TypeHier; gs : GenState; name := "") : TEXT =

  PROCEDURE M3Type() : TEXT =
    BEGIN
      CASE c OF 
        WordSize =>  RETURN "Word.T"
      |
        1..WordSize-1 => RETURN F("[0..16_%s]",
                                  Fmt.Int(Word.LeftShift(1, c)-1, base := 16))
      ELSE
        IF c > WordSize THEN
          Debug.Warning(F("%s : register widths of %s > %s not natively supported in Modula-3 on this machine", name, Fmt.Int(c), Fmt.Int(WordSize)));
          RETURN
            F("ARRAY [0..%s-1] OF [0..1]", Fmt.Int(c))
        ELSE
          (* only 0 in this category *)
          Debug.Error(F("Field width %s not supported : %s", Fmt.Int(c), name))
      END;
        <*ASSERT FALSE*>
      END
    END M3Type;

  CONST
    WordSize = BITSIZE(Word.T);
    (* normally BITSIZE(Word.T) = 64 *)
  BEGIN
    CASE th OF
      TypeHier.Addr =>
      EVAL gs.m3imports.insert("CompRange");
      RETURN "CompRange.T"
      (* all fields are addresses in the write mode *)
    |
      TypeHier.Read => RETURN M3Type()
    |
      TypeHier.Update =>
      EVAL gs.fieldWidths.insert(c);
      RETURN "UObj"&Fmt.Int(c)
    END      
  END M3FieldWidthType;
  
PROCEDURE Updater(ofType : TEXT) : TEXT =
  BEGIN
    RETURN F("OBJECT METHODS u(READONLY x : %s) END", ofType)
  END Updater;
  
BEGIN END RegModula3.
