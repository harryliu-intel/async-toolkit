MODULE GenViewsSvHlp;
IMPORT RegReg, RegField, RegContainer, RegChild, RegAddrmap, RegRegfile;
IMPORT Pathname;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT BigInt;
IMPORT FieldData, Rd, Pickle2;
IMPORT Wx;
IMPORT TextUtils;
IMPORT RegComponent;
IMPORT Thread;
FROM RegProperty IMPORT Unquote;
IMPORT CardSeq;
IMPORT BigIntSeq;
IMPORT Word;
IMPORT RdlPredefProperty;
IMPORT RefSeq;
IMPORT RdlArray;
IMPORT RegComponentRefTbl;

<*FATAL Thread.Alerted*>
<*FATAL BigInt.OutOfRange*>

CONST TE = Text.Equal;

REVEAL
  T = Public BRANDED OBJECT
    wx : Wx.T;
  METHODS
    put(txt : TEXT; lev : CARDINAL) := Put;
  OVERRIDES
    gen := Gen;
  END;

PROCEDURE Put(t : T; txt : TEXT; lev : CARDINAL) =
  BEGIN
    Wx.PutText(t.wx, Spaces(lev));
    Wx.PutText(t.wx, txt);
    Wx.PutText(t.wx, "\n");
  END Put;

PROCEDURE Spaces(lev : CARDINAL) : TEXT =
  VAR
    a := NEW(REF ARRAY OF CHAR, lev*2);
  BEGIN
    FOR i := FIRST(a^) TO LAST(a^) DO
      a[i] := ' '
    END;
    RETURN Text.FromChars(a^)
  END Spaces;

TYPE
  Arc = OBJECT
    up  : Arc;
  END;

  ArrayArc = Arc OBJECT
    sz : CARDINAL;
  END;

  NameArc = Arc OBJECT
    idx : CARDINAL;
    nm  : TEXT;
    field := FALSE;
  END;

PROCEDURE Gen(t : T; tgtmap : RegAddrmap.T; outDir : Pathname.T) =
  VAR
    a : REF ARRAY OF FieldData.T := NIL;
    b : REF ARRAY OF CARDINAL := NIL;
  BEGIN
    t.wx := Wx.New();
    IF t.fieldAddrRd # NIL THEN
      Debug.Out("Reading field data...");
      a := Pickle2.Read(t.fieldAddrRd);
      Debug.Out("size of a : " & Int(NUMBER(a^)));
      b := Pickle2.Read(t.fieldAddrRd);
      Debug.Out("size of b : " & Int(NUMBER(b^)));
      Rd.Close(t.fieldAddrRd)
    END;
    t.put(F("((cont %s)",tgtmap.nm), 0);

    EVAL ToType(tgtmap);            (* size calcs etc *)

    DoContainer(t, tgtmap, 1, NIL); (* emit code *)
    t.put(")",0);
    (*
    WITH txt = Wx.ToText(t.wx) DO
      Debug.Out("Producing\n"&txt);
    END;
    *)
  END Gen;

PROCEDURE DoContainer(t   : T;
                      c   : RegContainer.T;
                      lev : CARDINAL;
                      pfx : Arc) =
  VAR
    skipArc := c.skipArc();
  BEGIN
    <*ASSERT c # NIL*>
    EmitComment(t, "Container", pfx, lev);

    VAR tn : TEXT; BEGIN
      TYPECASE c OF
        RegAddrmap.T => tn := "addrmap"
      |
        RegRegfile.T => tn := "regfile"
      ELSE
        <*ASSERT FALSE*>
      END;
    END;
    FOR i := 0 TO c.children.size()-1 DO
      VAR chld := c.children.get(i);
          arc : Arc := pfx;
      BEGIN        
        IF NOT skipArc THEN
          arc := NEW(NameArc, idx := i, nm := HlpName(chld.comp, chld.nm),
                     up := arc)
        END;
        IF chld.array # NIL THEN
          arc := NEW(ArrayArc, sz := BigInt.ToInteger(chld.array.n.x),
                     up := arc)
        END;

        DoChild(t, chld, lev, arc, skipArc)
      END
    END;
    EmitComment(t, "Container", pfx, lev, TRUE);
    Emit(t,"",lev);
  END DoContainer;

PROCEDURE HasNoFurtherArcs(c : RegComponent.T) : BOOLEAN =
  BEGIN
    TYPECASE c OF
      RegReg.T  =>
      RETURN TRUE (* it's a register, so we have done the last arc *)
    |
      RegContainer.T(container) =>
      (* recursively go down and ensure every level has only one child,
         and the recursion bottoms in a register *)
      RETURN
        container.children.size()=1 AND
        HasNoFurtherArcs(container.children.get(0).comp)
    ELSE
      <*ASSERT FALSE*>
    END    
  END HasNoFurtherArcs;
  
PROCEDURE DoChild(t       : T;
                  c       : RegChild.T;
                  lev     : CARDINAL;
                  pfx     : Arc;
                  skipArc : BOOLEAN) =
  VAR
    tag : TEXT;
  BEGIN
    (* this is very tricky and a bit inconsistent, but it comes from
       the syntax of C-like languages ... :

       if the thing we are processing is a possibly multi-dimensional register,
       that is, an array of an array of.....of an array of registers,
       we need to report the type as "reg".  It is only ever reported
       as a container if it has any non-skip-arcs left. *)

    <*ASSERT NOT ISTYPE(c.comp, RegField.T)*>
    IF HasNoFurtherArcs(c.comp) THEN
      tag := "cont" (*"reg"*)
    ELSE
      tag := "cont" (*"cont"*)
    END;
    
    IF NOT skipArc THEN
      t.put(F("((%s %s)", tag, c.nm),lev);
      INC(lev)
    END;
    
    IF c.array # NIL THEN
      VAR extras := ""; BEGIN
        TYPECASE c.comp OF
          RegReg.T(reg) => extras := F(" (regheader %s)",reg.nm)
        ELSE
          (* skip *)
        END;
        t.put(F("((array %s%s)",
                Int(BigInt.ToInteger(c.array.n.x)),
                extras),
              lev);
        INC(lev)
      END
    END;
      
    WITH ccomp = c.comp DO
      TYPECASE ccomp OF
        RegContainer.T => DoContainer(t, ccomp, lev, pfx)
      |
        RegReg.T       => DoReg(t, ccomp, lev, pfx)
      |
        RegField.T     => <*ASSERT FALSE*> (* right? *)
      ELSE
        <*ASSERT FALSE*>
      END
    END;

    IF c.array # NIL THEN
      DEC(lev);
      t.put(F(")"),lev)
    END;
    IF NOT skipArc THEN
      DEC(lev);
      t.put(F(")"),lev)
    END
  END DoChild;

PROCEDURE EmitComment(t : T;
                      node : TEXT;
                      pfx : Arc;
                      lev : CARDINAL;
                      end := FALSE) =
  VAR
    endS := "";
  BEGIN
    IF end THEN endS := "END " END;
    
    Emit(t, F("  // %s%s %-60s", endS, node, FormatNameArcsOnly(pfx)), lev);
    IF NOT end AND NOT TE(node, "Field") THEN
      Emit(t, F("  // arr %s", FormatArrayArcsOnly(pfx)), lev)
    END
  END EmitComment;
  
PROCEDURE DoField(t : T; f : RegField.T; lev : CARDINAL; pfx : Arc) =
  BEGIN
    EmitComment(t, "Field", pfx, lev);

    IF f.lsb = RegField.Unspecified THEN
      Debug.Warning("Unspecified LSB in field " & f.nm)
    END;
    IF f.width = RegField.Unspecified THEN
      Debug.Warning("Unspecified width in field " & f.nm)
    END;
    t.put(F("((field %s %s %s))", f.nm, Int(f.lsb), Int(f.width)),lev);

    EmitLocalParam(t, FormatNameArcsOnly(pfx, "W_"), f.width, -1, lev); 
    IF f.width = 1 THEN
      EmitLocalParam(t, FormatNameArcsOnly(pfx, "B_"), f.lsb, -1, lev) 
    ELSE
      EmitLocalParam(t, FormatNameArcsOnly(pfx, "L_"), f.lsb, -1, lev); 
      EmitLocalParam(t, FormatNameArcsOnly(pfx, "H_"), f.lsb+f.width-1, -1, lev) 
    END;
  END DoField;

PROCEDURE Emit(t : T; str : TEXT; lev : CARDINAL) =
  BEGIN
    Debug.Out(str);
    t.put(str, lev);
  END Emit;
  
PROCEDURE EmitLocalParam(t       : T;
                         nm      : TEXT;
                         val     : INTEGER;
                         hexBits : [-1..LAST(CARDINAL)];
                         lev     : CARDINAL) =
  VAR
    valStr : TEXT;
  BEGIN
    IF hexBits = -1 THEN
      valStr := Int(val)
    ELSE
      <*ASSERT val >= 0*>
      <*ASSERT val < Word.Shift(1,hexBits)*>
      valStr := F("%s'h%s", Int(hexBits), Int(val, base := 16))
    END;
    WITH str = F(LocalParamFmt, nm, valStr) DO
      Emit(t, str, lev)
    END
  END EmitLocalParam;

CONST LocalParamFmt = "  localparam %-55s = %s;";

PROCEDURE EmitBigLocalParam(t       : T;
                         nm      : TEXT;
                         val     : BigInt.T;
                         hexBits : [-1..LAST(CARDINAL)];
                         lev     : CARDINAL) =
  VAR
    valStr : TEXT;
  BEGIN
    IF hexBits = -1 THEN
      valStr := BigInt.Format(val)
    ELSE
      <*ASSERT BigInt.Compare(val,BigInt.Zero) >= 0 *>
      <*ASSERT BigInt.Compare(val,BigPow2(hexBits)) < 1 *>
      valStr := F("%s'h%s", Int(hexBits), BigInt.Format(val, base := 16))
    END;
    WITH str = F(LocalParamFmt, nm, valStr) DO
      Emit(t, str, lev)
    END
  END EmitBigLocalParam;
  
PROCEDURE DoReg(t : T; r : RegReg.T; lev : CARDINAL; pfx : Arc) =
  VAR
    atomic : INTEGER;
    width  : INTEGER;
    svName := FormatNameArcsOnly(pfx);
    lim := 0;
    rst := BigInt.Zero;
    ref : REFANY;
  BEGIN
    EmitComment(t, "Reg", pfx, lev);

    WITH hadIt = r.getRdlPredefIntProperty(RdlPredefProperty.T.accesswidth,
                                           atomic) DO
      <*ASSERT hadIt*>
    END;
    WITH hadIt = r.getRdlPredefIntProperty(RdlPredefProperty.T.regwidth,
                                           width) DO
      <*ASSERT hadIt*>
    END;

    <*ASSERT atomic MOD 8 = 0*>
    <*ASSERT width  MOD 8 = 0*>
    EmitLocalParam(t, svName & "_ATOMIC_WIDTH", width DIV 8, -1, lev);

    WITH haveTree = compTypeTbl.get(r, ref) DO
      IF haveTree THEN
        Debug.Out(FmtType(ref))
      END
    END;

    FOR i := 0 TO r.fields.size()-1 DO
      WITH f = r.fields.get(i) DO
        lim := MAX(lim, f.lsb + f.width);
        IF f.defVal # NIL THEN
          rst := BigInt.Add(rst,
                             BigInt.Mul(f.defVal.x,BigPow2(f.lsb)))
        END
      END
    END;
    EmitLocalParam(t, svName & "_BITS", lim, -1, lev);
    EmitBigLocalParam(t, svName & "_DEFAULT", rst, lim, lev);
   
    VAR
      arraySizes := ArraySizes(pfx);
    BEGIN
      CASE arraySizes.size() OF
        0 =>
      |
        1 =>
          EmitLocalParam(t,
                         svName & "_ENTRIES",
                         arraySizes.get(0),
                         -1,
                         lev)
      ELSE
        FOR i := 0 TO arraySizes.size()-1 DO
          EmitLocalParam(t,
                         svName & "_ENTRIES_" & Int(i),
                         arraySizes.get(i),
                         -1,
                         lev)
        END
      END
    END;
    
    FOR i := 0 TO r.fields.size()-1 DO
      WITH f = r.fields.get(i),
           arc = NEW(NameArc,
                     idx := i,
                     nm := HlpName(f, f.nm),
                     up := pfx,
                     field := TRUE) DO
        DoField(t, f, lev, arc)
      END
    END;
    EmitComment(t, "Reg", pfx, lev, TRUE);
    Emit(t,"",lev);
  END DoReg;

PROCEDURE HlpName(comp : RegComponent.T; iNm : TEXT) : TEXT =
  VAR
    hn : TEXT;
    gotIt := comp.getRdlTextProperty("HlpName", hn);
  BEGIN
    IF gotIt THEN
      hn := Unquote(hn);
      hn := TextUtils.Replace(hn, "$", iNm)
    END;
    IF hn = NIL OR TE(hn, "") THEN
      hn := iNm
    END;
    RETURN hn
  END HlpName;

PROCEDURE FormatNameArcsOnly(p : Arc; fieldPfx := "") : TEXT =
  VAR
    res := "";
  BEGIN
    WHILE p # NIL DO
      TYPECASE p OF
        NameArc(q) =>
        IF Text.Length(res) > 0 THEN res := "_" & res END;
        res := q.nm & res;
        IF q.field THEN
          res := "_" & fieldPfx & res
        END;
        (*Debug.Out("NameArc q.nm=" & q.nm & " res="& res );*)
        IF Text.Length(res) > 0 AND  Text.GetChar(res, 0) = '/' THEN
          res := Text.Sub(res,1);
          (*Debug.Out("NameArc return " & res);*)
          RETURN res
        END
      ELSE
        (* skip *)
      END;
      p := p.up;
    END;
    RETURN res
  END FormatNameArcsOnly;

PROCEDURE FormatArrayArcsOnly(p : Arc) : TEXT =
  VAR
    res := "";
  BEGIN
    WHILE p # NIL DO
      TYPECASE p OF
        ArrayArc(q) => res := F("[%s]",Int(q.sz)) & res;
      ELSE
        (* skip *)
      END;
      p := p.up
    END;
    RETURN res
  END FormatArrayArcsOnly;

PROCEDURE ArraySizes(p : Arc) : CardSeq.T =
  VAR
    res := NEW(CardSeq.T).init();
  BEGIN
    WHILE p # NIL DO
      TYPECASE p OF
        ArrayArc(q) => res.addhi(q.sz)
      ELSE
        (* skip *)
      END;
      p := p.up
    END;
    RETURN res
  END ArraySizes;

VAR bigPow2 := NEW(BigIntSeq.T).init();
  
PROCEDURE BigPow2(n : CARDINAL) : BigInt.T =
  BEGIN
    WHILE n >= bigPow2.size() DO
      bigPow2.addhi(BigInt.Mul(bigPow2.get(bigPow2.size()-1),BigInt.Two))
    END;
    RETURN bigPow2.get(n)
  END BigPow2;

TYPE
  Type = OBJECT
    sz : CARDINAL;
    comp : RegComponent.T;
  END;

  Array = Type OBJECT
    n    : CARDINAL;
    elem : Type;
  END;

  Struct = Type OBJECT
    fields : RefSeq.T;
  END;

PROCEDURE ToType(c : RegComponent.T) : Type =
  BEGIN
    Debug.Out("ToType " & c.nm);
    TYPECASE c OF
      RegContainer.T => RETURN ContainerType(c)
    |
      RegReg.T       => RETURN RegType(c)
    |
      RegField.T     => <*ASSERT FALSE*> (* right? *)
    ELSE
      <*ASSERT FALSE*>
    END
  END ToType;

VAR compTypeTbl := NEW(RegComponentRefTbl.Default).init();
    
PROCEDURE ContainerType(c : RegContainer.T) : Type =
  VAR
    skipArc := c.skipArc();
  BEGIN
    IF skipArc THEN
      <*ASSERT c.children.size() = 1*>
      VAR
        chld := c.children.get(0);
        down := ChildType(chld);
        n := BigInt.ToInteger(NARROW(chld.array,RdlArray.Single).n.x);
      BEGIN
        WITH arr = NEW(Array,
                       comp := c,
                       n    := n,
                       sz   := n * down.sz,
                       elem := down) DO
          EVAL compTypeTbl.put(c, arr);
          RETURN arr
        END
      END
    ELSE
      VAR
        seq := NEW(RefSeq.T).init();
        sz := 0;
      BEGIN
        FOR i := 0 TO c.children.size()-1 DO
          VAR
            chld := c.children.get(i);
            ct := ChildType(chld);
          BEGIN
            IF chld.array # NIL THEN
              WITH n = BigInt.ToInteger(
                           NARROW(chld.array,RdlArray.Single).n.x) DO
                ct := NEW(Array,
                          comp := chld.comp,
                          n  := n,
                          sz :=  n * ct.sz,
                          elem := ct)
              END;
              EVAL compTypeTbl.put(chld.comp, ct)
            END;
            seq.addhi(ct);
            INC(sz, ct.sz);
          END
        END(*FOR*);

        WITH res = NEW(Struct, sz := sz, comp := c, fields := seq) DO
          EVAL compTypeTbl.put(c, res);
          RETURN res
        END
      END
    END;
  END ContainerType;

PROCEDURE ChildType(c : RegChild.T) : Type =
  BEGIN
    <*ASSERT NOT ISTYPE(c.comp, RegField.T)*>
    RETURN ToType(c.comp)
  END ChildType;

PROCEDURE RegType(c : RegReg.T) : Type =
  VAR
    seq := NEW(RefSeq.T).init();
  BEGIN
    FOR i := 0 TO c.fields.size()-1 DO
      seq.addhi(NEW(Type, sz := 1, comp := c.fields.get(i)))
    END;
    
    WITH res = NEW(Struct,
                   sz      := c.fields.size(),
                   comp    := c,
                   fields  := seq) DO
      EVAL compTypeTbl.put(c, res);
      RETURN res
    END
  END RegType;

PROCEDURE FmtType(type : Type) : TEXT =
  BEGIN
    TYPECASE type OF
      Array(a) =>
      RETURN F("Array sz %s", Int(a.sz))
    |
      Struct(s) =>
      RETURN F("Struct sz %s fields %s", Int(s.sz), Int(s.fields.size()))
    ELSE
      RETURN F("Unknown sz %s", Int(type.sz))
    END
  END FmtType;
  
BEGIN
  bigPow2.addhi(BigInt.One) (* 2^0 = 1 *)
END GenViewsSvHlp.
