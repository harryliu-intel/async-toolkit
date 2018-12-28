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
    DoContainer(t, tgtmap, 1, NIL);
    t.put(")",0);
    WITH txt = Wx.ToText(t.wx) DO
      Debug.Out("Producing\n"&txt);
    END;
  END Gen;

PROCEDURE DoContainer(t   : T;
                      c   : RegContainer.T;
                      lev : CARDINAL;
                      pfx : Arc) =
  VAR
    skipArc := c.skipArc();
  BEGIN
    <*ASSERT c # NIL*>
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
          arc : Arc;
      BEGIN        
        IF chld.array # NIL THEN
          arc := NEW(ArrayArc, sz := BigInt.ToInteger(chld.array.n.x))
        ELSE
          arc := NEW(NameArc, idx := i, nm := HlpName(pfx, chld))
        END;
        arc.up := pfx;
        DoChild(t, chld, lev, pfx, skipArc)
      END
    END;
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
        RegReg.T => DoReg(t, ccomp, lev, pfx)
      |
        RegField.T => <*ASSERT FALSE*> (* right? *)
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

PROCEDURE DoField(t : T; f : RegField.T; lev : CARDINAL; pfx : Arc) =
  BEGIN
    IF f.lsb = RegField.Unspecified THEN
      Debug.Warning("Unspecified LSB in field " & f.nm)
    END;
    IF f.width = RegField.Unspecified THEN
      Debug.Warning("Unspecified width in field " & f.nm)
    END;
    t.put(F("((field %s %s %s))", f.nm, Int(f.lsb), Int(f.width)),lev)
  END DoField;

PROCEDURE DoReg(t : T; r : RegReg.T; lev : CARDINAL; pfx : Arc) =
  BEGIN
    FOR i := 0 TO r.fields.size()-1 DO
      DoField(t, r.fields.get(i), lev, pfx)
    END;
  END DoReg;

PROCEDURE HlpName(pfx : Arc; chld : RegChild.T) : TEXT =
  VAR
    hn := chld.comp.getRdlTextProperty("HlpName");
    xsep := "";
  BEGIN
    IF hn = NIL OR TE(hn, "") THEN
      hn := chld.nm
    ELSIF Text.GetChar(hn, 1) = '/' THEN
      pfx := NIL;
      hn := Text.Sub(hn,1)
    END;
    hn := TextUtils.Replace(hn, "$", chld.nm);
    IF ISTYPE(chld.comp, RegField.T) THEN
      xsep := "_"
    END;
    hn := FormatNameArcsOnly(pfx) & xsep & hn;
    RETURN hn
  END HlpName;

PROCEDURE FormatNameArcsOnly(p : Arc) : TEXT =
  VAR
    res := "";
  BEGIN
    WHILE p # NIL DO
      TYPECASE p OF
        NameArc(q) => res := q.nm & "_" & res
      ELSE
        (* skip *)
      END;
      p := p.up
    END;
    RETURN res
  END FormatNameArcsOnly;
  
BEGIN END GenViewsSvHlp.
