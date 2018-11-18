MODULE GenViewsScheme;
IMPORT GenViews;
IMPORT RegReg, RegField, RegContainer, RegChild, RegAddrmap, RegRegfile;
IMPORT Pathname;
IMPORT IO;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT BigInt;

REVEAL
  T = GenViews.T BRANDED OBJECT
  METHODS
    put(txt : TEXT; lev : CARDINAL) := Put;
  OVERRIDES
    gen := Gen;
  END;

PROCEDURE Put(<*UNUSED*>t : T; txt : TEXT; lev : CARDINAL) =
  BEGIN
    IO.Put(Spaces(lev));
    IO.Put(txt);
    IO.Put("\n");
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
  
PROCEDURE Gen(t : T; tgtmap : RegAddrmap.T; outDir : Pathname.T) =
  BEGIN
    DoContainer(t, tgtmap, 0)
  END Gen;

PROCEDURE DoContainer(t : T; c : RegContainer.T; lev : CARDINAL) =
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
      t.put(F("(container %s %s",tn, c.nm), lev)
    END;
    FOR i := 0 TO c.children.size()-1 DO
      DoChild(t, c.children.get(i), lev+1)
    END;
    t.put(")", lev)
  END DoContainer;

PROCEDURE DoChild(t : T; c : RegChild.T; lev : CARDINAL) =
  BEGIN
    t.put(F("(%s", c.nm),lev);
    INC(lev);
    IF c.array # NIL THEN
      t.put(F("(array %s ", Int(BigInt.ToInteger(c.array.n.x))),lev);
      INC(lev)
    END;
      
    WITH ccomp = c.comp DO
      TYPECASE ccomp OF
        RegContainer.T => DoContainer(t, ccomp, lev)
      |
        RegReg.T => DoReg(t, ccomp, lev)
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
    DEC(lev);
    t.put(F(")"),lev)
  END DoChild;

PROCEDURE DoField(t : T; f : RegField.T; lev : CARDINAL) =
  BEGIN
    IF f.lsb = RegField.Unspecified THEN
      Debug.Warning("Unspecified LSB in field " & f.nm)
    END;
    IF f.width = RegField.Unspecified THEN
      Debug.Warning("Unspecified width in field " & f.nm)
    END;
    t.put(F("(field %s %s %s)", f.nm, Int(f.lsb), Int(f.width)),lev)
  END DoField;

PROCEDURE DoReg(t : T; r : RegReg.T; lev : CARDINAL) =
  BEGIN
    t.put("(reg " & r.nm, lev);
    FOR i := 0 TO r.fields.size()-1 DO
      DoField(t, r.fields.get(i), lev+1)
    END;
    t.put(")", lev)
  END DoReg;

BEGIN END GenViewsScheme.
