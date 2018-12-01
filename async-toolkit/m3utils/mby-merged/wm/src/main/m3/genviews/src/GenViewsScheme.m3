MODULE GenViewsScheme;
IMPORT GenViews;
IMPORT RegReg, RegField, RegContainer, RegChild, RegAddrmap, RegRegfile;
IMPORT Pathname;
IMPORT Debug;
FROM Fmt IMPORT F, Int;
IMPORT Text;
IMPORT BigInt;
IMPORT FieldData, Rd, Pickle2;
IMPORT Wx;
IMPORT Scheme, SchemeStubs, SchemeNavigatorEnvironment;
IMPORT IP, ReadLineError;
IMPORT NetObj;
IMPORT Atom, AL;
IMPORT SchemeM3;
FROM SchemeReadLine IMPORT MainLoop;
IMPORT ReadLine;
IMPORT TextRd, SchemeObject, SchemeInputPort;

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
  
PROCEDURE Gen(t : T; tgtmap : RegAddrmap.T; outDir : Pathname.T) =
  VAR
    a : REF ARRAY OF FieldData.T := NIL;
  BEGIN
    t.wx := Wx.New();
    IF t.fieldAddrRd # NIL THEN
      Debug.Out("Reading field data...");
      a := Pickle2.Read(t.fieldAddrRd);
      Rd.Close(t.fieldAddrRd)
    END;
    DoContainer(t, tgtmap, 0);
    WITH txt = Wx.ToText(t.wx) DO
      Debug.Out("Producing\n"&txt);
      RunScheme(t, a, txt);
    END;
  END Gen;

PROCEDURE RunScheme(t : T; a : REF ARRAY OF FieldData.T; prog : TEXT) =
  VAR
    env := NEW(SchemeNavigatorEnvironment.T).initEmpty();
    m : SchemeObject.T;
  BEGIN
    SchemeStubs.RegisterStubs();

    WITH progRd    = NEW(TextRd.T).init(prog),
         schemeIn  = NEW(SchemeInputPort.T).init(progRd) DO
      m := schemeIn.read()
    END;
    
    EVAL env.define(Atom.FromText("the-addresses"),a);
    EVAL env.define(Atom.FromText("the-map"),      m);
    
    WITH arr = NEW(REF ARRAY OF Pathname.T, NUMBER(t.scmFiles^)+1) DO
      arr[0] := "require";
      FOR i := 1 TO LAST(arr^) DO arr[i] := t.scmFiles[i-1] END;
      TRY
        WITH scm = NEW(SchemeM3.T).init(arr^, globalEnv := env) DO
                                            

          IF TRUE THEN
            MainLoop(NEW(ReadLine.Default).init(), scm)
          ELSE
            Debug.Out("No REPL, exiting..");
          END
        END
      EXCEPT
        Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
      |
        IP.Error(err) => Debug.Error("Caught IP.Error : " & AL.Format(err))
      |
        ReadLineError.E(err) => 
        Debug.Error("Caught ReadLineError.E : " & AL.Format(err))
      |
        NetObj.Error(err) => Debug.Error("Caught NetObj.Error : " & 
          AL.Format(err))
      END
    END
  END RunScheme;

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
