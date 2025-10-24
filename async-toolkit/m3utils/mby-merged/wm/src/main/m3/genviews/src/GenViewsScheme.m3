(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

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
IMPORT RegComponent;

REVEAL
  T = Public BRANDED OBJECT
  OVERRIDES
    gen := Gen;
  END;

PROCEDURE Put(wx : Wx.T; txt : TEXT; lev : CARDINAL) =
  BEGIN
    Wx.PutText(wx, Spaces(lev));
    Wx.PutText(wx, txt);
    Wx.PutText(wx, "\n");
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

TYPE Version = { Tree, Sync };
  
PROCEDURE Gen(t : T; tgtmap : RegAddrmap.T; outDir : Pathname.T) =
  VAR
    a       : REF ARRAY OF FieldData.T := NIL;
    b       : REF ARRAY OF CARDINAL := NIL;
    treeTxt : TEXT;
  BEGIN
    IF t.fieldAddrRd # NIL THEN
      Debug.Out("Reading field data...");
      a := Pickle2.Read(t.fieldAddrRd);
      Debug.Out("size of a : " & Int(NUMBER(a^)));
      b := Pickle2.Read(t.fieldAddrRd);
      Debug.Out("size of b : " & Int(NUMBER(b^)));
      Rd.Close(t.fieldAddrRd)
    END;

    WITH wx = Wx.New() DO
      Put(wx,F("((cont %s)",tgtmap.nm), 0);
      DoContainer(wx, tgtmap, 1, Version.Tree);
      Put(wx,")",0);
      treeTxt := Wx.ToText(wx)
    END;

    Debug.Out("Producing\n"&treeTxt);
    RunScheme(t, a, b, treeTxt);

  END Gen;

PROCEDURE RunScheme(t    : T;
                    a    : REF ARRAY OF FieldData.T;
                    b    : REF ARRAY OF CARDINAL;
                    prog : TEXT) =
  VAR
    env := NEW(SchemeNavigatorEnvironment.T).initEmpty();
    m : SchemeObject.T;
  BEGIN
    SchemeStubs.RegisterStubs();

    WITH progRd    = NEW(TextRd.T).init(prog),
         schemeIn  = NEW(SchemeInputPort.T).init(progRd) DO
      m := schemeIn.read()
    END;
    
    EVAL env.define(Atom.FromText("the-addresses"), a);
    EVAL env.define(Atom.FromText("the-map"),       m);
    EVAL env.define(Atom.FromText("the-tree"),      b);
    
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
  
PROCEDURE DoContainer(wx  : Wx.T;
                      c   : RegContainer.T;
                      lev : CARDINAL;
                      v   : Version) =
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
      DoChild(wx, c.children.get(i), lev (*+1*), skipArc, v)
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
  
PROCEDURE DoChild(wx : Wx.T;
                  c : RegChild.T;
                  lev : CARDINAL;
                  skipArc : BOOLEAN;
                  v : Version) =
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
      Put(wx,F("((%s %s)", tag, c.nm),lev);
      INC(lev)
    END;
    
    IF c.array # NIL THEN
      VAR extras := ""; BEGIN
        TYPECASE c.comp OF
          RegReg.T(reg) => extras := F(" (regheader %s)",reg.nm)
        ELSE
          (* skip *)
        END;
        Put(wx,F("((array %s%s)",
                Int(BigInt.ToInteger(c.array.n.x)),
                extras),
              lev);
        INC(lev)
      END
    END;
      
    WITH ccomp = c.comp DO
      TYPECASE ccomp OF
        RegContainer.T => DoContainer(wx, ccomp, lev, v)
      |
        RegReg.T => DoReg(wx, ccomp, lev, v)
      |
        RegField.T => <*ASSERT FALSE*> (* right? *)
      ELSE
        <*ASSERT FALSE*>
      END
    END;

    IF c.array # NIL THEN
      DEC(lev);
      Put(wx,F(")"),lev)
    END;
    IF NOT skipArc THEN
      DEC(lev);
      Put(wx,F(")"),lev)
    END
  END DoChild;

PROCEDURE DoField(wx : Wx.T; f : RegField.T; lev : CARDINAL; v : Version) =
  BEGIN
    IF f.lsb = RegField.Unspecified THEN
      Debug.Warning("Unspecified LSB in field " & f.nm)
    END;
    IF f.width = RegField.Unspecified THEN
      Debug.Warning("Unspecified width in field " & f.nm)
    END;
    Put(wx,F("((field %s %s %s))", f.nm, Int(f.lsb), Int(f.width)),lev)
  END DoField;

PROCEDURE DoReg(wx : Wx.T; r : RegReg.T; lev : CARDINAL; v : Version) =
  BEGIN
    FOR i := 0 TO r.fields.size()-1 DO
      DoField(wx, r.fields.get(i), lev(*+1*), v)
    END;
  END DoReg;

BEGIN END GenViewsScheme.
