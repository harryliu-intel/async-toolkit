MODULE Main;
IMPORT rdlLexExt;
IMPORT rdlParseExt;
IMPORT Stdio;
IMPORT ParseParams;
IMPORT Params;
IMPORT Debug; 
FROM Fmt IMPORT F;
IMPORT TextSetDef;
IMPORT RTName;
IMPORT RdlComponentDef, RdlComponentDefClass;
IMPORT RdlComponentDefType, RdlComponentDefElem;
IMPORT RdlArray, BigInt;
IMPORT RdlComponentInstElemList;
IMPORT RegField, RegFieldSeq;
IMPORT RegAddrmap;
IMPORT RegReg;
IMPORT RdlComponentDefElemList;
IMPORT RegRegfile;
IMPORT Fmt;
IMPORT RegModula3 AS Tgt; 
IMPORT RegModula3Naming AS TgtNaming;
IMPORT RegModula3Generators AS TgtGenerators;
IMPORT RegChild, RegChildSeq;
IMPORT Text;
IMPORT RegComponent;
IMPORT OSError, Wr, AL;
IMPORT Thread;
IMPORT Pathname;
IMPORT FS;

<*FATAL Thread.Alerted*>

CONST Usage = "-top <top map name>";

PROCEDURE DoUsage() : TEXT =
  BEGIN RETURN Params.Get(0) & ": usage: " & Usage END DoUsage;

PROCEDURE AllocFields(c : RdlComponentDef.T) : RegFieldSeq.T =
  VAR
    seq := NEW(RegFieldSeq.T).init();
    p : RdlComponentInstElemList.T;
  BEGIN
    <*ASSERT c.id # NIL*> (* now ANONYMOUS-smth...? *)
    <*ASSERT c.anonInstElems # NIL*>
    p := c.anonInstElems.list;
    WHILE p # NIL DO
      WITH i = p.head,
           f = NEW(RegField.T, name := TgtNaming.FieldName) DO
        f.nm := i.id;
        f.defVal := i.eq;
        IF i.array = NIL THEN
          f.width := 1
        ELSE
          TRY
            TYPECASE i.array OF
              RdlArray.Single(sing) =>
              f.width := BigInt.ToInteger(sing.n.x)
            |
              RdlArray.Range(rang) =>
              WITH hi = BigInt.Max(rang.to.x,rang.frm.x),
                   lo = BigInt.Min(rang.to.x,rang.frm.x) DO
                f.width := BigInt.ToInteger(hi) - BigInt.ToInteger(lo) + 1;
                f.lsb := BigInt.ToInteger(lo)
              END
            ELSE
              <*ASSERT FALSE*>
            END
          EXCEPT
            BigInt.OutOfRange => Debug.Error("Array is too big!")
          END
        END;
        seq.addhi(f)
      END;
      p := p.tail
    END;
    RETURN seq
  END AllocFields;
  
PROCEDURE AllocAddrmap(c         : RdlComponentDef.T) : RegAddrmap.T =
  VAR
    props := c.list.propTab;
    defs  := c.list.defTab;
    am := NEW(RegAddrmap.T,
              props    := props,
              intfName := TgtNaming.MapIntfName,
              typeName := TgtNaming.MapTypename,
              generate := TgtGenerators.GenAddrmap,
              children := NEW(RegChildSeq.T).init());
    p : RdlComponentDefElemList.T := c.list.lst;
  BEGIN
    <*ASSERT c.id # NIL*>
    am.nm := c.id;
    <*ASSERT c.anonInstElems = NIL*> (* no immediate instances *)
    WHILE p # NIL DO
      WITH cd = p.head DO
        TYPECASE cd OF
          RdlComponentDefElem.ComponentInst(ci) =>
          <*ASSERT ci.componentInst.alias = NIL*>
          <*ASSERT ci.componentInst.id # NIL*>
          VAR
            q := ci.componentInst.list;
            z : REFANY;
            def := defs.lookup(ci.componentInst.id);
          BEGIN
            IF def = NIL THEN
              Debug.Error("Couldnt find in defs : " & ci.componentInst.id)
            END;
            IF NOT ISTYPE(def, DecoratedComponentDef) THEN
              def := Decorate(def, defs.getPath(ci.componentInst.id,
                                                TgtNaming.PathSep));
              defs.update(ci.componentInst.id, def)
            END;

            z := NARROW(def,DecoratedComponentDef).comp;
              
            WHILE q # NIL DO
              WITH elem = q.head,
                   ne = NEW(RegChild.T,
                            comp  := z,
                            nm    := elem.id,
                            array := elem.array,
                            at    := elem.at) DO
                IF elem.inc # NIL THEN
                  ne.stride := elem.inc
                ELSE
                  ne.stride := NIL
                END;
                
                am.children.addhi(ne)
              END;
              q := q.tail
            END
          END
        ELSE
          (*skip*)
        END
      END;
      p := p.tail
    END;
    IF Text.Equal(c.id, tgtmapNm) THEN tgtmap := am END;
    RETURN am
  END AllocAddrmap;

TYPE
  DecoratedComponentDef = RdlComponentDef.T OBJECT
    comp : RegComponent.T;
  METHODS
    init(old : RdlComponentDef.T; comp : RegComponent.T) : DecoratedComponentDef := InitDCD;
  END;

PROCEDURE Decorate(def       : RdlComponentDef.T;
                   path      : TEXT) : DecoratedComponentDef =
  VAR
    comp : RegComponent.T;
  BEGIN
    CASE def.type OF
      RdlComponentDefType.T.addrmap =>
      comp := AllocAddrmap(def)
    |
      RdlComponentDefType.T.regfile =>
      comp := AllocRegfile(def)
    |
      RdlComponentDefType.T.reg =>
      comp := AllocReg(def)
    |
      RdlComponentDefType.T.field =>
      comp := NIL
    |
      RdlComponentDefType.T.signal =>
      comp := NIL
    END;
    IF comp # NIL THEN
      comp.path := path
    END;
    RETURN NEW(DecoratedComponentDef).init(def, comp)
  END Decorate;

PROCEDURE InitDCD(dcd : DecoratedComponentDef; old : RdlComponentDef.T; comp : RegComponent.T) : DecoratedComponentDef =
  BEGIN
    dcd.type := old.type;
    dcd.id   := old.id;
    dcd.list := old.list;
    dcd.anonInstElems := old.anonInstElems;

    dcd.comp := comp;

    RETURN dcd
  END InitDCD;
  
PROCEDURE AllocRegfile(c         : RdlComponentDef.T) : RegRegfile.T =
  VAR
    props := c.list.propTab;
    defs  := c.list.defTab;
    regf := NEW(RegRegfile.T,
                nm       := c.id,
                props    := props,
                typeName := TgtNaming.RegfileTypename,
                generate := TgtGenerators.GenRegfile,
                children := NEW(RegChildSeq.T).init());
    p : RdlComponentDefElemList.T := c.list.lst;
  BEGIN
    <*ASSERT c.anonInstElems = NIL*> (* no immediate instances *)
    WHILE p # NIL DO
      WITH cd = p.head DO
        TYPECASE cd OF
          RdlComponentDefElem.ComponentInst(ci) =>
          <*ASSERT ci.componentInst.alias = NIL*>
          <*ASSERT ci.componentInst.id # NIL*>
          VAR
            q := ci.componentInst.list;
            z : REFANY;
            def := defs.lookup(ci.componentInst.id);
          BEGIN
            IF def = NIL THEN
              Debug.Error(F("Couldnt find in defs : %s",ci.componentInst.id),FALSE);
              defs.dump();
              Debug.Error("QUIT")
            END;
            IF NOT ISTYPE(def, DecoratedComponentDef) THEN
              def := Decorate(def, defs.getPath(ci.componentInst.id,
                                                TgtNaming.PathSep));
              defs.update(ci.componentInst.id, def)
            END;
            z := NARROW(def,DecoratedComponentDef).comp;
            WHILE q # NIL DO
              WITH elem = q.head,
                   regDef = NEW(RegChild.T,
                                comp   := z,
                                nm     := elem.id,
                                array  := elem.array,
                                at     := elem.at,
                                stride := elem.inc,
                                mod    := elem.mod) DO
                <*ASSERT elem.eq = NIL*>
                regf.children.addhi(regDef)
              END;
              q := q.tail
            END
          END
         ELSE
           (*skip*)
         END
      END;
      p := p.tail
    END;
    RETURN regf
  END AllocRegfile;

PROCEDURE AllocReg(c     : RdlComponentDef.T) : RegReg.T =
  VAR
    props := c.list.propTab;
    reg := NEW(RegReg.T,
               props    := props,
               generate := TgtGenerators.GenReg,
               typeName := TgtNaming.RegTypename);
    fields := NEW(RegFieldSeq.T).init();
    p : RdlComponentDefElemList.T := c.list.lst;
  BEGIN
    <*ASSERT c.anonInstElems = NIL*> (* no immediate instances *)
    reg.nm := c.id;
    WHILE p # NIL DO
      WITH cd = p.head DO
        TYPECASE cd OF
          RdlComponentDefElem.ComponentDef(cd) =>
          IF cd.componentDef.type # RdlComponentDefType.T.field THEN
            Debug.Error("Unexpected component in reg " & c.id & " : " &
              RdlComponentDefType.Names[cd.componentDef.type])
          END;
          fields := RegFieldSeq.Cat(fields, AllocFields(cd.componentDef))
        |
          RdlComponentDefElem.PropertyAssign =>
        |
          RdlComponentDefElem.ComponentInst(ci) =>
          WITH comp = c.list.defTab.lookup(ci.componentInst.id) DO
            IF comp.type # RdlComponentDefType.T.field THEN
              Debug.Error("object of type RdlComponentDefElem.ComponentInst : "&
                ci.componentInst.id & " / " & RdlComponentDefType.Names[comp.type] )
            END
          END
        ELSE
          Debug.Error("object of type " & RTName.GetByTC(TYPECODE(cd)) &
            " unexpected in reg")
        END
      END;
      p := p.tail
    END;
    reg.fields := fields;
    RETURN reg
  END AllocReg;

PROCEDURE P() =
  PROCEDURE U(z : BOOLEAN) =
    BEGIN p := z END U;
  VAR
    p := TRUE;
  BEGIN
    Q(p, U)
  END P;

PROCEDURE Q(READONLY x : BOOLEAN; S : PROCEDURE(z : BOOLEAN)) =
  BEGIN
    Debug.Out("x=" & Fmt.Bool(x));
    <*ASSERT x*>
    S(NOT x);
    <*ASSERT NOT x*>
    Debug.Out("x=" & Fmt.Bool(x)); (* see Green Book 2.3.2 *)
  END Q;
  
VAR
  lexer  := NEW(rdlLexExt.T, userDefProperties := NEW(TextSetDef.T).init());
  parser := NEW(rdlParseExt.T, lexer := lexer);
  rd     := Stdio.stdin;
  tgtmap : RegAddrmap.T := NIL;
  tgtmapNm : TEXT;
  outDir : Pathname.T := "build/src";
BEGIN
  (* command-line args: *)
  TRY
    WITH pp = NEW(ParseParams.T).init(Stdio.stderr) DO
      IF pp.keywordPresent("-top") THEN
        tgtmapNm := pp.getNext()
      END;
      IF pp.keywordPresent("-o") THEN
        outDir := pp.getNext()
      END;
      
      pp.skipParsed();
      pp.finish()
    END;
    IF tgtmapNm = NIL THEN RAISE ParseParams.Error END
  EXCEPT
    ParseParams.Error => Debug.Error("Command-line params wrong:\n" & DoUsage())
  END;

  EVAL lexer.setRd(rd);
  (* set lexer input *)
  
  EVAL parser.setLex(lexer).parse();
  (* set parser lexer and call the parse method *)

  Debug.Out("Done parsing.");

  WITH rdlTgt = parser.defSymtab.lookup(tgtmapNm) DO
    IF rdlTgt = NIL THEN
      Debug.Error("Top symbol not found")
    END;
    tgtmap := Decorate(rdlTgt, "").comp
  END;

  VAR
    rm3 := NEW(Tgt.T).init(tgtmap);
  BEGIN
    FOR i := FIRST(Tgt.Phase) TO LAST(Tgt.Phase) DO
      TRY

        TRY
          <*UNUSED*>VAR dummy := FS.Iterate(outDir); BEGIN END
        EXCEPT
          OSError.E(x) => Debug.Error(F("Problem opening directory \"%s\" : OSError.E : %s", outDir, AL.Format(x)))
        END;
        
        rm3.write(outDir, i)
      EXCEPT
        OSError.E(x) =>
        Debug.Error("Error in " &
          Tgt.PhaseNames[i] & " code generation : OSError.E : " & AL.Format(x))
      |
        Wr.Failure(x) =>
        Debug.Error("Error in " &
          Tgt.PhaseNames[i] & " code generation : Wr.Failure : " & AL.Format(x))
      END
    END
  END;

  P()

END Main.
