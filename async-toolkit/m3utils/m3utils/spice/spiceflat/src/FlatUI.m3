MODULE FlatUI;
IMPORT TextSpiceInstanceSetTbl, TextTextSetTbl, TextTextTbl;
IMPORT ReadLine, Fmt, TextReader, Debug, Text;
FROM Fmt IMPORT F;
IMPORT TextUtils, TextSet;
IMPORT SpiceInstance, SpiceInstanceSet;
IMPORT SpiceObject;

CONST TE = Text.Equal;
      
PROCEDURE REPL(assocs   : TextSpiceInstanceSetTbl.T;
               symTab   : TextTextSetTbl.T;
               canonTbl : TextTextTbl.T) =
  VAR
    cnt := 0;
  BEGIN
    WITH t = NEW(ReadLine.Default).init() DO
      t.startProc();
      t.display("Hello there.\n");
      LOOP
        t.setPrompt(Fmt.Int(cnt) & " > ");
        WITH line   = t.readLine(),
             reader = NEW(TextReader.T).init(line) DO
          Debug.Out("Got line " & line);
          
          IF Text.Length(line) # 0 THEN
            WITH cmd = reader.nextE(" ") DO
              IF    TE(cmd, "quit") THEN t.quit(); EXIT
              ELSIF TE(cmd, "explain") THEN
                WITH what = reader.nextE(" ", skipNulls := TRUE) DO
                  Explain(t, assocs, symTab, canonTbl, what)
                END
              ELSIF TE(cmd, "assoc") THEN
                WITH what = reader.nextE(" ", skipNulls := TRUE) DO
                  Assoc(t, assocs, symTab, canonTbl, what)
                END
              END
            END
          END
        END;

        INC(cnt)
      END
    END
  END REPL;

PROCEDURE Canonicalize(nm : TEXT;
                       VAR canon : TEXT;
                       canonTbl : TextTextTbl.T) : BOOLEAN =
  BEGIN
    (* ground is special case... *)
    IF TextUtils.HaveSuffix(nm, ".vss") THEN canon := "vss"; RETURN TRUE END;

    IF canonTbl.get(nm, canon) THEN
      RETURN TRUE
    ELSE
      (* unknown node, its own best friend *)
      canon := nm;
      RETURN FALSE
    END
  END Canonicalize;

PROCEDURE Explain(t        : ReadLine.T;
                  assocs   : TextSpiceInstanceSetTbl.T;
                  symTab   : TextTextSetTbl.T;
                  canonTbl : TextTextTbl.T;
                  what     : TEXT) =
  VAR
    canon, alias : TEXT;
    s : TextSet.T;
  BEGIN
    IF Canonicalize(what, canon, canonTbl) THEN
      t.display(F("NODE %s CANON %s\n", what, canon));
      IF symTab.get(what, s) THEN
        WITH iter = s.iterate() DO
          WHILE iter.next(alias) DO
            t.display(F("ALIAS %s\n", alias))
          END
        END
      END
    ELSE
      t.display(F("?\n"))
    END
  END Explain;

PROCEDURE TypeName(obj : SpiceObject.T) : TEXT =
  BEGIN
    TYPECASE obj OF
      SpiceObject.R => RETURN "RES"
    |
      SpiceObject.C => RETURN "CAP"
    |
      SpiceObject.X => RETURN "SUB"
    |
      SpiceObject.M => RETURN "FET"
    ELSE
      RETURN "???"
    END
  END TypeName;
  
PROCEDURE Assoc(t        : ReadLine.T;
                assocs   : TextSpiceInstanceSetTbl.T;
                symTab   : TextTextSetTbl.T;
                canonTbl : TextTextTbl.T;
                what     : TEXT) =

  PROCEDURE DisplayInstance(inst : SpiceInstance.T) =

    PROCEDURE Terminal(lab : TEXT; nm : TEXT; opt : TEXT) =
      VAR
        tfn : TEXT;
        tCanon : TEXT;
        sfx := "";
      BEGIN
        IF inst.parent = NIL THEN
          tfn := nm
        ELSE
          tfn := inst.parent.flatName & "." & nm;
        END;
        EVAL Canonicalize(tfn, tCanon, canonTbl);
        IF TE(tCanon, canon) THEN sfx := "*THIS* " & opt END;
        t.display(F("%s %s %s\n", lab, tCanon, sfx));
      END Terminal;

    PROCEDURE Value(lab, c : TEXT) =
      BEGIN
        t.display(F("%s %s\n", lab, c))
      END Value;
      
    BEGIN
      t.display(F("%s %s\n", TypeName(inst.obj), inst.flatName));

      TYPECASE inst.obj OF
        SpiceObject.M(m) =>
        Terminal("  SD ", m.terminals.get(0), "DRIVER");
        Terminal("  GT ", m.terminals.get(1), "FANOUT");
        Terminal("  SD ", m.terminals.get(2), "DRIVER");
        Terminal("  SUB", m.terminals.get(3), "SUBSTRATE");
        Value   ("  TYP", m.data.get(0));
        Value   ("     ", m.data.get(1));
        Value   ("     ", m.data.get(2));
      |
        SpiceObject.R, SpiceObject.C =>
      ELSE
      END
    END DisplayInstance;
    
  VAR
    canon : TEXT;
    s : SpiceInstanceSet.T;
    inst : SpiceInstance.T;
  BEGIN
    EVAL Canonicalize(what, canon, canonTbl);
    IF assocs.get(canon, s) THEN
      WITH iter = s.iterate() DO
        WHILE iter.next(inst) DO
          DisplayInstance(inst)
        END
      END
    ELSE
      t.display(F("?\n"))
    END;
  END Assoc;

BEGIN END FlatUI.
