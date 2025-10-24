MODULE LibertyComponent;

IMPORT LibertyComponentChildren;

IMPORT LibertyComponentSeq;
IMPORT LibertyComponentSeqBuilder;
IMPORT Random;
IMPORT Word;
IMPORT TextWr;
IMPORT Wr;
IMPORT RTName;
IMPORT Text;

REVEAL
  T = LibertyComponentChildren.Private BRANDED Brand OBJECT
    id : REF CARDINAL := NIL;
    fertile := TRUE; (*cleared by DefaultChildren*)
  OVERRIDES
    getId           := DefaultId;
    children        := DefaultChildren;
    makeParentLinks := MakeParentLinks;
    getParent       := GetParent;
    canHaveChildren := CanHaveChildren;
    format          := Format;
    debugDump       := DebugDump;
  END;

VAR rand := NEW(Random.Default).init();
VAR mu := NEW(MUTEX);

PROCEDURE Format(t : T) : TEXT =
  VAR
    wr := TextWr.New();
  BEGIN
    t.write(wr);
    RETURN TextWr.ToText(wr)
  END Format;
  
PROCEDURE DefaultId(t : T) : CARDINAL =
  BEGIN
    IF t.id = NIL THEN
      LOCK mu DO
        t.id := NEW(REF CARDINAL);
        t.id^ := rand.integer(FIRST(CARDINAL), LAST(CARDINAL))
      END
    END;
    RETURN t.id^
  END DefaultId;

PROCEDURE Hash(t : T) : Word.T =
  BEGIN RETURN t.getId() END Hash;

PROCEDURE DefaultChildren(t : T) : LibertyComponentSeq.T =
  BEGIN
    t.fertile := FALSE;
    RETURN LibertyComponentSeqBuilder.BuildSeq()
  END DefaultChildren;

PROCEDURE CanHaveChildren(t : T) : BOOLEAN =
  BEGIN
    EVAL t.children();
    RETURN t.fertile
  END CanHaveChildren;

PROCEDURE MakeParentLinks(t : T) =
  BEGIN
    WITH children = t.children() DO
      FOR i := 0 TO children.size() - 1 DO
        WITH ch = children.get(i) DO
          ch.makeParentLinks();
          ch.parent := t
        END
      END
    END
  END MakeParentLinks;

PROCEDURE GetParent(t : T) : T =
  BEGIN RETURN t.parent END GetParent;

PROCEDURE DebugDump(t : T; truncate : CARDINAL) : TEXT =
  VAR
    wr := TextWr.New();
  BEGIN
    DebugDumpPrint(wr, t, 0, truncate);
    RETURN TextWr.ToText(wr)
  END DebugDump;

PROCEDURE DebugDumpPrint(wr : Wr.T; t : T; indent, truncate : CARDINAL) =
  BEGIN
    FOR i := 0 TO indent - 1 DO
      Wr.PutChar(wr, ' ')
    END;
    Wr.PutText(wr, RTName.GetByTC(TYPECODE(t)));
    Wr.PutText(wr, " : ");

    IF truncate = LAST(CARDINAL) THEN
      t.write(wr)
    ELSE
      WITH tempWr = TextWr.New() DO
        t.write(tempWr);
        WITH str = TextWr.ToText(tempWr),
             len = Text.Length(str) DO
          IF len > truncate THEN
            Wr.PutText(wr, Text.Sub(str, 0, truncate - 13));
            Wr.PutText(wr, "...");
            Wr.PutText(wr, Text.Sub(str, len - 10, 10))
          ELSE
            Wr.PutText(wr, str)
          END
        END
      END
    END;

    Wr.PutChar(wr, '\n');
    WITH children = t.children() DO
      FOR i := 0 TO children.size() - 1 DO
        DebugDumpPrint(wr, children.get(i), indent + 4, truncate)
      END
    END
  END DebugDumpPrint;
  
BEGIN END LibertyComponent.
