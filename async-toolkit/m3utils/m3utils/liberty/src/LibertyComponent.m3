MODULE LibertyComponent;

IMPORT LibertyComponentChildren;

IMPORT LibertyComponentSeq;
IMPORT LibertyComponentSeqBuilder;
IMPORT Random;
IMPORT Word;

REVEAL
  T = LibertyComponentChildren.Private BRANDED Brand OBJECT
    id : REF CARDINAL := NIL;
  OVERRIDES
    getId           := DefaultId;
    children        := DefaultChildren;
    makeParentLinks := MakeParentLinks;
    getParent       := GetParent;
  END;

VAR rand := NEW(Random.Default).init();
VAR mu := NEW(MUTEX);
    
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

PROCEDURE DefaultChildren(<*UNUSED*>t : T) : LibertyComponentSeq.T =
  BEGIN
    RETURN LibertyComponentSeqBuilder.BuildSeq()
  END DefaultChildren;

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

BEGIN END LibertyComponent.
