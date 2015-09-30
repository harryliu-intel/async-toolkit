MODULE Dims;
IMPORT Word;
FROM Fmt IMPORT Int;
IMPORT Debug;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T =
  VAR res : Word.T := 0;
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO res := Word.Plus(res, a[i]) END;
    RETURN res
  END Hash;

PROCEDURE Clone(READONLY a : T) : REF T =
  BEGIN 
    WITH res = NEW(REF T, NUMBER(a)) DO
      FOR i := FIRST(res^) TO LAST(res^) DO res[i] := 0 END;
      RETURN res
    END
  END Clone;

REVEAL
  Iterator = PubIterator BRANDED OBJECT
    lim : REF T;
    new : BOOLEAN;
  OVERRIDES
    init := InitI;
    next := NextI;
  END;

PROCEDURE Iterate(READONLY lim : T) : Iterator =
  BEGIN RETURN NEW(Iterator).init(lim) END Iterate;

PROCEDURE InitI(i : Iterator; READONLY lim : T) : Iterator =
  BEGIN 
    i.lim := Clone(lim); 
    i.lim^ := lim; 
    i.new := TRUE;
    RETURN i 
  END InitI;

PROCEDURE NextI(i : Iterator; VAR nxt : T) : BOOLEAN =
  BEGIN
    IF i.new THEN
      i.new := FALSE;
      FOR j := FIRST(nxt) TO LAST(nxt) DO
        nxt[j] := 0
      END;
      Debug.Out("Dims.NextI: new, returning " & Format(nxt));
      RETURN TRUE
    ELSE
      VAR
        done := TRUE;
      BEGIN
        FOR j := LAST(nxt) TO FIRST(nxt) BY -1 DO
          IF nxt[j] = i.lim[j]-1 THEN
            nxt[j] := 0
          ELSE
            nxt[j] := nxt[j]+1;
            done := FALSE; EXIT
          END
        END;
        Debug.Out("Dims.NextI: not new, returning " & Format(nxt));
        RETURN NOT done
      END
    END
  END NextI;

PROCEDURE Format(READONLY z : T) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := FIRST(z) TO LAST(z) DO
      res := res & "[" & Int(z[i]) & "]"
    END;
    RETURN res
  END Format;


BEGIN END Dims.

