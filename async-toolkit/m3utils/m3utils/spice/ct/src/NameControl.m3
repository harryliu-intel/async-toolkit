MODULE NameControl;
IMPORT TextSeq;
IMPORT TextSet;
IMPORT RegExList;
IMPORT CardSeq;
IMPORT RegEx;
IMPORT Text;

CONST TE = Text.Equal;

PROCEDURE MakeIdxMap(names         : TextSeq.T;
                     restrictNodes : TextSet.T;
                     regExList     : RegExList.T) : CardSeq.T =
  VAR
    res := NEW(CardSeq.T).init();
    c := 0;
    success : BOOLEAN;
  BEGIN
    FOR i := 0 TO names.size() - 1 DO
      IF TE(names.get(i), "TIME") THEN
        success := TRUE
      ELSIF restrictNodes = NIL AND regExList = NIL THEN
        success := TRUE
      ELSIF restrictNodes # NIL AND restrictNodes.member(names.get(i)) THEN
        success := TRUE
      ELSE
        success := FALSE;
        VAR
          p := regExList;
        BEGIN
          WHILE p # NIL DO
            IF RegEx.Execute(p.head,
                             names.get(i)) # -1 THEN
              success := TRUE;
              EXIT
            END;
            p := p.tail
          END
        END
      END;

      IF success THEN
        res.addhi(c);
        INC(c)
      ELSE
        res.addhi(NoMapping)
      END
    END;

    RETURN res
  END MakeIdxMap;
  
PROCEDURE SanitizeNames(idxMap : CardSeq.T;
                        names  : TextSeq.T) =
  VAR
    store := NEW(TextSeq.T).init();
  BEGIN
    FOR i := 0 TO names.size() - 1 DO
      IF idxMap.get(i) # NoMapping THEN store.addhi(names.get(i)) END
    END;

    EVAL names.init();

    FOR i := 0 TO store.size() - 1 DO
      names.addhi(store.get(i))
    END
  END SanitizeNames;

BEGIN END NameControl.
