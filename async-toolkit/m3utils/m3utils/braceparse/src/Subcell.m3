MODULE Subcell;

PROCEDURE EncodeName(longNames     : LongNames;
                     READONLY name : ARRAY OF CHAR;
                     VAR      tgt  : InstanceName) =
  BEGIN
    IF NUMBER(name) < NUMBER(tgt) THEN
      SUBARRAY(tgt, 0, NUMBER(name)) := name;
      tgt[NUMBER(name)] := VAL(0, CHAR);
      tgt[LAST(tgt)] := VAL(NUMBER(name), CHAR)
    ELSE
      VAR
        idx := longNames.size();
      BEGIN
        tgt[LAST(tgt)] := LAST(CHAR);
        FOR i := FIRST(tgt) TO LAST(tgt) - 1 DO
          tgt[i] := VAL(idx MOD NUMBER(CHAR),CHAR);
          idx := idx DIV NUMBER(CHAR)
        END;
        <*ASSERT idx = 0*>
        FOR i := FIRST(name) TO LAST(name) DO
          longNames.addhi(name[i])
        END;
        longNames.addhi(VAL(0,CHAR))
      END
    END
  END EncodeName;

PROCEDURE DecodeName(longNames     : LongNames;
                     READONLY inst : InstanceName;
                     VAR buffer    : ARRAY OF CHAR) =
  VAR
    last := ORD(inst[LAST(inst)]);
  BEGIN
    IF last < LAST(inst) THEN
      SUBARRAY(buffer, 0, last) := SUBARRAY(inst, 0, last)
    ELSE
      VAR
        idx : CARDINAL := 0;
        c : CHAR;
        j := 0;
      BEGIN
        FOR i := FIRST(inst) TO LAST(inst) - 1 DO
          idx := NUMBER(CHAR) * idx + ORD(inst[i])
        END;
        REPEAT
          c := longNames.get(idx);
          buffer[j] := c;
          INC(j); INC(idx);
        UNTIL c = VAL(0,CHAR)
      END
    END
  END DecodeName;

BEGIN END Subcell.
