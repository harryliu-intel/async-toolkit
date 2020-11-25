MODULE Subcell;
IMPORT Random, Wr, Rd, FileRd, FileWr;
IMPORT FS, TextList;
IMPORT Compiler, Fmt, Process;

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
        idx : CARDINAL;
      BEGIN
        IF longNames.rd # NIL THEN
          Rd.Close(longNames.rd);
          longNames.rd := NIL
        END;
        IF longNames.wr = NIL THEN
          longNames.wr := FileWr.OpenAppend(longNames.nm);
          Wr.Seek(longNames.wr, LAST(CARDINAL))
        END;

        idx := Wr.Index(longNames.wr);
        
        tgt[LAST(tgt)] := LAST(CHAR);
        FOR i := FIRST(tgt) TO LAST(tgt) - 1 DO
          tgt[i] := VAL(idx MOD NUMBER(CHAR),CHAR);
          idx := idx DIV NUMBER(CHAR)
        END;
        <*ASSERT idx = 0*>
        FOR i := FIRST(name) TO LAST(name) DO
          Wr.PutChar(longNames.wr, name[i])
        END;
        Wr.PutChar(longNames.wr, VAL(0,CHAR))
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
        IF longNames.wr # NIL THEN
          Wr.Close(longNames.wr);
          longNames.wr := NIL
        END;
        IF longNames.rd = NIL THEN
          longNames.rd := FileRd.Open(longNames.nm)
        END;

        FOR i := FIRST(inst) TO LAST(inst) - 1 DO
          idx := NUMBER(CHAR) * idx + ORD(inst[i])
        END;
        Rd.Seek(longNames.rd, idx);
        REPEAT
          c := Rd.GetChar(longNames.rd);
          buffer[j] := c;
          INC(j); INC(idx);
        UNTIL c = VAL(0,CHAR)
      END
    END
  END DecodeName;

REVEAL
  LongNames = BRANDED Brand & " LongNames" OBJECT
    nm : TEXT;
    wr : Wr.T;
    rd : Rd.T;
  END;

VAR rand := NEW(Random.Default).init();

VAR longNameNames : TextList.T := NIL;
    
PROCEDURE NewLongNames() : LongNames =
  VAR
    q := rand.integer(FIRST(CARDINAL), LAST(CARDINAL));
    nm := Compiler.ThisFile() & ":" & Fmt.Int(q);
  BEGIN
    longNameNames := TextList.Cons(nm, longNameNames);

    TRY FS.DeleteFile(nm) EXCEPT ELSE END;
    
    RETURN NEW(LongNames,
               nm := nm,
               wr := NIL,
               rd := NIL)
  END NewLongNames;

PROCEDURE CleanupLongnames() =
  VAR
    p := longNameNames;
  BEGIN
    WHILE p # NIL DO
      FS.DeleteFile(p.head);
      p := p.tail
    END
  END CleanupLongnames;
  
BEGIN
  Process.RegisterExitor(CleanupLongnames)
END Subcell.
