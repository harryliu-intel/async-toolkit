MODULE Subcell;
IMPORT Random, Wr, Rd, FileRd, FileWr;
IMPORT FS, TextList;
IMPORT Compiler, Fmt, Process;
IMPORT Thread;
IMPORT AL, OSError, Debug;
FROM Fmt IMPORT F;
IMPORT BraceParse;
IMPORT Text;

<*FATAL Thread.Alerted*>

PROCEDURE EncodeName(longNames     : LongNames;
                     READONLY name : ARRAY OF CHAR;
                     VAR      tgt  : InstanceName) =
  BEGIN
    TRY
      IF NUMBER(name) < NUMBER(tgt) THEN
        SUBARRAY(tgt, 0, NUMBER(name)) := name;
        tgt[NUMBER(name)] := VAL(0, CHAR);
        tgt[LAST(tgt)] := VAL(NUMBER(name), CHAR)
      ELSE
        VAR
          idx : CARDINAL;
        BEGIN
          IF longNames.rd # NIL THEN
            TRY
              Rd.Close(longNames.rd);
            EXCEPT
              Rd.Failure(e) => Debug.Error(F("trouble closing longnames file : Rd.Failure : %s", AL.Format(e)))
            END;
            longNames.rd := NIL
          END;
          IF longNames.wr = NIL THEN
            TRY
              longNames.wr := FileWr.OpenAppend(longNames.nm);
            EXCEPT
              OSError.E(e) => Debug.Error(F("trouble opening longnames file \"%s\" for writing : OSError.E : %s", longNames.nm, AL.Format(e)))
            END;
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
    EXCEPT
      Wr.Failure(e) => Debug.Error(F("trouble writing longnames file \"%s\" for writing : OSError.E : %s", longNames.nm, AL.Format(e)))
    END
  END EncodeName;

PROCEDURE DecodeName(longNames     : LongNames;
                     READONLY inst : InstanceName;
                     VAR buffer    : ARRAY OF CHAR) =
  VAR
    last := ORD(inst[LAST(inst)]);
  BEGIN
    TRY
      IF last < LAST(inst) THEN
        SUBARRAY(buffer, 0, last) := SUBARRAY(inst, 0, last)
      ELSE
        VAR
          idx : CARDINAL := 0;
          c : CHAR;
          j := 0;
        BEGIN
          IF longNames.wr # NIL THEN
            TRY
              Wr.Close(longNames.wr);
            EXCEPT
              Wr.Failure(e) => Debug.Error(F("trouble writing longnames file \"%s\" for writing : OSError.E : %s", longNames.nm, AL.Format(e)))
            END;
            longNames.wr := NIL
          END;
          IF longNames.rd = NIL THEN
            TRY
              longNames.rd := FileRd.Open(longNames.nm)
            EXCEPT
              OSError.E(e) => Debug.Error(F("trouble opening longnames file \"%s\" for reading : OSError.E : %s", longNames.nm, AL.Format(e)))
            END;
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
    EXCEPT
      Rd.Failure(e) => Debug.Error(F("trouble reading longnames file : Rd.Failure : %s", AL.Format(e)))
    |
      Rd.EndOfFile => Debug.Error("unexpected EOF reading longnames file")
    END
  END DecodeName;

PROCEDURE DecodeNameToText(longNames     : LongNames;
                           READONLY inst : InstanceName) : TEXT =
  VAR
    buf : BraceParse.Buffer;
  BEGIN
    DecodeName(longNames, inst, buf);
    FOR i := FIRST(buf) TO LAST(buf) DO
      IF buf[i] = VAL(0, CHAR) THEN
        RETURN Text.FromChars(SUBARRAY(buf, 0, i))
      END
    END;
    <*ASSERT FALSE*>
  END DecodeNameToText;

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
      TRY FS.DeleteFile(p.head) EXCEPT ELSE END;
      p := p.tail
    END
  END CleanupLongnames;
  
BEGIN
  Process.RegisterExitor(CleanupLongnames)
END Subcell.
