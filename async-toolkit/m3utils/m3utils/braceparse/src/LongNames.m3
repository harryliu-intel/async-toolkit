MODULE LongNames;
IMPORT Random, Wr, Rd, FileRd, FileWr;
IMPORT FS, TextList;
IMPORT Fmt, Process;
IMPORT Thread;
IMPORT AL, OSError, Debug;
FROM Fmt IMPORT F, Int;
IMPORT BraceParse;
IMPORT Text;
IMPORT InstanceName;

<*FATAL Thread.Alerted*>

PROCEDURE Encode(longNames     : T;
                 READONLY name : ARRAY OF CHAR;
                 VAR      tgt  : InstanceName.T) =
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
          
          FOR i := FIRST(tgt) TO LAST(tgt) - 1 DO
            tgt[i] := VAL(idx MOD NUMBER(CHAR),CHAR);
            idx := idx DIV NUMBER(CHAR)
          END;
          <*ASSERT idx = 0*>
          tgt[LAST(tgt)] := LAST(CHAR);

          FOR i := FIRST(name) TO LAST(name) DO
            Wr.PutChar(longNames.wr, name[i])
          END;
          Wr.PutChar(longNames.wr, VAL(0,CHAR))
        END
      END
    EXCEPT
      Wr.Failure(e) => Debug.Error(F("trouble writing longnames file \"%s\" for writing : OSError.E : %s", longNames.nm, AL.Format(e)))
    END
  END Encode;

PROCEDURE Decode(longNames     : T;
                 READONLY inst : InstanceName.T;
                 VAR buffer    : ARRAY OF CHAR) =
  VAR
    last := ORD(inst[LAST(inst)]);
    idx : CARDINAL := 0;
    j := 0;
  BEGIN
    TRY
      IF last # ORD(LAST(CHAR)) THEN
        SUBARRAY(buffer, 0, last) := SUBARRAY(inst, 0, last);
        buffer[last] := VAL(0,CHAR)
      ELSE
        VAR
          c : CHAR;
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

          FOR i := LAST(inst) - 1 TO FIRST(inst) BY -1 DO
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
      Rd.EndOfFile => 
      longNameNames := NIL; (* do not delete any files *)
      VAR
        str := "";
      BEGIN
        FOR i := FIRST(inst) TO LAST(inst) DO
          str := str & "16_" & Int(ORD(inst[i]), base := 16) & " "
        END;
        Debug.Error(F("unexpected EOF reading longnames file, idx = %s j = %s inst = %s", Int(idx), Int(j), str))
      END
    END
  END Decode;

PROCEDURE DecodeToText(longNames     : T;
                       READONLY inst : InstanceName.T) : TEXT =
  VAR
    buf : BraceParse.Buffer;
  BEGIN
    Decode(longNames, inst, buf);
    FOR i := FIRST(buf) TO LAST(buf) DO
      IF buf[i] = VAL(0, CHAR) THEN
        RETURN Text.FromChars(SUBARRAY(buf, 0, i))
      END
    END;
    <*ASSERT FALSE*>
  END DecodeToText;

REVEAL
  T = BRANDED Brand OBJECT
    nm : TEXT;
    wr : Wr.T;
    rd : Rd.T;
  END;

VAR rand := NEW(Random.Default).init();

VAR longNameNames : TextList.T := NIL;

VAR mu := NEW(MUTEX);
    
PROCEDURE New() : T =
  VAR
    q : CARDINAL;
    nm : TEXT;
  BEGIN
    LOCK mu DO
      q := rand.integer(FIRST(CARDINAL), LAST(CARDINAL));
      nm := Brand & ":" & Fmt.Int(q);
      longNameNames := TextList.Cons(nm, longNameNames)
    END;

    TRY FS.DeleteFile(nm) EXCEPT ELSE END;
    
    RETURN NEW(T,
               nm := nm,
               wr := NIL,
               rd := NIL)
  END New;

PROCEDURE Cleanup() =
  VAR
    p := longNameNames;
  BEGIN
    WHILE p # NIL DO
      TRY FS.DeleteFile(p.head) EXCEPT ELSE END;
      p := p.tail
    END
  END Cleanup;
  
BEGIN
  Process.RegisterExitor(Cleanup)
END LongNames.
