MODULE TcamSequencer;
IMPORT Command, CommandSeq;
IMPORT Verb, BitInteger;
FROM Fmt IMPORT Int, F;
IMPORT RefSeq, Debug, Text, Bit, CardSeq;
IMPORT Word;

CONST SI = BitInteger.Small;

PROCEDURE Compile(prog          : CommandSeq.T;
                  VAR      seq  : ARRAY Verb.T OF REF ARRAY OF BitInteger.T) =

  PROCEDURE Clear() =
    BEGIN FOR i := FIRST(buf) TO LAST(buf) DO buf[i] := SI(0) END END Clear;

  PROCEDURE Push() =
    VAR
      dbgStr := F("cycle %5s", Int(tmp[FIRST(tmp)].size()));
    BEGIN 
      FOR i := FIRST(buf) TO LAST(buf) DO 
        dbgStr := dbgStr & F(" %s %s", 
                             Verb.Names[i], 
                             Int(NARROW(buf[i],BitInteger.SmallPromise).v)
        )
      END;

      Debug.Out(dbgStr);

      FOR i := FIRST(buf) TO LAST(buf) DO tmp[i].addhi(buf[i]) END 
    END Push;

  PROCEDURE Flip(p : BitInteger.SmallPromise) : BitInteger.SmallPromise =
    BEGIN RETURN SI(1-p.v) END Flip;

  TYPE
    V = Verb.T;
  VAR
    tmp : ARRAY V OF RefSeq.T;
    buf : ARRAY V OF BitInteger.T;
  BEGIN
    FOR i := FIRST(tmp) TO LAST(tmp) DO tmp[i] := NEW(RefSeq.T).init() END;
    
    Clear();
    FOR i := 0 TO prog.size()-1 DO

      WITH ordr = prog.get(i) DO
        CASE ordr.v OF
          V.Nop   => (* skip *)
        |
          V.Rset  => buf[V.Rset] := SI(1);
        |
          V.Look  => buf[V.Look] := SI(1); 
                     buf[V.Data] := SI(ordr.p0)
        |
          V.Read  => buf[V.Read] := SI(1); 
                     buf[V.Addr] := SI(ordr.p0)
        |
          V.Writ  => buf[V.Writ] := SI(1); 
                     buf[V.Data] := SI(ordr.p0); 
                     buf[V.Addr] := SI(ordr.p1)
        ELSE
          <*ASSERT FALSE*>
        END;
        Push();

        Clear();

        FOR i := 1 TO Verb.Nops[ordr.v] DO Push() END (* push nops as req'd *)

      END
    END;

    Push(); (* a few more cycles for good measure *) 
    Push();

    (* invert Rset buffer *)
    FOR i := 0 TO tmp[V.Rset].size()-1 DO
      tmp[V.Rset].put(i,Flip(tmp[V.Rset].get(i)))
    END;

    FOR v := FIRST(seq) TO LAST(seq) DO
      seq[v] := NEW(REF ARRAY OF BitInteger.T, tmp[v].size());

      FOR i := 0 TO tmp[v].size()-1 DO
        seq[v][i] := tmp[v].get(i)
      END
    END
  END Compile;

PROCEDURE AddKey(prog   : CommandSeq.T;
                 keyIdx : CARDINAL;
                 key    : TEXT;
                 w      : CARDINAL) =
  VAR 
    n := Text.Length(key);
    k := ARRAY Bit.T OF CardSeq.T { NEW(CardSeq.T).init(), 
                                  NEW(CardSeq.T).init() } ;
    z : ARRAY Bit.T OF Word.T;
  BEGIN
    FOR i := n-1 TO 0 BY -1 DO
      WITH c   = Text.GetChar(key, i) DO
        CASE c OF
          '0' => k[0].addhi(1); k[1].addhi(0)
        |
          '1' => k[0].addhi(0); k[1].addhi(1)
        |
          '?' => k[0].addhi(1); k[1].addhi(1)
        |
          '-' => k[0].addhi(0); k[1].addhi(0)
        ELSE
          <*ASSERT FALSE*>
        END
      END
    END;

    VAR
      q : Bit.T;
    BEGIN
      FOR i := FIRST(k) TO LAST(k) DO
        FOR b := 0 TO w-1 DO
          IF b > k[i].size() THEN
            q := 1
          ELSE
            q := k[i].get(b)
          END;
          z[i] := Word.Insert(z[i], q, b, 1)
        END
      END
    END;
    prog.addhi(Command.T { Verb.T.Writ, z[0], keyIdx*2   });
    prog.addhi(Command.T { Verb.T.Nop });
    prog.addhi(Command.T { Verb.T.Writ, z[1], keyIdx*2+1 });
    prog.addhi(Command.T { Verb.T.Nop })
  END AddKey;

BEGIN END TcamSequencer.
