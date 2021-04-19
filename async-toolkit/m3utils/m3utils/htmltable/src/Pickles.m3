(* $Id$ *)

MODULE Pickles;
IMPORT Pickle2 AS Pickle, TextWr, TextRd, Wr, Thread, SlowTextCompress, Database;
IMPORT DatabaseUtils;
IMPORT Text, Process;
IMPORT Rd;
IMPORT Fingerprint;
IMPORT Pathname, FileWr, FileRd, FS, Debug;
IMPORT ProcUtils;

TYPE CMode = SlowTextCompress.Mode;

TYPE
  TWriter = Pickle.Writer OBJECT
  OVERRIDES
    write := WriteP
  END;

  TSpecial = Pickle.Special OBJECT
  OVERRIDES
    read := ReadS
  END;

PROCEDURE ReadS(self : TSpecial;
                rd: Pickle.Reader; 
                id: Pickle.RefID): REFANY
  RAISES {Pickle.Error, Rd.EndOfFile, Rd.Failure, Thread.Alerted} =
  VAR
    res := Pickle.Special.read(self,rd,id);
  BEGIN
    IF res # NIL AND ISTYPE(res, CharArray) THEN
      RETURN Text.FromChars(NARROW(res, CharArray)^)
    ELSE
      RETURN res
    END
  END ReadS;
  

PROCEDURE ToChars(txt : TEXT) : CharArray =
  VAR
    a : CharArray;
  BEGIN
    IF txt = NIL THEN RETURN NIL END;

    a := NEW(CharArray, Text.Length(txt));
    FOR i := FIRST(a^) TO LAST(a^) DO
      a[i] := Text.GetChar(txt,i)
    END;

    RETURN a
  END ToChars;
  
TYPE CharArray = BRANDED "SpecialCharArray" REF ARRAY OF CHAR;

PROCEDURE WriteP(self: TWriter; r: REFANY)
  RAISES { Pickle.Error, Wr.Failure, Thread.Alerted} =
  BEGIN
    IF r # NIL AND ISTYPE(r, TEXT) THEN
      WITH chars = ToChars(r) DO
        Pickle.Writer.write(self, chars)
      END
    ELSE
      <* ASSERT r = NIL OR NOT ISTYPE(r, TEXT) *>
      Pickle.Writer.write(self, r)
    END;
  END WriteP;

(**********************************************************************)

PROCEDURE UnStuff(bytea : Database.ByteA) : REFANY RAISES { ProcUtils.ErrorExit } =
  VAR
    pickle := SlowTextCompress.Text(CMode.Decompress,
                                    DatabaseUtils.ByteA2Text(bytea));
    pickleRd := NEW(TextRd.T).init(pickle);
  BEGIN
    TRY
      RETURN NEW(Pickle.Reader, rd := pickleRd).read()
    EXCEPT
      Pickle.Error(e) => Process.Crash("Pickle.Error: " & e)
    |
      Rd.EndOfFile => Process.Crash("Short read in UnStuffPickle")
    |
      Rd.Failure, Thread.Alerted =>
        Process.Crash("I/O error in UnStuffPickle")
    END;
    <* ASSERT FALSE *>
  END UnStuff;

PROCEDURE UnStuffOC(rd : Rd.T;
                    tmpNam1, tmpNam2 : Pathname.T) : REFANY =
  VAR
    wr1 := FileWr.Open(tmpNam1);
  BEGIN
    DatabaseUtils.ByteA2TextRdWr(rd,wr1);
    Wr.Close(wr1);

    Debug.Out("Pickles.UnStuffOC: ByteA2TextRdWr done.");

    WITH rd = FileRd.Open(tmpNam1),
         wr2 = FileWr.Open(tmpNam2) DO
      Debug.Out("Attempting decompress of " & tmpNam1 & " -> " & tmpNam2);
      SlowTextCompress.RdWr(CMode.Decompress,rd,wr2)
    END;

    Debug.Out("Pickles.UnStuffOC: SlowTextCompress.RdWr done.");

    TRY
      WITH pickleRd = FileRd.Open(tmpNam2) DO
        TRY
          TRY
            RETURN NEW(Pickle.Reader, rd := pickleRd).read()
          EXCEPT
            Pickle.Error(e) => Process.Crash("Pickle.Error: " & e)
          |
            Rd.EndOfFile => Process.Crash("Short read in UnStuffPickle")
          |
            Rd.Failure, Thread.Alerted =>
            Process.Crash("I/O error in UnStuffPickle")
          END
        FINALLY
          TRY Rd.Close(pickleRd) EXCEPT ELSE END
        END
      END;
      <* ASSERT FALSE *>
    FINALLY
      TRY FS.DeleteFile(tmpNam1) EXCEPT ELSE END;
      TRY FS.DeleteFile(tmpNam2) EXCEPT ELSE END
    END
  END UnStuffOC;

PROCEDURE Stuff(ref : REFANY) : Database.ByteA =
  VAR
    pickleWr := NEW(TextWr.T).init();
    pickle : TEXT;
  BEGIN
    TRY
      NEW(Pickle.Writer, wr := pickleWr).write(ref)

    EXCEPT
      Pickle.Error(e) => Process.Crash("Pickle.Error: " & e)
    |
      Wr.Failure, Thread.Alerted =>
        Process.Crash("I/O error in StuffPickle")
    END;
    pickle := TextWr.ToText(pickleWr);
    RETURN Crunch(pickle)
  END Stuff;

PROCEDURE StuffAndFingerprint(ref : REFANY; 
                               VAR fingerprint : Fingerprint.T) : Database.ByteA =
  VAR
    pickleWr := NEW(TextWr.T).init();
    pickle : TEXT;
  BEGIN
    TRY
      NEW(Pickle.Writer, wr := pickleWr).write(ref)

    EXCEPT
      Pickle.Error(e) => Process.Crash("Pickle.Error: " & e)
    |
      Wr.Failure, Thread.Alerted =>
        Process.Crash("I/O error in StuffPickle")
    END;

    pickle := TextWr.ToText(pickleWr);
    fingerprint := Fingerprint.FromText(pickle);
    RETURN Crunch(pickle)
  END StuffAndFingerprint;

PROCEDURE Crunch(pickle : TEXT) : Database.ByteA =
  BEGIN 
    RETURN DatabaseUtils.Text2ByteA(SlowTextCompress.Text(CMode.Compress,
                                                     pickle)) 
  END Crunch;

PROCEDURE StuffAndFingerprintWr(ref : REFANY; 
                                VAR fingerprint : Fingerprint.T;
                                wr : Wr.T;
                                tmpNam1, tmpNam2 : Pathname.T) =
  (* as above, but stuffs to Wr.T *)
  VAR
    wr1 := FileWr.Open(tmpNam1);
  BEGIN
    TRY
      TRY
        NEW(Pickle.Writer, wr := wr1).write(ref)

      EXCEPT
        Pickle.Error(e) => Process.Crash("Pickle.Error: " & e)
      |
        Wr.Failure, Thread.Alerted =>
        Process.Crash("I/O error in StuffPickle")
      END;
      Wr.Close(wr1);

      (* fingerprint *)
      CONST
        BufSize = 8192;
      VAR
        fp := Fingerprint.OfEmpty;
        buff : ARRAY [0..BufSize-1] OF CHAR;
        rd := FileRd.Open(tmpNam1);
        got : CARDINAL;
      BEGIN
        REPEAT
          got := Rd.GetSub(rd,buff);
          fp := Fingerprint.FromChars(SUBARRAY(buff,0,got),
                                      fp)
        UNTIL got < BufSize;
        Rd.Close(rd);
        fingerprint := fp
      END;

      (* crunch it *)
      WITH rd = FileRd.Open(tmpNam1),
           wr2 = FileWr.Open(tmpNam2) DO
        SlowTextCompress.RdWr(CMode.Compress,rd,wr2)
      END;

      WITH rd2 = FileRd.Open(tmpNam2) DO
        DatabaseUtils.Text2ByteARdWr(rd2,wr)
      END;
      Wr.Close(wr)
    FINALLY
      FS.DeleteFile(tmpNam1); FS.DeleteFile(tmpNam2)
    END
  END StuffAndFingerprintWr;

PROCEDURE StuffAndFingerprintAsTextWr(ref : REFANY; 
                                VAR fingerprint : Fingerprint.T;
                                wr : Wr.T;
                                tmpNam1 : Pathname.T) =
  (* as above, but stuffs to Wr.T *)
  VAR
    wr1 := FileWr.Open(tmpNam1);
  BEGIN
    TRY
      TRY
        NEW(Pickle.Writer, wr := wr1).write(ref)

      EXCEPT
        Pickle.Error(e) => Process.Crash("Pickle.Error: " & e)
      |
        Wr.Failure, Thread.Alerted =>
        Process.Crash("I/O error in StuffPickle")
      END;
      Wr.Close(wr1);

      (* fingerprint *)
      CONST
        BufSize = 8192;
      VAR
        fp := Fingerprint.OfEmpty;
        buff : ARRAY [0..BufSize-1] OF CHAR;
        rd := FileRd.Open(tmpNam1);
        got : CARDINAL;
      BEGIN
        REPEAT
          got := Rd.GetSub(rd,buff);
          fp := Fingerprint.FromChars(SUBARRAY(buff,0,got),
                                      fp)
        UNTIL got < BufSize;
        Rd.Close(rd);
        fingerprint := fp
      END;

      (* crunch it *)
      WITH rd = FileRd.Open(tmpNam1) DO
        SlowTextCompress.RdWr(CMode.Compress,rd,wr)
      END;

      Wr.Close(wr)
    FINALLY
      FS.DeleteFile(tmpNam1)
    END
  END StuffAndFingerprintAsTextWr;
  
(**********************************************************************)


BEGIN END Pickles.
