MODULE FsdbComms;
IMPORT Debug;
IMPORT Wr;
IMPORT Rd;
IMPORT TextReader;
IMPORT SpiceCompress;
IMPORT AL;
IMPORT Text;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT UnsafeReader;
IMPORT Thread;
IMPORT Atom;

(* the following procedures communicate with a nanosimrd process *)

CONST TE = Text.Equal;
      LR = LongReal;

VAR doDebug := Debug.DebugThis("FsdbComms");

(* Convert Rd.EndOfFile to Rd.Failure *)

PROCEDURE RaiseRdEndOfFile(str : TEXT) RAISES { Rd.Failure } =
  BEGIN
    RAISE Rd.Failure(AL.List2(Atom.FromText("FsdbComms." & str),
                              Atom.FromText("Rd.EndOfFile")))
  END RaiseRdEndOfFile;

PROCEDURE RaiseRdFailure(str : TEXT; x : AL.T) RAISES { Rd.Failure } =
  BEGIN
    RAISE Rd.Failure(AL.Cons(Atom.FromText("FsdbComms." & str), x))
  END RaiseRdFailure;
    
PROCEDURE RaiseWrFailure(str : TEXT; x : AL.T) RAISES { Wr.Failure } =
  BEGIN
    RAISE Wr.Failure(AL.Cons(Atom.FromText("FsdbComms." & str), x))
  END RaiseWrFailure;

PROCEDURE PutCommandG(wr : Wr.T; cmd : TEXT)
  RAISES { Thread.Alerted, Wr.Failure } =
  BEGIN
    TRY
      IF doDebug THEN
        Debug.Out(F("Fsdb.Parse.PutCommand \"%s\"", cmd));
      END;
      Wr.PutText(wr, cmd);
      Wr.PutChar(wr, '\n');
      Wr.Flush(wr);
    EXCEPT
      Wr.Failure(x) =>
      RaiseWrFailure("PutCommandG", x)
    END
  END PutCommandG;

PROCEDURE GetResponseG(rd : Rd.T; matchKw : TEXT) : TextReader.T
  RAISES { Thread.Alerted, Rd.Failure } =
  VAR
    kw : TEXT;
  BEGIN
    IF doDebug THEN
      Debug.Out(F("Fsdb.Parse.GetResponse \"%s\"", matchKw));
    END;

    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line) DO
          IF doDebug THEN
            Debug.Out(F("Fsdb.Parse.GotResponse \"%s\"", line));
          END;
          IF reader.next(" ", kw, TRUE) THEN
            IF TE(kw, matchKw) THEN
              RETURN reader
            END
          END
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      RaiseRdFailure("GetResponseG", x);
      <*ASSERT FALSE*>
    |
      Rd.EndOfFile =>
      RaiseRdEndOfFile("GetResponseG");
      <*ASSERT FALSE*>
    END
  END GetResponseG;

PROCEDURE ReadCompressedNodeDataG(rd         : Rd.T;
                                  VAR nodeid : CARDINAL;
                                  VAR norm   : SpiceCompress.Norm) : TEXT
  RAISES { Thread.Alerted, Rd.Failure } =
  VAR
    kw    : TEXT;
    bytes : CARDINAL;
    tag   : CHAR;
  BEGIN
    IF doDebug THEN
      Debug.Out(F("FsdbComms.ReadCompressedNodeDataG start"))
    END;
    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line) DO
          IF doDebug THEN
            Debug.Out(F("FsdbComms.ReadCompressedNodeDataG line \"%s\"", line))
          END;

          IF reader.next(" ", kw, TRUE) THEN
            IF    TE(kw, "E") THEN
              Debug.Error(F("Got time mismatch: nodeid %s: line %s",
                            Int(nodeid), line));
              <*ASSERT FALSE*>
            ELSIF TE(kw, "ZZZ") THEN
              (* this code synchronized with Main.m3<spicestream> *)
              tag      := Rd.GetChar(rd);
              <*ASSERT tag = 'x'*>
              nodeid   := UnsafeReader.ReadI(rd);
              bytes    := UnsafeReader.ReadI(rd);
              norm.min := UnsafeReader.ReadLR(rd);
              norm.max := UnsafeReader.ReadLR(rd);

              IF doDebug THEN
                Debug.Out(F("FsdbComms.ReadCompressedNodeDataG got nodeid %s bytes %s m in %s max %s",
                            Int(nodeid), Int(bytes), LR(norm.min), LR(norm.max) ));
              END;
              
              WITH buflen  = bytes - 8, (* 9 is len of min, max, code *)
                   chars   = NEW(REF ARRAY OF CHAR, buflen),
                   got     = Rd.GetSub(rd, chars^) DO
                IF got # buflen THEN RAISE Rd.EndOfFile END;

                IF doDebug THEN
                  Debug.Out(F("FsdbComms.ReadCompressedNodeDataG nodeid %s bytes %s min %s max %s",
                              Int(nodeid), Int(bytes),
                              LR(norm.min), LR(norm.max)))
                END;
                RETURN Text.FromChars(chars^)
              END
            ELSE
              Debug.Error(F("?syntax error : ReadBinaryNodeData: got \"%s\"",
                            line));
              <*ASSERT FALSE*>
            END
          END
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      RaiseRdFailure("ReadCompressedNodeDataG", x);
      <*ASSERT FALSE*>
    |
      Rd.EndOfFile =>
      RaiseRdEndOfFile("ReadCompressedNodeDataG");
      <*ASSERT FALSE*>
    END
  END ReadCompressedNodeDataG;
  
PROCEDURE ReadBinaryNodeDataG(rd         : Rd.T;
                              VAR nodeid : CARDINAL;
                              VAR buff   : ARRAY OF LONGREAL)
  RAISES { Thread.Alerted, Rd.Failure } =
  VAR
    kw : TEXT;
    n  : CARDINAL;
  BEGIN
    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line) DO
          IF doDebug THEN
            Debug.Out(F("FsdbComms.ReadBinaryNodeData line \"%s\"", line))
          END;

          IF reader.next(" ", kw, TRUE) THEN
            IF    TE(kw, "E") THEN
              Debug.Error(F("Got time mismatch: nodeid %s: line %s",
                            Int(nodeid), line))
            ELSIF TE(kw, "OK") THEN
              WITH tag = Rd.GetChar(rd) DO
                nodeid := UnsafeReader.ReadI(rd);
                n      := UnsafeReader.ReadI(rd);

                IF doDebug THEN
                  Debug.Out(F("FsdbComms.ReadBinaryNodeData tag %s nodeid %s n %s",
                              Text.FromChar(tag),
                              Int(nodeid),
                              Int(n)))
                END;
                
                IF n # NUMBER(buff) THEN
                  Debug.Error(F("Size mismatch n %s # NUMBER(buff) %s",
                                Int(n), Int(NUMBER(buff))))
                END;
                UnsafeReader.ReadLRA(rd, buff);

                IF doDebug THEN
                  Debug.Out(F("ReadBinaryNodeData buff[0] %s",
                              LR(buff[0])))
                END;
                RETURN
                END
            ELSE
              Debug.Error(F("?syntax error : ReadBinaryNodeData: got \"%s\"",
                            line))
            END
          END
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      RaiseRdFailure("ReadBinaryNodeDataG", x)
    |
      Rd.EndOfFile =>
      RaiseRdEndOfFile("ReadBinaryNodeDataG")
    END
  END ReadBinaryNodeDataG;

PROCEDURE ReadInterpolatedBinaryNodeDataG(rd          : Rd.T;
                                          VAR nodeid  : CARDINAL;
                                          VAR buff    : ARRAY OF LONGREAL;
                                          interpolate : LONGREAL;
                                          unit        : LONGREAL)
  RAISES { Thread.Alerted, Rd.Failure } =
  VAR
    kw : TEXT;
    n  : CARDINAL;
  BEGIN
    <*ASSERT interpolate # 0.0d0*>
    IF doDebug THEN
      Debug.Out(F("ReadInterpolatedBinaryNodeDataG, NUMBER(buff)=%s",
                Int(NUMBER(buff))))
    END;
    TRY
      LOOP
        WITH line    = Rd.GetLine(rd),
             reader  = NEW(TextReader.T).init(line) DO
          IF doDebug THEN
            Debug.Out(F("Fsdb.Parse.ReadInterpolatedBinaryNodeData line \"%s\"", line))
          END;

          IF reader.next(" ", kw, TRUE) THEN
            IF    TE(kw, "E") THEN
              Debug.Error(F("Got time mismatch: nodeid %s: line %s",
                            Int(nodeid), line))
            ELSIF TE(kw, "OK") THEN
              WITH tag = Rd.GetChar(rd) DO
                nodeid := UnsafeReader.ReadI(rd);
                n      := UnsafeReader.ReadI(rd);

                IF doDebug THEN
                  Debug.Out(F("ReadInterpolatedBinaryNodeData tag %s nodeid %s n %s",
                              Text.FromChar(tag),
                              Int(nodeid),
                              Int(n)))
                END;

                WITH hi = NEW(REF ARRAY OF CARDINAL, n),
                     lo = NEW(REF ARRAY OF CARDINAL, n),
                     vv = NEW(REF ARRAY OF LONGREAL, n) DO
                  UnsafeReader.ReadUA (rd, hi^);
                  UnsafeReader.ReadUA (rd, lo^);
                  UnsafeReader.ReadLRA(rd, vv^);

                  IF doDebug THEN
                    Debug.Out(F("ReadInterpolatedBinaryNodeData read %s records", Int(n)))
                  END;

                  IF n = 0 AND NUMBER(buff) # 0 THEN
                    Debug.Error("?no data")
                  END;
                  
                  VAR
                    j := 0;
                    y := vv[j];
                    py := y;
                    dt := 0.0d0;
                    pt := 0.0d0; 
                  BEGIN
                    FOR i := FIRST(buff) TO LAST(buff) DO
                      WITH st = FLOAT(i, LONGREAL) * interpolate DO
                        WHILE st > dt DO
                          INC(j);
                          IF j > LAST(vv^) THEN
                            pt := dt;
                            dt := LAST(LONGREAL)
                          ELSE
                            pt := dt;
                            dt := (FLOAT(hi[j],LONGREAL) * TwoToThe32 + FLOAT(lo[j], LONGREAL)) * unit;
                            py := y;
                            y := vv[j]
                          END
                        END;
                        
                        <*ASSERT st <= dt*>
                        
                        IF    dt = LAST(LONGREAL) OR dt = st THEN
                          (* fill with the last data point *)
                          buff[i] := y
                        ELSE
                          <*ASSERT dt # pt*>
                          (* normal interpolation case *)
                          buff[i] := (st - pt) / (dt - pt) * (y - py) + py
                        END
                      END
                    END
                  END;
                  
                  IF doDebug THEN
                    Debug.Out(F("ReadInterpolatedBinaryNodeData buff[0] %s buff[LAST(buff)=%s] %s",
                                LR(buff[0]), Int(LAST(buff)), LR(buff[LAST(buff)]) ))
                  END;
                  RETURN
                END
              END
            ELSE
              Debug.Error(F("?syntax error : ReadInterpolatedBinaryNodeData: got \"%s\"",
                            line))
            END
          END
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      RaiseRdFailure("ReadInterpolatedBinaryNodeDataG", x)
    |
      Rd.EndOfFile =>
      RaiseRdEndOfFile("ReadInterpolatedBinaryNodeDataG")
    END
  END ReadInterpolatedBinaryNodeDataG;

PROCEDURE GetLineUntilG(rd : Rd.T; term : TEXT; VAR line : TEXT) : BOOLEAN
  RAISES { Thread.Alerted, Rd.Failure } =
  BEGIN
    TRY
      WITH this    = Rd.GetLine(rd) DO
        IF TE(this, term) THEN
          RETURN FALSE
        ELSE
          line := this;
          RETURN TRUE
        END
      END
    EXCEPT
      Rd.Failure(x) =>
      RaiseRdFailure("GetLineUntilG", x);
      <*ASSERT FALSE*>
    |
      Rd.EndOfFile =>
      RaiseRdEndOfFile("GetLineUntilG");
      <*ASSERT FALSE*>
    END
  END GetLineUntilG;

BEGIN END FsdbComms.
