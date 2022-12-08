MODULE Main;
IMPORT ParseParams;
IMPORT Stdio;
IMPORT Debug;
IMPORT Params;
IMPORT AL;
IMPORT OSError;
IMPORT Rd;
IMPORT Pathname;
FROM Fmt IMPORT F, Int;
IMPORT Trace;
IMPORT Wr, FileWr;
IMPORT TextWr;
IMPORT Text;
IMPORT UnsafeWriter;
IMPORT FileRd;
IMPORT UnsafeReader;
IMPORT SpiceCompress;
IMPORT Thread;
IMPORT TripleRefTbl;
IMPORT Fsdb;
IMPORT ArithConstants;
IMPORT ArithCode;
IMPORT ArithCallback;
IMPORT Matrix;

<*FATAL Thread.Alerted*>

CONST
  Usage    = "";
  TE       = Text.Equal;

TYPE
  Mode = { ReadBinary, Compress, Filter };

CONST
  ModeNames = ARRAY Mode OF TEXT { "ReadBinary", "Compress", "Filter" };
  
PROCEDURE Lookup(str : TEXT; READONLY a : ARRAY OF TEXT) : CARDINAL =
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      IF TE(str, a[i]) THEN RETURN i END
    END;
    VAR
      str := F("could not find %s among alternatives :");
    BEGIN
      FOR i := FIRST(a) TO LAST(a) DO
        str := str & F( " \"%s\"" , a[i])
      END;
      Debug.Error(str)
    END;
    <*ASSERT FALSE*>
  END Lookup;

PROCEDURE DoArithCompress(of : TEXT;
                          VAR codeIdx : ArithConstants.CodeIdx) : TEXT =
  VAR
    enTxt : TEXT;
  BEGIN
    codeIdx := 1;

    WITH ft      = ArithConstants.CodeBook[codeIdx],
         code    = NEW(ArithCode.T).init(ft),
         encoder = code.newEncoder(),
         enWr    = TextWr.New(),
         enCb    = NEW(ArithCallback.Writer).init(enWr),

         (* verify code *)
         decoder = code.newDecoder(),
         deWr    = TextWr.New(),
         deCb    = NEW(ArithCallback.Writer).init(deWr)
     DO
      encoder.setCallback(enCb);
      encoder.text(of);
      encoder.eof();

      enTxt := TextWr.ToText(enWr);
      
      decoder.setCallback(deCb);
      decoder.text(enTxt);
      decoder.eof();

      WITH deTxt = TextWr.ToText(deWr) DO
        IF NOT TE(of, deTxt) THEN
          VAR
            msg : TEXT;
            ofLen := Text.Length(of);
            deLen := Text.Length(deTxt);
          BEGIN
            IF ofLen # deLen THEN
              msg := F("length mismatch of %s # de %s", Int(ofLen), Int(deLen))
            ELSE
              FOR i := 0 TO ofLen - 1 DO
                WITH ofChar = Text.GetChar(of, i),
                     deChar = Text.GetChar(deTxt, i) DO
                  IF ofChar # deChar THEN
                    msg := F("mismatch @ i=%s : of[i]='%s' # de[i]='%s'",
                             Int(i),
                             Text.FromChar(ofChar), Text.FromChar(deChar))
                  END
                END
              END
            END;
            Debug.Error("??? DoArithCompress verify error, " & msg)
          END
        END
      END
    END;

    RETURN enTxt
  END DoArithCompress;

TYPE
  FilterData = RECORD
    (* in filtering mode, this is information known by the driver
       program, and that will be passed to us on our command line *)
    npoints     : CARDINAL;
    interpolate : LONGREAL;
    unit        : LONGREAL;
  END;
  
VAR
  pp                             := NEW(ParseParams.T).init(Stdio.stderr);
  traceRt       : Pathname.T     := "xa";
  doAllDumps    : BOOLEAN;
  relPrec                        := 0.005d0;
  mode          : Mode;
  nodeId        : CARDINAL;
  trace         : Trace.T;
  outFn, inFn   : Pathname.T     := "-";
  wr : Wr.T;
  doDump        : BOOLEAN;
  fd            : FilterData;
  noArith       : BOOLEAN;
  
BEGIN

  TRY

    noArith := pp.keywordPresent("-noarith");

    doDump := pp.keywordPresent("-dump");
    
    IF pp.keywordPresent("-mode") THEN
      mode := VAL(Lookup(pp.getNext(), ModeNames), Mode)
    END;
    
    doAllDumps := pp.keywordPresent("-dodumpall");
    
    IF pp.keywordPresent("-t") THEN
      traceRt := pp.getNext()
    END;

    IF pp.keywordPresent("-o") THEN
      outFn := pp.getNext()
    END;

    IF pp.keywordPresent("-i") THEN
      inFn := pp.getNext()
    END;

    IF pp.keywordPresent("-prec") THEN
      relPrec := pp.getNextLongReal()
    END;

    IF pp.keywordPresent("-n") THEN
      nodeId := pp.getNextInt()
    END;

    IF pp.keywordPresent("-filter") THEN
      mode           := Mode.Filter;
      fd.npoints     := pp.getNextInt();
      fd.interpolate := pp.getNextLongReal();
      fd.unit        := pp.getNextLongReal()
    END;
    
  EXCEPT
    ParseParams.Error =>
    Debug.Error("Can't parse command-line parameters\nUsage: " &
      Params.Get(0) & " " & Usage)
  END;

  (* all modes produce some sort of output! *)

  TRY
    IF TE(outFn, "-") THEN
      wr := Stdio.stdout
    ELSE
      wr := FileWr.Open(outFn)
    END
  EXCEPT
    OSError.E(x) =>
    Debug.Error(F("Can't open output file %s : OSError.E : %s",
                  outFn, AL.Format(x)))
  END;

  (* detailed execution of modes *)
  
  CASE mode OF
    Mode.ReadBinary =>
    TRY
      trace := NEW(Trace.T).init(traceRt)
    EXCEPT
      OSError.E(x) =>
      Debug.Error(F("Trouble opening input trace %s : OSError.E : %s",
                    traceRt, AL.Format(x)))
    |
      Rd.Failure(x) =>
      Debug.Error("Trouble opening input trace : Rd.Failure : " & AL.Format(x))
    |
      Rd.EndOfFile =>
      Debug.Error(F("Short read opening input trace"))
    END;

    VAR
      nSteps := trace.getSteps();
      a      := NEW(REF ARRAY OF LONGREAL, nSteps);
    BEGIN
      TRY
        trace.getNodeData(nodeId, a^)
      EXCEPT
        Rd.Failure(x) =>
        Debug.Error(F("Can't read trace for node id %s : Rd.Failure : %s",
                      Int(nodeId), AL.Format(x)))
      |
        Rd.EndOfFile =>
        Debug.Error(F("Short read reading trace for node id %s",
                      Int(nodeId)))
      END;

      TRY
        UnsafeWriter.WriteI  (wr, nSteps);
        UnsafeWriter.WriteLRA(wr, a^);

        Wr.Close(wr)
      EXCEPT
        Wr.Failure(x) =>
        Debug.Error(F("Can't write raw trace FOR node id %s : Wr.Failure : %s",
                      Int(nodeId), AL.Format(x)))
      END
    END
  |
    Mode.Compress =>
    VAR
      rd : Rd.T;
      nSteps : CARDINAL;
      a : REF ARRAY OF LONGREAL;
    BEGIN
      TRY
        IF TE(inFn, "-") THEN
          rd := Stdio.stdin
        ELSE
          rd := FileRd.Open(inFn)
        END
      EXCEPT
        OSError.E(x) =>
        Debug.Error(F("Can't open input file %s : OSError.E : %s",
                      outFn, AL.Format(x)))
      END;

      TRY
        nSteps := UnsafeReader.ReadI(rd);
        a      := NEW(REF ARRAY OF LONGREAL, nSteps);
        UnsafeReader.ReadLRA(rd, a^);
      EXCEPT
        Rd.Failure(x) =>
        Debug.Error(F("Trouble reading raw trace from %s : Rd.Failure : %s",
                      inFn, AL.Format(x)))
      |
        Rd.EndOfFile =>
        Debug.Error(F("Short read reading raw trace from %s",
                      inFn))
      END;

      WITH z      = NEW(REF ARRAY OF LONGREAL, nSteps),
           textWr = NEW(TextWr.T).init() DO

        (* first write to mem *)
        TRY
          SpiceCompress.CompressArray("zdebug",
                                      a^,
                                      z^,
                                      relPrec,
                                      doAllDumps,
                                      textWr,
                                      mem    := NEW(TripleRefTbl.Default).init(),
                                      doDump := doDump)
        EXCEPT
          Matrix.Singular =>
          Debug.Error("Internal error attempting waveform compression : Matrix.Singular")
        END;

        <*FATAL Wr.Failure*>
        BEGIN
          WITH txt = TextWr.ToText(textWr),
               len = Text.Length(txt) DO
            UnsafeWriter.WriteI(wr, len);
            Wr.PutText(wr, txt);
            
            Wr.Close(wr)
          END
        END
      END
    END
  |
    Mode.Filter =>
    (* 
       this is the filter mode for nanosimrd.cpp
       
       any changes here must be synchronized with the appropriate changes
       to nanosimrd.cpp 

       The output format of the compression then needs to be synchronized
       with the trace file format specification, used in tracelib and also
       (eventually) in aplot.
    *)
    
    VAR
      rd     := Stdio.stdin;
      a      := NEW(REF ARRAY OF LONGREAL, fd.npoints);
      
      nodeid   : CARDINAL;
      code     : ArithConstants.CodeIdx;
      finalTxt : TEXT;
      finalLen : CARDINAL;
    BEGIN
      Fsdb.ReadInterpolatedBinaryNodeDataG(rd,
                                           nodeid,
                                           a^,
                                           fd.interpolate,
                                           fd.unit);
      
      WITH z      = NEW(REF ARRAY OF LONGREAL, fd.npoints),
           textWr = NEW(TextWr.T).init() DO

        (* first write to mem *)

        TRY
        SpiceCompress.CompressArray("zdebug",
                                    a^,
                                    z^,
                                    relPrec,
                                    doAllDumps,
                                    textWr,
                                    mem    := NEW(TripleRefTbl.Default).init(),
                                    doDump := doDump);
 EXCEPT
          Matrix.Singular =>
          Debug.Error("Internal error attempting waveform compression : Matrix.Singular")
        END;
        (* we now have the polynomially compressed in textWr *)
        
        WITH txt = TextWr.ToText(textWr),
             len = Text.Length(txt) DO

          (* txt is the polynomially compressed waveform *)

          IF noArith THEN
            code     := ArithConstants.ZeroCode;
            finalTxt := txt;
            finalLen := len
          ELSE
            finalTxt := DoArithCompress(txt, code);
            finalLen := Text.Length(finalTxt)
          END;

          Debug.Out(F("%s timesteps, compressed size %s bytes, coded size %s",
                      Int(NUMBER(a^)),
                      Int(len),
                      Int(finalLen)));

          (* write final result to target wr *)
          TRY
            UnsafeWriter.WriteI(wr, finalLen + 1);
            Wr.PutChar         (wr, VAL(code, CHAR));
            Wr.PutText         (wr, finalTxt);
            Wr.Close           (wr)
          EXCEPT
            Wr.Failure(x) =>
            Debug.Error(F("Can't write compressed trace data (%s bytes) : Wr.Failure : %s",
                          Int(finalLen + 1), AL.Format(x)))

          END
        END
      END
    END
  END
END Main.
