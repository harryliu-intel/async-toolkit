MODULE TraceRewriter;

IMPORT Pathname;
IMPORT Trace, TraceRep;
IMPORT TraceOp;
IMPORT TextSeq;
IMPORT Rd;
IMPORT OSError;
IMPORT Wr;
IMPORT ZtraceFile;
IMPORT FileWr;
IMPORT Debug;
FROM Fmt IMPORT Int, F;
IMPORT SpiceCompress;
IMPORT TripleRefTbl;
IMPORT ArithConstants;
IMPORT Text;
IMPORT TraceFile;
IMPORT TextWr;
IMPORT DistZTrace;
IMPORT NameControl;
IMPORT ZtraceNodeHeader;

REVEAL
  T = Public BRANDED Brand OBJECT
    root         : Pathname.T;
    rewriterPath : Pathname.T;
    tr           : Trace.T;
    dirty        : BOOLEAN;
    scratch      : REF ARRAY OF LONGREAL;
    scratch1     : REF ARRAY OF LONGREAL;
    wr, nwr      : Wr.T;
    wfMin, wfLim : CARDINAL;
    metadata     : ZtraceFile.Metadata;
    
  OVERRIDES
    init    := Init;
    sync    := Sync;
    addhiOp := AddhiOp;
  END;

PROCEDURE Init(t : T; root : Pathname.T; rewriterPath : Pathname.T) : T
  RAISES { OSError.E, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    t.root         := root;
    t.rewriterPath := rewriterPath;
    t.tr           := NEW(Trace.T).init(root);
    t.wr           := NIL;
    t.nwr          := NIL;
    t.metadata     := t.tr.getMetadata();

    ZtraceFile.GetDataBoundaries(t.metadata.directory,
                                 t.wfMin,
                                 t.wfLim);
        
    RETURN t
  END Init;

PROCEDURE Sync(t : T) =
  BEGIN
  END Sync;

PROCEDURE OpenWr(t : T) RAISES { OSError.E } =
  BEGIN
    IF t.wr = NIL THEN
      VAR
        vers : TraceFile.Version;
        path : Pathname.T;
      BEGIN
        t.tr.getActualFormat(path, vers);
        <*ASSERT vers = TraceFile.Version.CompressedV1*>

        t.wr := FileWr.OpenAppend(path)
      END;
      t.nwr := FileWr.OpenAppend(t.root & ".names")
    END
  END OpenWr;

  
PROCEDURE AddhiOp(t            : T;
                  op           : TraceOp.T;
                  aliases      : TextSeq.T;
                  relPrec      : LONGREAL;
                  noArith      : BOOLEAN)
  RAISES { Rd.EndOfFile, Rd.Failure } =
  VAR
    code     : ArithConstants.CodeIdx;
    finalTxt : TEXT;
    finalLen : CARDINAL;
    norm     : SpiceCompress.Norm;
    textWr   : TextWr.T;
    
  BEGIN
    IF t.scratch = NIL OR NUMBER(t.scratch^) # t.tr.getSteps() THEN
      t.scratch  := NEW(REF ARRAY OF LONGREAL, t.tr.getSteps());
      t.scratch1 := NEW(REF ARRAY OF LONGREAL, t.tr.getSteps())
    END;
    op.exec(t.tr, t.scratch^);

    OpenWr(t);
    (* t.wr is open and ready *)

    Wr.Seek(t.wr, t.wfLim);
    (* seek to EOD *)

    Debug.Out("TraceRewriter.AddhiOp : wr @ " & Int(Wr.Index(t.wr)));

    textWr := NEW(TextWr.T).init();
    
    SpiceCompress.CompressArray("zdebug",
                                t.scratch^,
                                t.scratch1^,
                                relPrec,
                                FALSE,
                                textWr,
                                norm,
                                mem    := NEW(TripleRefTbl.Default).init(),
                                doDump := FALSE);
    WITH txt = TextWr.ToText(textWr),
         len = Text.Length(txt) DO
      
      (* txt is the polynomially compressed waveform *)
      
      IF noArith THEN
        code     := ArithConstants.ZeroCode;
        finalTxt := txt;
        finalLen := len
      ELSE
        finalTxt := DistZTrace.DoArithCompress(txt, code);
        finalLen := Text.Length(finalTxt)
      END;
      
      Debug.Out(F("AddhiOp : %s timesteps, compressed size %s bytes, coded size %s",
                  Int(NUMBER(t.scratch^)),
                  Int(len),
                  Int(finalLen)))
    END;

    Wr.PutText(t.wr, finalTxt);
    NameControl.PutNames(t.nwr, t.metadata.directory.size(), aliases, TRUE);

    Wr.Flush(t.wr);
    Wr.Flush(t.nwr);

    WITH dirent = ZtraceNodeHeader.T { finalLen,
                                       t.wfLim,
                                       norm,
                                       code,
                                       0 } DO
      t.dirty := TRUE;

      t.metadata.directory.addhi(dirent);

      INC(t.metadata.header.nwaves);
      INC(t.wfLim, finalLen);
    END
    

  END AddhiOp;

BEGIN END TraceRewriter.
