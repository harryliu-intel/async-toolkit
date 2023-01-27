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
IMPORT FileRd;
IMPORT Thread;
IMPORT Matrix;

<*FATAL Thread.Alerted*>

REVEAL
  T = Public BRANDED Brand OBJECT
    root         : Pathname.T;
    rewriterPath : Pathname.T;
    tracePath    : Pathname.T;
    
    tr           : Trace.T;
    dirty        : BOOLEAN;
    scratch      : REF ARRAY OF LONGREAL;
    scratch1     : REF ARRAY OF LONGREAL;
    wr, nwr      : Wr.T;
    wfMin, wfLim : CARDINAL;
    metadata     : ZtraceFile.Metadata;
    
  OVERRIDES
    init     := Init;
    flush    := Flush;
    addhiOp  := AddhiOp;
    addhi    := Addhi;
    shareTrace := ShareTrace;
  END;

PROCEDURE ShareTrace(t : T) : Trace.T = BEGIN RETURN t.tr END ShareTrace;

PROCEDURE Init(t : T; root : Pathname.T; rewriterPath : Pathname.T) : T
  RAISES { OSError.E, Rd.Failure, Rd.EndOfFile, TraceFile.FormatError } =
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

PROCEDURE Flush(t : T)
  RAISES { Wr.Failure, OSError.E, Rd.Failure, Rd.EndOfFile, TraceFile.FormatError } =

  PROCEDURE MoveTo(i : CARDINAL; tgt : CARDINAL)
    RAISES { OSError.E, Rd.Failure, Wr.Failure } =
    VAR
      rd       := FileRd.Open(t.tracePath);
      dirent   := t.metadata.directory.get(i);
      buff     := NEW(REF ARRAY OF CHAR, dirent.bytes);
      oldStart := dirent.start;
    BEGIN
      Rd.Seek(rd, oldStart);
      WITH readBytes = Rd.GetSub(rd, buff^) DO
        <*ASSERT readBytes = dirent.bytes*>
      END;

      Rd.Close(rd);

      Wr.Seek(t.wr, tgt);
      Wr.PutString(t.wr, buff^);
      Wr.Flush(t.wr);

      dirent.start := tgt;

      t.metadata.directory.put(i, dirent);
      
      ZtraceFile.GetDataBoundaries(t.metadata.directory,
                                   t.wfMin,
                                   t.wfLim);
   
      Debug.Out(F("TraceRewriter.Flush.MoveTo : dirent @ %s -> %s. wfMin %s wfLim %s",
                  Int(oldStart), Int(tgt), Int(t.wfMin), Int(t.wfLim)))
    END MoveTo;
  
  PROCEDURE MakeSpace(limit : CARDINAL)
    RAISES { OSError.E, Rd.Failure, Wr.Failure } =
    BEGIN
      FOR i := 0 TO t.metadata.directory.size() - 1 DO
        VAR
          dirent := t.metadata.directory.get(i);
        BEGIN
          IF dirent.start < limit THEN
            Debug.Out(F("TraceRewriter.MakeSpace, moving i=%s", Int(i)));
            MoveTo(i, MAX(limit, t.wfLim));
          END
        END
      END
    END MakeSpace;
    
  BEGIN
    IF NOT t.dirty THEN RETURN END;
    
    WITH entries  = t.metadata.directory.size(),
         dirBytes = entries * ZtraceNodeHeader.SerialSize,
         dirLim   = dirBytes + t.metadata.dirStart DO
      
      Debug.Out(F("TraceRewriter.Flush : entries %s dirBytes %s dirLim %s wfMin %s",
                  Int(entries), Int(dirBytes), Int(dirLim), Int(t.wfMin)));

      IF dirLim > t.wfMin THEN
        MakeSpace(dirLim)
      END;

      Debug.Out(F("TraceRewriter.Flush : ready to write directory @ %s",
                  Int(t.metadata.dirStart)));

      Wr.Seek(t.wr, 0);

      ZtraceFile.Write(t.wr, t.metadata);

      Wr.Flush(t.wr);
      
      Debug.Out(F("TraceRewriter.Flush : wrote directory, stopped @ %s",
                  Int(Wr.Index(t.wr))));

      (* set version back *)
      UpdateFileVersion(t, TraceFile.Version.CompressedV1);
      
      (* close writers *)
      Wr.Close(t.wr);
      t.wr := NIL;
      Wr.Close(t.nwr);
      t.nwr := NIL;

      (* and mark ourselves clean *)
      t.dirty := FALSE;
    END
  END Flush;

PROCEDURE UpdateFileVersion(t : T; to : TraceFile.Version)
  RAISES { OSError.E, Rd.EndOfFile, Rd.Failure, TraceFile.FormatError, Wr.Failure } =
  VAR
    hdr  : TraceFile.Header;
  BEGIN
    WITH origIdx = Wr.Index(t.wr),
         rd      = FileRd.Open(t.tracePath)
     DO
      hdr := TraceFile.ReadHeader(rd);
      
      hdr.version := to;
      
      (* rewind write handle and write modified header *)
      Wr.Seek(t.wr, 0);
      TraceFile.WriteHeader(t.wr, hdr);
      Wr.Flush(t.wr);
      
      (* go back to EOF *)
      Wr.Seek(t.wr, origIdx);
      
      (* and we're done with rd *)
      Rd.Close(rd);
    END
  END UpdateFileVersion;
  
PROCEDURE OpenWr(t : T) RAISES { Rd.EndOfFile, Rd.Failure, TraceFile.FormatError, Wr.Failure, OSError.E } =
  BEGIN
    IF t.wr = NIL THEN
      VAR
        vers : TraceFile.Version;
      BEGIN
        t.tr.getActualFormat(t.tracePath, vers);
        <*ASSERT vers = TraceFile.Version.CompressedV1*>

        t.wr := FileWr.OpenAppend(t.tracePath);

        (* now change version to Modifying *)
        UpdateFileVersion(t, TraceFile.Version.Modifying)
        
      END;
      t.nwr := FileWr.OpenAppend(t.root & ".names")
    END
  END OpenWr;

PROCEDURE Addhi(t               : T;
                stream          : TEXT;
                norm            : SpiceCompress.Norm;
                code            : ArithConstants.Encoding;
                aliases         : TextSeq.T) : CARDINAL
  RAISES { TraceFile.FormatError, OSError.E, Rd.EndOfFile, Rd.Failure, Wr.Failure } =
  VAR
    finalLen := Text.Length(stream);
  BEGIN
    OpenWr(t);
    (* t.wr is open and ready *)

    Wr.Seek(t.wr, t.wfLim);
    (* seek to EOD *)

    Debug.Out("TraceRewriter.Addhi : wr @ " & Int(Wr.Index(t.wr)));

    Wr.PutText(t.wr, stream);

    NameControl.PutNames(t.nwr, t.metadata.directory.size(), aliases, TRUE);
    
    Debug.Out(F("TraceRewriter.Addhi : writing stopped @ %s",
                Int(Wr.Index(t.wr))));
    
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

      NARROW(t.tr, TraceRep.CompressedV1).z := t.metadata;

      RETURN t.metadata.directory.size() - 1
    END

  END Addhi;

          
PROCEDURE AddhiOp(t            : T;
                  op           : TraceOp.T;
                  aliases      : TextSeq.T;
                  relPrec      : LONGREAL;
                  encoding     : ArithConstants.Encoding) : CARDINAL
  RAISES { TraceFile.FormatError, OSError.E, Rd.EndOfFile, Rd.Failure, Wr.Failure, Matrix.Singular } =
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
      
      IF encoding = ArithConstants.ZeroCode THEN
        code     := ArithConstants.ZeroCode;
        finalTxt := txt;
        finalLen := len
      ELSE
        code := encoding;
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

    Debug.Out(F("TraceRewriter.AddhiOp : writing stopped @ %s",
                Int(Wr.Index(t.wr))));
    
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
      
      RETURN t.metadata.directory.size() - 1
    END
    

  END AddhiOp;

BEGIN END TraceRewriter.
