MODULE Trace;

IMPORT TraceRep;

IMPORT Pathname;
IMPORT OSError;
IMPORT TraceHeader;
IMPORT TraceUnsafe;
IMPORT Rd, FileRd;
IMPORT TextCardTbl;
IMPORT CardTextSeqTbl;
IMPORT TextSeq;
IMPORT Debug;
IMPORT Thread;
FROM Fmt IMPORT F, Int; IMPORT Fmt;
IMPORT TextSet, TextSetDef;
IMPORT TraceFile;
IMPORT ZtraceFile;
IMPORT ArithConstants;
IMPORT UnsafeReader;
IMPORT Wx;
IMPORT ArithCode;
IMPORT TextWr;
IMPORT ArithCallback;
IMPORT TextRd;
IMPORT SpiceCompress;
IMPORT Text;
IMPORT ZtraceNodeHeader;

<*FATAL Thread.Alerted*>

VAR doDebug := TRUE;

REVEAL
  T = TraceRep.Private BRANDED Brand OBJECT
    root          : Pathname.T;
    tRd           : Rd.T;
    timeArr       : REF ARRAY OF LONGREAL := NIL;
    actualPath    : Pathname.T;
    actualVersion : TraceFile.Version;
    
  OVERRIDES
    init             := Init;
    getNodeIdx       := GetNodeIdx;
    getNodes         := GetNodes;
    getTimeData      := GetTimeData;
    getCanonicalName := GetCanonicalName;
    getAliases       := GetAliases;
    allNames         := AllNames;
    close            := Close;
    sharedTime       := SharedTime;
    getActualFormat  := GetActualFormat;
  END;

TYPE
  Reordered = T OBJECT
    h : TraceHeader.T;
  OVERRIDES
    getSteps    := GetStepsR;
    getNodeData := GetNodeDataR;
  END;

  CompressedV1 = T OBJECT
    z        : ZtraceFile.Metadata;
    time     : REF ARRAY OF LONGREAL;
    startB   : REF ARRAY OF CARDINAL;
  OVERRIDES
    getSteps    := GetStepsC;
    getNodeData := GetNodeDataC;
    getMetadata := GetMetadataC;
  END;
  
PROCEDURE Close(t : T) RAISES { Rd.Failure } =
  BEGIN
    Rd.Close(t.tRd);
    t.tRd := NIL
  END Close;
  
PROCEDURE Init(t : T; root : Pathname.T) : T
  RAISES { OSError.E, Rd.Failure, Rd.EndOfFile } =
  VAR
    tNam : Pathname.T;
    nNam := root & ".names";
    ok : BOOLEAN;
  BEGIN

    t.timeArr := NIL;
    
    t.fwdTbl := NEW(TextCardTbl.Default).init();
    t.revTbl := NEW(CardTextSeqTbl.Default).init();

    EVAL ParseNames(nNam, t.fwdTbl, t.revTbl);

    FOR i := FIRST(TraceFile.Version) TO LAST(TraceFile.Version) DO
      ok   := TRUE;
      tNam := root & "." & TraceFile.VersionSuffixes[i];

      (* look for file .. *)
      TRY
        Debug.Out(F("Trace.Init root %s attempting %s %s",
                    root, tNam, TraceFile.VersionNames[i]));
        
        t.tRd := FileRd.Open(tNam);
        Debug.Out("Trace.Init, OK : " & tNam);
        t.actualPath    := tNam;
        t.actualVersion := i;
        Debug.Out(F("Trace.Init : success with actualPath %s version %s",
                    t.actualPath,
                    TraceFile.VersionNames[t.actualVersion]));
      EXCEPT
        OSError.E =>
        Debug.Out("Trace.Init, not found : " & tNam);
        ok := FALSE
      END;

      (* attempt to parse it *)
      IF ok THEN
        <*ASSERT t.actualPath # NIL*>
        ok :=  AttemptParse[i](t);
        <*ASSERT t.actualPath # NIL*>
      ELSE
        t.tRd := NIL;
      END;
      
      IF ok THEN
        <*ASSERT t.actualPath # NIL*>
        EXIT
      ELSIF t.tRd # NIL THEN
        Rd.Close(t.tRd);
        t.tRd := NIL
      END
      
    END;

    IF ok THEN
      <*ASSERT t.actualPath # NIL*>
      Debug.Out(F("Trace.Init : success with actualPath %s version %s",
                  t.actualPath,
                  TraceFile.VersionNames[t.actualVersion]));
      RETURN t
    ELSE
      Debug.Error("Trace.Init unable to parse for trace root " & root);
      <*ASSERT FALSE*>
    END
  END Init;

PROCEDURE GetActualFormat(t                  : T;
                          VAR actualPath     : Pathname.T;
                          VAR actualVersion  : TraceFile.Version) =
  BEGIN
    actualPath := t.actualPath;
    actualVersion := t.actualVersion
  END GetActualFormat;
  
  (**********************************************************************)

TYPE Parser = PROCEDURE(VAR t : T) : BOOLEAN
  RAISES { Rd.Failure, Rd.EndOfFile };

CONST  AttemptParse = ARRAY TraceFile.Version OF Parser {
  AttemptParseUnreordered,
  AttemptParseReordered,
  AttemptParseCompressedV1,
  NIL};

PROCEDURE AttemptParseUnreordered(<*UNUSED*>VAR t : T) : BOOLEAN =
  BEGIN
    RETURN FALSE
  END AttemptParseUnreordered;

PROCEDURE AttemptParseReordered(VAR t : T) : BOOLEAN
  RAISES { Rd.Failure, Rd.EndOfFile } =
  CONST
    V = TraceFile.Version.Reordered;
  BEGIN

    WITH hh = TraceFile.ReadHeader(t.tRd) DO
      IF hh.version # V THEN
        RETURN FALSE
      END
    END;

    WITH new = NEW(Reordered,
                   fwdTbl := t.fwdTbl,
                   revTbl := t.revTbl,
                   tRd    := t.tRd,
                   root   := t.root) DO
      
    Rd.Seek(new.tRd, 0);
    
    new.h := TraceUnsafe.GetHeader(new.tRd, new.revTbl.size());

    (* need to check format *)
    
    IF doDebug THEN
      Debug.Out(F("TraceHeader start %s, end %s, steps %s, nNodes %s",
                  Int(new.h.start), Int(new.h.end), Int(new.h.steps), Int(new.h.nNodes)));
    END;
    t := new
    END;
    RETURN TRUE
  END AttemptParseReordered;

PROCEDURE AttemptParseCompressedV1(VAR t : T) : BOOLEAN =
  CONST
    V = TraceFile.Version.CompressedV1;
  VAR
    bp : CARDINAL;
  BEGIN
    WITH hh = TraceFile.ReadHeader(t.tRd) DO
      IF hh.version #  V THEN
        RETURN FALSE
      END;
      
    WITH new = NEW(CompressedV1,
                   fwdTbl        := t.fwdTbl,
                   revTbl        := t.revTbl,
                   tRd           := t.tRd,
                   root          := t.root,
                   actualPath    := t.actualPath,
                   actualVersion := t.actualVersion) DO
      

      Rd.Seek(new.tRd, 0);

      new.z     := ZtraceFile.Read(new.tRd);

      (* build start bytes for each series *)
      bp := Rd.Index(t.tRd);
      Debug.Out(F("Attempt ParseCompressedV1 start of data %s", Int(bp)));

      new.startB := NEW(REF ARRAY OF CARDINAL, new.z.header.nwaves);

      FOR i := FIRST(new.startB^) TO LAST(new.startB^) DO
        new.startB[i] := bp;
        INC(bp, new.z.directory.get(i).bytes)
      END;

      Debug.Out(F("Attempt ParseCompressedV1 end   of data %s", Int(bp)));
      
      new.time   := NEW(REF ARRAY OF LONGREAL, new.z.nsteps);

      Debug.Out(F("Trace.AttemptParseCompressedV1 nwaves %s nsteps %s",
                  Int(new.z.header.nwaves),
                  Int(new.z.nsteps)));

      Debug.Out(ZtraceFile.Format(new.z));
      
      new.getNodeData(0, new.time^);
    t := new
    END;
      RETURN TRUE
    END;
  END AttemptParseCompressedV1;

  (**********************************************************************)
  
PROCEDURE GetNodes(t : T) : CARDINAL =
  BEGIN
    RETURN t.revTbl.size()
  END GetNodes;

PROCEDURE GetCanonicalName(t : T; idx : CARDINAL) : TEXT =
  VAR
    seq : TextSeq.T;
  BEGIN
    WITH hadIt = t.revTbl.get(idx, seq) DO
      <*ASSERT hadIt*>
      
      RETURN seq.get(0)
    END
  END GetCanonicalName;

PROCEDURE GetAliases(t : T; idx : CARDINAL) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();
  BEGIN
    GetAliasesInt(t, idx, res);
    RETURN res
  END GetAliases;
  
PROCEDURE GetAliasesInt(t : T; idx : CARDINAL; res : TextSet.T) =
  VAR
    seq : TextSeq.T;
  BEGIN
    WITH hadIt = t.revTbl.get(idx, seq) DO
      <*ASSERT hadIt*>
      
      FOR i := 0 TO seq.size() - 1 DO
        EVAL res.insert(seq.get(i))
      END
    END
  END GetAliasesInt;

PROCEDURE AllNames(t : T) : TextSet.T =
  VAR
    res := NEW(TextSetDef.T).init();
  BEGIN
    FOR i := 0 TO t.revTbl.size() - 1 DO
      GetAliasesInt(t, i, res)
    END;
    RETURN res
  END AllNames;
 
PROCEDURE ParseNames(nNam   : Pathname.T;
                     fwdTbl : TextCardTbl.T;
                     revTbl : CardTextSeqTbl.T) : CARDINAL
  RAISES { OSError.E, Rd.Failure } =

  CONST
    Verbose = FALSE;

  PROCEDURE Add(seq : TextSeq.T; new : TEXT) =
    BEGIN
      IF Verbose THEN Debug.Out("Adding alias " & new) END;
      seq.addhi(new);
      WITH hadIt = fwdTbl.put(new, id) DO
        IF hadIt THEN
          Debug.Warning(nNam & " : duplicate mapping for node \"" &
            new & "\"")
        END
      END
    END Add;

  PROCEDURE GetLine() : CARDINAL
    RAISES { Rd.Failure } =
    VAR
      start := 0;
    BEGIN

      LOOP
        WITH n   = Rd.GetSubLine(rd,
                                 SUBARRAY(buff^, start, buflen - start)) DO
          IF Verbose THEN
            Debug.Out(F("GetLine n %s start %s buflen %s",
                        Int(n),
                        Int(start),
                        Int(buflen)))
          END;
          
          IF n + start = buflen THEN
            (* extend buff and read some more *)
            start  := buflen; 
            
            buflen := buflen * 2;
            WITH new    = NEW(REF ARRAY OF CHAR, buflen) DO
              SUBARRAY(new^, 0, NUMBER(buff^)) := buff^;
              buff   := new;
            END
          ELSE
            IF Verbose THEN
              Debug.Out(F("GetLine return %s", Int(n + start)))
            END;
            
            RETURN n + start
          END
        END
      END
    END GetLine;
    
  VAR
    rd := FileRd.Open(nNam);
    id := 0;
    q  : CARDINAL;

    buflen : CARDINAL := 1;
    buff              := NEW(REF ARRAY OF CHAR, buflen);

  BEGIN
    TRY
      LOOP
        
        
        WITH len    = GetLine(),
             seq    = NEW(TextSeq.T).init() DO

          IF Verbose THEN
            Debug.Out("len = " & Int(len))
          END;

          IF len = 0 THEN EXIT END;
          
          q := 0;
          FOR i := 0 TO len - 1 DO
            WITH c = buff[i] DO
              IF c = '=' THEN
                WITH new = Text.FromChars(SUBARRAY(buff^, q, i - q)) DO
                  Add(seq, new);
                  q := i + 1
                END
              END
            END
          END;

          WITH new = Text.FromChars(SUBARRAY(buff^, q, len - q)) DO
            Add(seq, new)
          END;
          
          EVAL revTbl.put(id, seq);
          INC(id)
        END
      END
    EXCEPT
      Rd.EndOfFile =>
    END;
    Rd.Close(rd);
    IF doDebug THEN
      Debug.Out(F("Trace.ParseNames : %s nodes, %s names",
                  Int(revTbl.size()), Int(fwdTbl.size())))
    END;
    RETURN id (* number of nodes *)
  END ParseNames;
  
PROCEDURE GetNodeIdx(t : T; node : TEXT; VAR idx : CARDINAL) : BOOLEAN =
  BEGIN
    RETURN t.fwdTbl.get(node, idx)
  END GetNodeIdx;
  
PROCEDURE GetTimeData(t : T; VAR timea : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Rd.EndOfFile } =
  BEGIN
    t.getNodeData(TimeId, timea)
  END GetTimeData;

(********************  Reordered methods  ********************)
  
PROCEDURE GetStepsR(t : Reordered) : CARDINAL =
  BEGIN
    RETURN t.h.steps
  END GetStepsR;

PROCEDURE GetNodeDataR(t : Reordered; idx : CARDINAL; VAR arr : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Rd.EndOfFile } =
  BEGIN
    TraceUnsafe.GetDataArray(t.tRd, t.h, idx, arr)
  END GetNodeDataR;

(********************  CompressedV1 methods  ********************)

PROCEDURE GetStepsC(t : CompressedV1) : CARDINAL =
  BEGIN
    RETURN t.z.nsteps
  END GetStepsC;

PROCEDURE GetNodeDataC(t       : CompressedV1;
                       idx     : CARDINAL;
                       VAR arr : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    dirent : ZtraceNodeHeader.T;
  BEGIN
    Debug.Out(F("Trace.GetNodeDataC : directory.size %s idx %s",
                Int(t.z.directory.size()), Int(idx)));
    
    dirent := t.z.directory.get(idx);
    (*TraceUnsafe.GetDataArray(t.tRd, t.h, idx, arr)*)
    (* here goes all the clever stuff *)

    CASE dirent.code OF
      ArithConstants.DenseCode =>
      (* dense array as in Andrew's format *)
      <* ASSERT NUMBER(arr) * 4 = dirent.bytes *>
      Rd.Seek(t.tRd, t.startB[idx]);
      UnsafeReader.ReadLRA(t.tRd, arr)
    |
      ArithConstants.LinearCode =>
      (* a linear interpolation *)
      FOR i := FIRST(arr) TO LAST(arr) DO
        WITH if    = FLOAT(i, LONGREAL),
             lf    = FLOAT(LAST(arr), LONGREAL),
             ratio = if/lf,
             range = (dirent.norm.max - dirent.norm.min),
             v     = ratio * range + dirent.norm.min
         DO
          arr[i] := v
        END
      END
    ELSE
      Rd.Seek(t.tRd, t.startB[idx]);
      WITH data = GetBytes(t.tRd, dirent.bytes),
           decoded = ArithDecode(data, dirent.code)
       DO
        Decompress(decoded, arr);
        FOR i := FIRST(arr) TO LAST(arr) DO
          WITH range = dirent.norm.max - dirent.norm.min DO
            arr[i] := arr[i] * range + dirent.norm.min
          END
        END
      END
    END
  END GetNodeDataC;

PROCEDURE GetMetadataC(c : CompressedV1) : ZtraceFile.Metadata =
  BEGIN
    RETURN c.z
  END GetMetadataC;

PROCEDURE GetBytes(rd : Rd.T; bytes : CARDINAL) : TEXT
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := 0 TO bytes - 1 DO
      WITH c = Rd.GetChar(rd) DO
        Wx.PutChar(wx, c)
      END
    END;
    RETURN Wx.ToText(wx)
  END GetBytes;

PROCEDURE ArithDecode(from : TEXT; codeIdx : ArithConstants.CodeIdx) : TEXT =
  BEGIN
    IF codeIdx = ArithConstants.ZeroCode THEN
      RETURN from
    ELSE
      WITH     ft      = ArithConstants.CodeBook[codeIdx],
               code    = NEW(ArithCode.T).init(ft),
               decoder = code.newDecoder(),
               deWr    = TextWr.New(),
               deCb    = NEW(ArithCallback.Writer).init(deWr) DO
        (* see Main.m3<spicestream>  / Main.DoArithCompress *)
        decoder.setCallback(deCb);
        decoder.text(from);
        decoder.eof();
        RETURN TextWr.ToText(deWr)
      END
    END
  END ArithDecode;

PROCEDURE Decompress(txt : TEXT; VAR a : ARRAY OF LONGREAL) =
  VAR
    deRd := TextRd.New(txt);
  BEGIN
    SpiceCompress.DecompressArray(deRd, a)
  END Decompress;

PROCEDURE SharedTime(t : T) : REF ARRAY OF LONGREAL =
  BEGIN
    IF t.timeArr = NIL THEN
      t.timeArr := NEW(REF ARRAY OF LONGREAL, t.getSteps());
      t.getTimeData(t.timeArr^)
    END;
    RETURN t.timeArr
  END SharedTime;
  
BEGIN END Trace.
