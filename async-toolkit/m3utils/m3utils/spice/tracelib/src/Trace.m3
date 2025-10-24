(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

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
IMPORT ArithCode;
IMPORT TextWr;
IMPORT ArithCallback;
IMPORT TextRd;
IMPORT SpiceCompress;
IMPORT Text;
IMPORT ZtraceNodeHeader;
IMPORT PolySegment16Serial;
FROM TraceUnsafe IMPORT GetBytes;
IMPORT Pickle;

<*FATAL Thread.Alerted*>

VAR doDebug := Debug.DebugThis("Trace");

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
    getMaxVal        := GetMaxVal;
    getMinVal        := GetMinVal;
    getMeanVal        := GetMeanVal;
  END;

TYPE
  Reordered = T OBJECT
    h : TraceHeader.T;
  OVERRIDES
    getSteps        := GetStepsR;
    getNodeData     := GetNodeDataR;
    getNodeDataType := GetNodeDataTypeR;
  END;

TYPE CompressedV1 = TraceRep.CompressedV1;
     
REVEAL
  TraceRep.CompressedV1 = TraceRep.PrivateCompressedV1 BRANDED Brand & " CompressedV1" OBJECT
    time     : REF ARRAY OF LONGREAL;
  OVERRIDES
    getSteps        := GetStepsC;
    getNodeData     := GetNodeDataC;
    getNodeDataType := GetNodeDataTypeC;
    getNodePickle   := GetNodePickleC;
    getMetadata     := GetMetadataC;
  END;
  
PROCEDURE Close(t : T) RAISES { Rd.Failure } =
  BEGIN
    Rd.Close(t.tRd);
    t.tRd := NIL
  END Close;
  
PROCEDURE Init(t : T; root : Pathname.T) : T
  RAISES { OSError.E, Rd.Failure, Rd.EndOfFile, TraceFile.FormatError } =
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
        IF doDebug THEN
          Debug.Out(F("Trace.Init root %s attempting %s %s",
                      root, tNam, TraceFile.VersionNames[i]))
        END;
          
        t.tRd := FileRd.Open(tNam);
        IF doDebug THEN
          Debug.Out("Trace.Init, OK : " & tNam)
        END;
        t.actualPath    := tNam;
        t.actualVersion := i;
        IF doDebug THEN
          Debug.Out(F("Trace.Init : success with actualPath %s version %s",
                      t.actualPath,
                      TraceFile.VersionNames[t.actualVersion]))
        END;
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
      IF doDebug THEN
        Debug.Out(F("Trace.Init : success with actualPath %s version %s",
                    t.actualPath,
                    TraceFile.VersionNames[t.actualVersion]))

      END;
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
  RAISES { Rd.Failure, Rd.EndOfFile, TraceFile.FormatError };

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
  RAISES { Rd.Failure, Rd.EndOfFile, TraceFile.FormatError } =
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
                   root   := t.root,
                     actualPath    := t.actualPath,
                     actualVersion := t.actualVersion) DO
      
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

PROCEDURE AttemptParseCompressedV1(VAR t : T) : BOOLEAN
  RAISES { Rd.EndOfFile, Rd.Failure, TraceFile.FormatError } =
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

        IF doDebug THEN
          Debug.Out(F("Attempt ParseCompressedV1 start of data %s", Int(bp)));
          
        END;
        
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
          
          IF buff[len - 1] # '\n' THEN
            Debug.Error(F("line %s : no NL : \"%s\"!",
                          Int(id + 1),
                          Text.FromChars(buff^)))
          END;
          
          q := 0;
          FOR i := 0 TO len - 2 DO
            WITH c = buff[i] DO
              IF c = '=' THEN
                WITH new = Text.FromChars(SUBARRAY(buff^, q, i - q)) DO
                  Add(seq, new);
                  q := i + 1
                END
              END
            END
          END;

          WITH new = Text.FromChars(SUBARRAY(buff^, q, len - q - 1)) DO
            Add(seq, new)
          END;
          
          EVAL revTbl.put(id, seq);
          INC(id)
        END
      END
    EXCEPT
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

PROCEDURE GetNodeDataTypeR(<*UNUSED*>t : Reordered;
                           <*UNUSED*>idx : NodeId) : DataType =
  BEGIN
    RETURN DataType.Array
  END GetNodeDataTypeR;

(********************  CompressedV1 methods  ********************)

PROCEDURE GetStepsC(t : CompressedV1) : CARDINAL =
  BEGIN
    RETURN t.z.nsteps
  END GetStepsC;

PROCEDURE GetNodeDataTypeC(t   : CompressedV1;
                           idx : NodeId) : DataType =
  VAR
    dirent : ZtraceNodeHeader.T;
  BEGIN
    dirent := t.z.directory.get(idx);

    CASE dirent.code OF
      ArithConstants.Pickle => RETURN DataType.Pickle
    ELSE
      RETURN DataType.Array
    END
  END GetNodeDataTypeC;
  
PROCEDURE GetNodeDataC(t       : CompressedV1;
                       idx     : CARDINAL;
                       VAR arr : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    dirent : ZtraceNodeHeader.T;
  BEGIN
    
    dirent := t.z.directory.get(idx);

    IF doDebug THEN
      Debug.Out(F("Trace.GetNodeDataC : directory.size %s idx %s startB %s",
                  Int(t.z.directory.size()),
                  Int(idx),
                  Int(dirent.start)))
    END;

    CASE dirent.code OF
      ArithConstants.DenseCode =>
      (* dense array as in Andrew's format *)
      <* ASSERT NUMBER(arr) * 4 = dirent.bytes *>
      Rd.Seek(t.tRd, dirent.start);
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
    |
      ArithConstants.Pickle =>
      (* wrong place to be reading *)
      Debug.Error("Trace.GetNodeDataC : Attempt to get waveform from a Pickle object")
    ELSE
      Rd.Seek(t.tRd, dirent.start);
      WITH data    = GetBytes(t.tRd, dirent.bytes),
           decoded = ArithDecode(data, dirent.code)
       DO
        TRY
          Decompress(decoded, arr);
        EXCEPT
          PolySegment16Serial.Error =>
          Debug.Error("Bad format in GetNodeDataC");
          <*ASSERT FALSE*>
        END;
        FOR i := FIRST(arr) TO LAST(arr) DO
          WITH range = dirent.norm.max - dirent.norm.min DO
            arr[i] := arr[i] * range + dirent.norm.min
          END
        END
      END
    END
  END GetNodeDataC;

PROCEDURE GetNodePickleC(t       : CompressedV1;
                         idx     : CARDINAL) : REFANY
  RAISES { Rd.Failure, Rd.EndOfFile, Pickle.Error } =
  VAR
    dirent : ZtraceNodeHeader.T;
  BEGIN
    dirent := t.z.directory.get(idx);

    IF doDebug THEN
      Debug.Out(F("Trace.GetNodePickleC : directory.size %s idx %s startB %s",
                  Int(t.z.directory.size()),
                  Int(idx),
                  Int(dirent.start)))
    END;
    CASE dirent.code OF
      ArithConstants.Pickle =>
      Rd.Seek(t.tRd, dirent.start);
      RETURN Pickle.Read(t.tRd)
    ELSE
      (* actually what we could do is allocate a new array and call
         GetNodeDataC and return that as a sort of generic response... *)
      Debug.Error("Trace.GetPickleC : Attempt to get pickle from a waveform Array object");
      <*ASSERT FALSE*>
    END
  END GetNodePickleC;

PROCEDURE GetMetadataC(c : CompressedV1) : ZtraceFile.Metadata =
  BEGIN
    RETURN c.z
  END GetMetadataC;

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

PROCEDURE Decompress(txt : TEXT; VAR a : ARRAY OF LONGREAL)
  RAISES { PolySegment16Serial.Error, Rd.EndOfFile, Rd.Failure } =
  VAR
    deRd := TextRd.New(txt);
  BEGIN
    SpiceCompress.DecompressArray(deRd, a)
  END Decompress;

PROCEDURE SharedTime(t : T) : REF ARRAY OF LONGREAL
  RAISES { Rd.EndOfFile, Rd.Failure } =
  BEGIN
    IF t.timeArr = NIL THEN
      t.timeArr := NEW(REF ARRAY OF LONGREAL, t.getSteps());
      t.getTimeData(t.timeArr^)
    END;
    RETURN t.timeArr
  END SharedTime;

PROCEDURE GetMaxVal(t : T; idx : NodeId) : LONGREAL
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    arr := NEW(REF ARRAY OF LONGREAL, t.getSteps());
    res := FIRST(LONGREAL);
  BEGIN
    t.getNodeData(idx, arr^);
    FOR i := FIRST(arr^) TO LAST(arr^) DO
      res := MAX(res, arr[i])
    END;
    RETURN res
  END GetMaxVal;
  
PROCEDURE GetMinVal(t : T; idx : NodeId) : LONGREAL
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    arr := NEW(REF ARRAY OF LONGREAL, t.getSteps());
    res := FIRST(LONGREAL);
  BEGIN
    t.getNodeData(idx, arr^);
    FOR i := FIRST(arr^) TO LAST(arr^) DO
      res := MIN(res, arr[i])
    END;
    RETURN res
  END GetMinVal;

PROCEDURE GetMeanVal(t : T; idx : NodeId) : LONGREAL
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    arr := NEW(REF ARRAY OF LONGREAL, t.getSteps());
    res := 0.0d0;
  BEGIN
    t.getNodeData(idx, arr^);
    FOR i := FIRST(arr^) TO LAST(arr^) DO
      res := res + arr[i]
    END;
    RETURN res/FLOAT(NUMBER(arr^), LONGREAL)
  END GetMeanVal;
  
BEGIN END Trace.
