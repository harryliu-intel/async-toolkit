MODULE DataBlock;
IMPORT Wr, Rd;
IMPORT UnsafeWriter, UnsafeReader;
IMPORT Thread;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Bool, FN, LongReal;
IMPORT IntRefTbl;

<*FATAL Thread.Alerted*>
(* format assumes the readers and writers pertain to (seekable) disk files *)

CONST LR = LongReal;
      
VAR doDebug := Debug.DebugThis("DataBlock");

PROCEDURE WriteData(wr                 : Wr.T;
                    tag                : CARDINAL; (* written if nonzero *)
                    READONLY block     : ARRAY OF LONGREAL)
  RAISES { Wr.Failure } =
  BEGIN
    IF tag = 0 THEN
      UnsafeWriter.WriteLRA(wr,block)
    ELSE
      UnsafeWriter.WriteI  (wr, tag);     
      UnsafeWriter.WriteI  (wr, NUMBER(block));
      UnsafeWriter.WriteLRA(wr, block)
    END
  END WriteData;

PROCEDURE DataCount(rd : Rd.T; tag : CARDINAL) : CARDINAL
  RAISES { Rd.Failure } =
  BEGIN
    <*ASSERT tag = 0*>
    (* only call this for TIME, for now *)
    WITH pos = Rd.Index(rd) DO
      Rd.Seek(rd, LAST(CARDINAL));
      WITH len = Rd.Index(rd) DO
        Rd.Seek(rd, pos);
        RETURN len DIV 4
      END
    END
  END DataCount;

PROCEDURE ReadData(rd        : Rd.T;
                   tag       : CARDINAL; (* must match if nonzero *)
                   VAR data  : ARRAY OF LONGREAL;
                   fn        : TEXT) : CARDINAL
  RAISES { Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF tag = 0 THEN
      UnsafeReader.ReadLRA(rd, data);
      RETURN NUMBER(data)
    ELSE
      (* first read block header, whch consists of Tag and Hollerith length *)

      IF doDebug THEN
        Debug.Out(F("DataBlock.ReadData tag=%s NUMBER(data)=%s",
                    Int(tag),
                    Int(NUMBER(data))))
      END;
      
      WITH readTag = UnsafeReader.ReadI(rd),
           count   = UnsafeReader.ReadI(rd) DO

        IF doDebug THEN
          WITH bytePos = Rd.Index(rd) DO
            Debug.Out(F("DataBlock.ReadData (%s) @%sB readTag=%s match=%s count=%s",
                        fn,
                        Int(bytePos),
                        Int(readTag),
                        Bool(readTag = tag),
                        Int(count)))
          END
        END;
        
        (* decide whether to skip or not *)
        IF tag = readTag THEN
          (* correct tag, report result back *)

          IF count > NUMBER(data) THEN
            WITH bytePos = Rd.Index(rd) DO
              Debug.Error(F("DataBlock  (%s) @%sB format error : count %s > NUMBER(data) %s",
                            fn,
                            Int(bytePos),
                            Int(count), Int(NUMBER(data))))
            END
          END;

          UnsafeReader.ReadLRA(rd, SUBARRAY(data, 0, count));
          (* if we get Rd.EndOfFile here, then 
             the temp file that we are reading is corrupted *)
          
          RETURN count
        ELSE
          (* incorrect tag, just skip it *)
          WITH pos = Rd.Index(rd) DO
            Rd.Seek(rd, pos + 4 * count)
          END;
          RETURN 0
        END
      END
    END
  END ReadData;

REVEAL
  T = Public BRANDED Brand OBJECT
    rd       : Rd.T;
    maxCount : CARDINAL;
    values   : IntRefTbl.T;
  OVERRIDES
    init     := Init;
    haveTag  := HaveTag;
    readData := ReadDataM;
  END;

TYPE
  Rec = REF RECORD
    n : CARDINAL;
    a : REF ARRAY OF LONGREAL
  END;
     
PROCEDURE Init(t : T; rd : Rd.T; maxCount : CARDINAL; fn : TEXT) : T
  RAISES { Rd.Failure } =
  VAR
    ref : REFANY;
    rec : Rec;
  BEGIN
    t.rd := rd;
    t.maxCount := maxCount;
    t.values := NEW(IntRefTbl.Default).init();

    TRY
      LOOP
        WITH readTag = UnsafeReader.ReadI(rd),
             count   = UnsafeReader.ReadI(rd) DO
          
          IF doDebug THEN
            Debug.Out(F("DataBlock.ReadData readTag=%s count=%s",
                        Int(readTag),
                        Int(count)))
          END;
          
          
          IF NOT t.values.get(readTag, ref) THEN
            rec := NEW(Rec,
                       n := 0,
                       a := NEW(REF ARRAY OF LONGREAL, maxCount));
            EVAL t.values.put(readTag, rec)
          ELSE
            rec := ref
          END;
          
          IF count > NUMBER(rec.a^) - rec.n THEN
            WITH bytePos = Rd.Index(rd) DO
              Debug.Error(FN("DataBlock.Init (%s) @%sB format error : count %s > buf %s = NUMBER(rec.a^) %s - rec.n %s tag=%s",
                             ARRAY OF TEXT{
                            fn,
                            Int(bytePos),
                            Int(count), Int(NUMBER(rec.a^) - rec.n),
                            Int(NUMBER(rec.a^)),
                            Int(rec.n),
                            Int(readTag)}))
            END
          END;
          
          UnsafeReader.ReadLRA(rd, SUBARRAY(rec.a^, rec.n, count));
          (* if we get Rd.EndOfFile here, then 
             the temp file that we are reading is corrupted *)

          INC(rec.n, count)
        END
      END
    EXCEPT
      Rd.EndOfFile => (* skip *)
    END;
    RETURN t
  END Init;

PROCEDURE HaveTag(t : T; tag : CARDINAL) : BOOLEAN =
  VAR
    ref : REFANY;
  BEGIN
    RETURN t.values.get(tag, ref)
  END HaveTag;

PROCEDURE ReadDataM(t        : T;
                    tag      : CARDINAL;
                    VAR data : ARRAY OF LONGREAL) : CARDINAL =
  VAR
    ref : REFANY;
  BEGIN
    WITH hadIt = t.values.get(tag, ref) DO
      <*ASSERT hadIt*>
    END;

    WITH rec = NARROW(ref, Rec) DO
      <*ASSERT NUMBER(data) = t.maxCount*>
      data := rec.a^;
      RETURN rec.n
    END
  END ReadDataM;

PROCEDURE DebugTraverse(rd : Rd.T; fn : TEXT) RAISES { Rd.Failure } =
  BEGIN
    TRY
      LOOP
        WITH readTag = UnsafeReader.ReadI(rd),
             count   = UnsafeReader.ReadI(rd),
             bytePos = Rd.Index(rd) DO

          Debug.Out(F("DataBlock.DebugTraverse %s @ %sB readTag=%s count=%s",
                      fn,
                      Int(bytePos),
                      Int(readTag),
                      Int(count)));
          
          VAR arr := NEW(REF ARRAY OF LONGREAL, count);
          BEGIN
            UnsafeReader.ReadLRA(rd, arr^);
            Debug.Out(F("DataBlock.DebugTraverse %s @ %sB read block count %s arr[0]=%s arr[LAST(arr)]=%s",
                        fn, Int(Rd.Index(rd)), Int(count),
                        LR(arr[0]), LR(arr[LAST(arr^)])))
          END
        END
      END
    EXCEPT
      Rd.EndOfFile => Debug.Out(F("DebugTraverse %s EOF",fn))
    END
  END DebugTraverse;

  
BEGIN END DataBlock.
  
