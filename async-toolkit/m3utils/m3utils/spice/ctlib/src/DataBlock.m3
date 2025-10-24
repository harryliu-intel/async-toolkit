(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE DataBlock;
IMPORT Wr, Rd;
IMPORT UnsafeWriter, UnsafeReader;
IMPORT Thread, ThreadF;
IMPORT Debug;
FROM Fmt IMPORT F, Int, Bool, FN, LongReal;
IMPORT IntRefTbl;
IMPORT Text;

<*FATAL Thread.Alerted*>
(* format assumes the readers and writers pertain to (seekable) disk files *)

CONST LR = LongReal;

TYPE TA = ARRAY OF TEXT;
      
VAR doDebug := Debug.DebugThis("DataBlock");

TYPE FileFormat = CARDINAL;
     
CONST ArrayFormat      : FileFormat  = 1;
      CompressedFormat : FileFormat  = 2;
    
PROCEDURE WriteData(wr                 : Wr.T;
                    tag                : CARDINAL; (* written if nonzero *)
                    READONLY block     : ARRAY OF LONGREAL)
  RAISES { Wr.Failure } =
  BEGIN
    IF tag = 0 THEN
      UnsafeWriter.WriteLRA(wr, block)
    ELSE
      UnsafeWriter.WriteI  (wr, ArrayFormat);
      UnsafeWriter.WriteI  (wr, tag);
      UnsafeWriter.WriteI  (wr, ThreadF.MyId());
      UnsafeWriter.WriteI  (wr, 4*NUMBER(block));
      UnsafeWriter.WriteLRA(wr, block)
    END
  END WriteData;

PROCEDURE WriteCompressed(wr                 : Wr.T;
                          tag                : CARDINAL; (* written if nonzero *)
                          READONLY data      : ARRAY OF TEXT)
  RAISES { Wr.Failure } =
  VAR
    len := 0;
  BEGIN
    <*ASSERT tag # 0*>

    FOR i := FIRST(data) TO LAST(data) DO
      INC(len, Text.Length(data[i]))
    END;

    UnsafeWriter.WriteI  (wr, CompressedFormat);
    UnsafeWriter.WriteI  (wr, tag);
    UnsafeWriter.WriteI  (wr, ThreadF.MyId());
    UnsafeWriter.WriteI  (wr, len);
    FOR i := FIRST(data) TO LAST(data) DO
      Wr.PutText           (wr, data[i])
    END

  END WriteCompressed;
  
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
  VAR
    readFormat : FileFormat;
  BEGIN
    IF tag = 0 THEN
      UnsafeReader.ReadLRA(rd, data);
      RETURN NUMBER(data)
    ELSE
      (* first read block header, whch consists of Tag and Hollerith length *)

      readFormat := UnsafeReader.ReadI(rd);
      
      IF doDebug THEN
        Debug.Out(F("DataBlock.ReadData tag=%s NUMBER(data)=%s",
                    Int(tag),
                    Int(NUMBER(data))))
      END;
      
      WITH readTag  = UnsafeReader.ReadI(rd),
           threadId = UnsafeReader.ReadI(rd),
           bytes    = UnsafeReader.ReadI(rd) DO

        IF doDebug THEN
          WITH bytePos = Rd.Index(rd) DO
            Debug.Out(FN("DataBlock.ReadData (%s) @%sB readTag=%s writeThread=%s match=%s bytes=%s",
                        TA {fn,
                        Int(bytePos),
                        Int(readTag),
                        Int(threadId),
                        Bool(readTag = tag),
                        Int(bytes) } ))
          END
        END;
        
        (* decide whether to skip or not *)
        IF tag = readTag THEN
          (* correct tag, report result back *)
          IF readFormat # ArrayFormat THEN
            WITH bytePos = Rd.Index(rd) DO
              Debug.Error(F("DataBlock (%s) @%sB format error : file format %s not array",
                            fn, Int(bytePos), Int(readFormat)))
            END
          END;

          IF bytes DIV 4 > NUMBER(data) THEN
            WITH bytePos = Rd.Index(rd) DO
              Debug.Error(F("DataBlock  (%s) @%sB format error : bytes %s > 4 * NUMBER(data) %s",
                            fn,
                            Int(bytePos),
                            Int(bytes), Int(4 * NUMBER(data))))
            END
          END;
          IF bytes MOD 4 # 0 THEN
            WITH bytePos = Rd.Index(rd) DO
              Debug.Error(F("DataBlock  (%s) @%sB format error : bytes (%s) MOD 4 # 0",
                            fn,
                            Int(bytePos),
                            Int(bytes)))
            END
          END;

          UnsafeReader.ReadLRA(rd, SUBARRAY(data, 0, bytes DIV 4));
          (* if we get Rd.EndOfFile here, then 
             the temp file that we are reading is corrupted *)
          
          RETURN bytes DIV 4
        ELSE
          (* incorrect tag, just skip it *)
          WITH pos = Rd.Index(rd) DO
            Rd.Seek(rd, pos + bytes)
          END;
          RETURN 0
        END
      END
    END
  END ReadData;

PROCEDURE ReadCompressed(rd        : Rd.T;
                         tag       : CARDINAL; (* must match if nonzero *)
                         fn        : TEXT) : TEXT
  RAISES { Rd.Failure, Rd.EndOfFile } =
  VAR
    readFormat : FileFormat;
  BEGIN
    <*ASSERT tag # 0*>
    
    (* first read block header, whch consists of Tag and Hollerith length *)

    readFormat := UnsafeReader.ReadI(rd);
    
    IF doDebug THEN
      Debug.Out(F("DataBlock.ReadCompressed tag=%s",
                  Int(tag)))
    END;
    
    WITH readTag  = UnsafeReader.ReadI(rd),
         threadId = UnsafeReader.ReadI(rd),
         bytes    = UnsafeReader.ReadI(rd) DO

      IF doDebug THEN
        WITH bytePos = Rd.Index(rd) DO
          Debug.Out(FN("DataBlock.ReadData (%s) @%sB readTag=%s writeThread=%s match=%s bytes=%s",
                       TA {fn,
                           Int(bytePos),
                           Int(readTag),
                           Int(threadId),
                           Bool(readTag = tag),
                           Int(bytes) } ))
        END
      END;
      
      (* decide whether to skip or not *)
      IF tag = readTag THEN
        (* correct tag, report result back *)
        IF readFormat # CompressedFormat THEN
          WITH bytePos = Rd.Index(rd) DO
            
            Debug.Error(F("DataBlock (%s) @%sB format error : file format %s not compressed",
                          fn, Int(bytePos), Int(readFormat)))
          END
        END;

        VAR
          chars := NEW(REF ARRAY OF CHAR, bytes);
        BEGIN
          WITH rb = Rd.GetSub(rd, chars^) DO
            (* if we get Rd.EndOfFile here, then 
               the temp file that we are reading is corrupted *)
            IF rb < bytes THEN
              RAISE Rd.EndOfFile
            END
          END;
          RETURN Text.FromChars(chars^)
        END
      ELSE
        (* incorrect tag, just skip it *)
        WITH pos = Rd.Index(rd) DO
          Rd.Seek(rd, pos + bytes)
        END;
        RETURN NIL
      END
    END
  END ReadCompressed;

REVEAL
  T = Public BRANDED Brand OBJECT
    rd       : Rd.T;
    maxCount : CARDINAL;
    values   : IntRefTbl.T;
  OVERRIDES
    init           := Init;
    haveTag        := HaveTag;
    readData       := ReadDataM;
    readCompressed := ReadCompressedM;
    blockType      := BlockTypeM;
  END;

TYPE
  Rec = OBJECT
  END;

  ArrayRec = Rec OBJECT
    n : CARDINAL;
    a : REF ARRAY OF LONGREAL;
  END;

  CompressedRec = Rec OBJECT
    data : TEXT;
  END;
     
PROCEDURE Init(t : T; rd : Rd.T; maxCount : CARDINAL; fn : TEXT) : T
  RAISES { Rd.Failure } =
  VAR
    ref : REFANY;
    rec : ArrayRec;
    format : FileFormat;
  BEGIN
    t.rd        := rd;
    t.maxCount  := maxCount;
    t.values    := NEW(IntRefTbl.Default).init();

    TRY
      LOOP
        format := UnsafeReader.ReadI(rd);
        WITH readTag  = UnsafeReader.ReadI(rd),
             threadId = UnsafeReader.ReadI(rd),
             bytes    = UnsafeReader.ReadI(rd) DO
          CASE format OF
            ArrayFormat =>

            WITH count    = bytes DIV 4 DO
              
              <*ASSERT bytes MOD 4 = 0 *>
              IF doDebug THEN
                Debug.Out(FN("DataBlock.Init readTag=%s writeThread=%s bytes=%s count=%s",
                             TA{Int(readTag),
                                Int(threadId),
                                Int(bytes),
                                Int(count)}))
              END;
              
              
              IF NOT t.values.get(readTag, ref) THEN
                rec := NEW(ArrayRec,
                           n := 0,
                           a := NEW(REF ARRAY OF LONGREAL, maxCount));
                EVAL t.values.put(readTag, rec)
              ELSE

                (* if we have ONE ArrayRec for a given tag, we should have
                   all ArrayRecs for that tag! *)
                
                <*ASSERT ISTYPE(ref, ArrayRec)*>
                
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
          |
            CompressedFormat =>
            VAR
              chars := NEW(REF ARRAY OF CHAR, bytes);
            BEGIN
              WITH rb = Rd.GetSub(rd, chars^) DO
                (* if we get Rd.EndOfFile here, then 
                   the temp file that we are reading is corrupted *)
                IF rb < bytes THEN
                  RAISE Rd.EndOfFile
                END
              END;
              EVAL t.values.put(readTag,
                                NEW(CompressedRec,
                                    data := Text.FromChars(chars^)))
            END
          ELSE
            <*ASSERT FALSE*>
          END
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

    WITH rec = NARROW(ref, ArrayRec) DO
      <*ASSERT NUMBER(data) = t.maxCount*>
      data := rec.a^;
      RETURN rec.n
    END
  END ReadDataM;

PROCEDURE ReadCompressedM(t        : T;
                          tag      : CARDINAL) : TEXT =
  VAR
    ref : REFANY;
  BEGIN
    WITH hadIt = t.values.get(tag, ref) DO
      <*ASSERT hadIt*>
    END;

    WITH rec = NARROW(ref, CompressedRec) DO
      RETURN rec.data
    END
  END ReadCompressedM;

PROCEDURE BlockTypeM(t        : T;
                     tag      : CARDINAL) : BlockType =
  VAR
    ref : REFANY;
  BEGIN
    WITH hadIt = t.values.get(tag, ref) DO
      <*ASSERT hadIt*>
    END;

    TYPECASE ref OF
      ArrayRec => RETURN BlockType.Array
    |
      CompressedRec => RETURN BlockType.Compressed
    ELSE
      <*ASSERT FALSE*>
    END;
  END BlockTypeM;

PROCEDURE DebugTraverse(rd : Rd.T; fn : TEXT) RAISES { Rd.Failure } =
  BEGIN
    TRY
      LOOP
        WITH readTag = UnsafeReader.ReadI(rd),
             threadId= UnsafeReader.ReadI(rd),
             count   = UnsafeReader.ReadI(rd),
             bytePos = Rd.Index(rd) DO

          Debug.Out(FN("DataBlock.DebugTraverse %s @ %sB readTag=%s threadId=%s count=%s",
                      TA{fn,
                      Int(bytePos),
                      Int(readTag),
                      Int(threadId),
                      Int(count)}));
          
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
  
