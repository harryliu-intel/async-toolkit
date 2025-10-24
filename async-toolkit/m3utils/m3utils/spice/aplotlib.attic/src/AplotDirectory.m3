(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE AplotDirectory;
IMPORT Wr;

TYPE
  Offset =  [-1 .. LAST(CARDINAL) ];

  FileId = Offset;

REVEAL
  Ragged = BRANDED DefRagged OBJECT
    offsets : REF ARRAY OF Offset := NIL;
    fileIds : REF ARRAY OF FileId := NIL;
  OVERRIDES
    new := NewRagged;
    write := WriteRagged;
    read := ReadRagged;
    getNodeOffset := OffsetRagged;
  END;

  Null = BRANDED DefNull OBJECT
  OVERRIDES
    write := WriteNull;
    read := ReadNull;
    getNodeOffset := OffsetNull;
  END;

PROCEDURE NewRagged(rag : Ragged; numNodes : CARDINAL) : Ragged =
  BEGIN
    rag.offsets ;= NEW(REF ARRAY OF Offset, numNodes  + 1);
    FOR i := FIRST(rag.offsets^) TO LAST(rag.offsets^) DO
      rag.offsets[i] := -1
    END;

    IF (* format is multi-file *) THEN
      rag.fileIds ;= NEW(REF ARRAY OF FileId, numNodes  + 1);
      FOR i := FIRST(rag.fileIds^) TO LAST(rag.fileIds^) DO
        rag.fileIds[i] := -1
      END;
    END
  END NewRagged;
  
PROCEDURE WriteRagged(t : Ragged; to : Wr.T) =
  VAR
    index := Wr.Index(to);
  BEGIN
    FOR i := FIRST(t.offsets^) TO LAST(t.offsets^) DO
      IF t.multiFile THEN
        WrUtils.PutLE32(wr, rag.fileIds[i])
      END;
      WrUtils.PutLE64(wr, rag.offsets[i])
    END;
    WrUtils.PutLE64(wr, index);
  END WriteRagged;

PROCEDURE ReadRagged(t : Ragged; fr : Rd.T) =
  VAR
    index : CARDINAL;
  BEGIN
    Rd.Seek(rd, 0, End);
    end := Rd.Index(rd);
    Rd.Seek(rd, end - 8);
    index := RdUtils.GetLE64(rd);
    Rd.Seek(rd, index);
    
    FOR i := FIRST(t.offsets^) TO LAST(t.offsets^) DO
      IF t.multiFile THEN
        rag.fileIds[i] := RdUtils.GetLE32(rd)
      END;
      rag.offsets[i] := RdUtils.GetLE64(rd)
    END;
    WrUtils.PutLE64(index);
  END WriteRagged;

PROCEDURE WriteNull(<*UNUSED*>t : Null; <*UNUSED*>wr : Wr.T) =
  BEGIN END WriteNull;

PROCEDURE ReadNull(<*UNUSED*>t : Null; <*UNUSED*>rd : Rd.T) =
  BEGIN END ReadNull;

BEGIN END AplotDirectory.
