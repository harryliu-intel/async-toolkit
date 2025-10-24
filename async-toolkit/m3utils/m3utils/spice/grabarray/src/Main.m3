(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT Params;
IMPORT Scan;
IMPORT UnsafeReader;
IMPORT FileRd;
IMPORT Rd;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT IO;

CONST LR = LongReal;
      
VAR
  path     := Params.Get(1);
  baseByte := Scan.Int(Params.Get(2));
  cnt      := Scan.Int(Params.Get(3));

  rd := FileRd.Open(path);

  arr := NEW(REF ARRAY OF LONGREAL, cnt);
  
BEGIN
  Rd.Seek(rd, baseByte);

  UnsafeReader.ReadLRA(rd, arr^);

  FOR i := FIRST(arr^) TO LAST(arr^) DO
    IO.Put(F("%s %s\n", Int(i), LR(arr[i])))
  END;

  Rd.Close(rd)
END Main.
  
