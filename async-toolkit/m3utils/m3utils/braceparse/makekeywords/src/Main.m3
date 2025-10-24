(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;
IMPORT Rd, Wr;
IMPORT FileRd;
IMPORT Params;
FROM Fmt IMPORT F;
IMPORT FileWr;
IMPORT Text;
IMPORT FS;

VAR rd  := FileRd.Open(Params.Get(1));
    oin := Params.Get(2);
    wr  : Wr.T;
    haveOld := FALSE;

    fn  := oin & ".i3";
    ofn := fn & ".old";
BEGIN
  TRY
    (* save old version of file for posterity *)
    FS.Rename(fn, ofn);
    haveOld := TRUE;
  EXCEPT ELSE END;
    
  wr := FileWr.Open(fn);
  Wr.PutText(wr, "INTERFACE ");
  Wr.PutText(wr, oin);
  Wr.PutText(wr, F(";\n\nCONST Brand = \"%s\";\n\n", oin));
  
  TRY
    LOOP
      VAR
        line := Rd.GetLine(rd);
        len  := Text.Length(line);
      BEGIN
        Wr.PutText(wr, "CONST ");
        Wr.PutText(wr, line);
        Wr.PutText(wr, "kw = ARRAY OF CHAR { ");
        FOR i := 0 TO len - 1 DO
          Wr.PutText(wr, F("'%s'", Text.Sub(line, i, 1)));
          IF i # len - 1 THEN
            Wr.PutText(wr, ", ")
          END
        END;
        Wr.PutText(wr, " };\n")
      END
    END
  EXCEPT
    Rd.EndOfFile =>
    Wr.PutText(wr, F("\nEND %s.\n", oin));
    Rd.Close(rd);
    Wr.Close(wr)
  END;

  (* now see whether the old file was the same as the new one,
     if they are the same, we copy the old file over the new one,
     which will retain the original modification time *)
  
  TRY
    IF haveOld THEN
      CONST
        BufSiz = 16 * 1024;
      VAR
        nRd := FileRd.Open(fn);
        oRd := FileRd.Open(ofn);
        nBuf, oBuf : ARRAY [0..BufSiz-1] OF CHAR;
      BEGIN
        LOOP
          WITH nRes = Rd.GetSub(nRd, nBuf),
               oRes = Rd.GetSub(oRd, oBuf) DO
            IF nRes # oRes OR
              SUBARRAY(nBuf, 0, nRes) # SUBARRAY(oBuf, 0, oRes) THEN
              Rd.Close(nRd);
              Rd.Close(oRd);
              FS.DeleteFile(ofn);
              EXIT
            ELSIF nRes = 0 THEN
              (* new and old are the same --
                 close readers & copy old file over the new,
                 retaining mod. time *)
              
              Rd.Close(nRd);
              Rd.Close(oRd);
              FS.Rename(ofn, fn); 
              EXIT
            END(*FI*)
          END(*HTIW*)
        END(*POOL*)
      END(*NIGEB*)
    END(*FI*)
  EXCEPT
  ELSE END;

END Main.
