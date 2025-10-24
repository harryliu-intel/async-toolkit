(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;

(* 

   Capture histogram / entropy data on binary byte streams

   mika.nystroem@intel.com 
   January 2023

*)

IMPORT Stdio;
IMPORT Scan;
IMPORT Params;
IMPORT Rd;
IMPORT FileWr;
IMPORT IO;
IMPORT Wr;
IMPORT Math;
FROM Fmt IMPORT F, Int, LongReal;
IMPORT Word;

CONST LR = LongReal;

VAR
  rd      := Stdio.stdin;
  bytes   := Scan.Int(Params.Get(1));
  buf     := NEW(REF ARRAY OF CHAR, bytes);
  counts  := NEW(REF ARRAY OF CARDINAL, Word.Shift(1, bytes * 8));
  
BEGIN

  LOOP
    WITH num = Rd.GetSub(rd, buf^) DO
      IF num < bytes THEN EXIT END;

      VAR
        ptr := 0;
      BEGIN
        FOR i := bytes - 1 TO 0 BY -1 DO
          ptr := ptr * 256 + ORD(buf[i])
        END;
        INC(counts[ptr])
      END
      
    END
  END;

  VAR
    sum   := 0;
    sumsq := 0;
    h     := 0.0d0;
    log2  := Math.log(2.0d0);

    hi : LONGREAL;
    
  BEGIN
    
    FOR i := FIRST(counts^) TO LAST(counts^) DO
      INC(sum,   counts[i]);
      INC(sumsq, counts[i] * counts[i])
    END;


    WITH wr = FileWr.Open("histo.dat") DO
      FOR i := FIRST(counts^) TO LAST(counts^) DO
        WITH pr = FLOAT(counts[i], LONGREAL) / FLOAT(sum, LONGREAL) DO
          IF pr = 0.0d0 THEN
            hi := 0.0d0
          ELSE
            hi := -pr * Math.log(pr) / log2
          END;
          h := h + hi;
          
          Wr.PutText(wr, F("%s %s %s\n", Int(i), Int(counts[i]), LR(hi)))
        END;
      END;
      Wr.Close(wr)
    END;

    WITH bucks  = FLOAT(NUMBER(counts^), LONGREAL),
         mean   = FLOAT(sum, LONGREAL) / bucks,
         meanSq = FLOAT(sumsq, LONGREAL) / bucks,
         var    = meanSq - mean * mean,
         varD   = bucks / (bucks - 1.0d0) * var,
         sdevD  = Math.sqrt(varD) DO
      IO.Put(F("%s buckets : tot. count %s\n", Int(NUMBER(counts^)), Int(sum)));
      IO.Put(F("ave %s sdev %s entropy %s\n", LR(mean), LR(sdevD), LR(h)))
    END
  END
  


END Main.
