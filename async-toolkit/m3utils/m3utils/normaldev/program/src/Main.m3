(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Main;

(* 

   Generate N normal deviates ~ N(mu, sigma)

   Usage: 

   $0 <mu> <sigma> <N>

*)

IMPORT NormalDeviate;
IMPORT Random;
IMPORT Wr;
IMPORT Stdio;
IMPORT Params;
IMPORT Scan;
IMPORT Fmt;

VAR
  mean := Scan.LongReal(Params.Get(1));
  sdev := Scan.LongReal(Params.Get(2));
  n    := Scan.Int     (Params.Get(3));
  rand := NEW(Random.Default).init();
  
BEGIN
  FOR i := 0 TO n - 1 DO
    WITH x = NormalDeviate.Get(rand, mean, sdev) DO
      Wr.PutText(Stdio.stdout, Fmt.LongReal(x));
      Wr.PutChar(Stdio.stdout, '\n')
    END
  END
END Main.
