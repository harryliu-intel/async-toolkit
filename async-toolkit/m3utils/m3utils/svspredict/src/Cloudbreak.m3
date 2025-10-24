(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Cloudbreak;
FROM SvsTypes IMPORT CornerData;
IMPORT Corner;
IMPORT Power;

PROCEDURE SetProgram(VAR p                       : Power.Params;
                     VAR Trunc                   : LONGREAL) =
  BEGIN
    p.c := ARRAY Corner.T OF CornerData {
    CornerData { 0.710d0, 0.760d0, +3.0d0 },
    CornerData { 0.660d0, 0.710d0,  0.0d0 },
    CornerData { 0.620d0, 0.670d0, -3.0d0 } };
    
    (* from the Karthik/Mika/Julianne Excel 2020WW47 *)
    p.RefP          := 388.6d0;
    p.FixedP        :=  81.8d0;
    p.RefLeakP      :=  21.1d0;
    
    p.LkgRatio      :=   2.0d0; (* how much does leakage vary over corner *)
    p.LkgRatioSigma :=   3.0d0;
    Trunc         := 3.0d0; (* where to truncate the distribution at sort *)
  END SetProgram;

BEGIN END Cloudbreak.
