(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TriOsc;
IMPORT DataPointSeq;
IMPORT Oscillator;
IMPORT Triangle3List;
IMPORT P3;
IMPORT P3P3Tbl;
IMPORT TriConfig;
IMPORT TriConfigSeq;

PROCEDURE Calibrate(corner         : TEXT;
                    recs           : DataPointSeq.T;
                    READONLY temps : ARRAY OF LONGREAL) : Oscillator.T;

PROCEDURE MakeMeshes(oscs : ARRAY [0..2] OF Oscillator.T;
                     READONLY calTemps : ARRAY OF LONGREAL;
                     N    : CARDINAL) : REF ARRAY OF Mesh;

PROCEDURE Estimate(at          : P3.T;
                   tempMeshes  : REF ARRAY OF Mesh;
                   Samples     : CARDINAL;
                   k           : LONGREAL) : TriConfig.T
  RAISES { NoData };

EXCEPTION NoData;

TYPE
  Mesh = RECORD
    temp      : LONGREAL;
    triangles : Triangle3List.T;
    volttbl   : P3P3Tbl.T;
  END;


PROCEDURE EvalEstimator(tempMeshes : REF ARRAY OF Mesh;
                        Samples : CARDINAL;
                        k : LONGREAL;
                        over : TriConfigSeq.T;
                        Tests : CARDINAL) : ARRAY Dim OF DevRecord;

TYPE
  DevRecord = RECORD
    n                     : CARDINAL := 0;
    maxDev, sumAbs, sumSq : LONGREAL := 0.0d0;
  END;

  Dim = { Temperature, Voltage };

PROCEDURE FmtDev(READONLY rec : DevRecord) : TEXT;
  
CONST Brand = "TriOsc";
      
END TriOsc.
