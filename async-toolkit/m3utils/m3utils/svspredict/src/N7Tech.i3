(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE N7Tech;
IMPORT Corner;

TYPE
  Transistor = { Ulvt, Lvt, Svt };

CONST TransistorNames = ARRAY Transistor OF TEXT { "ulvt", "lvt", "svt" };

CONST TranLeakageRatio = ARRAY Transistor OF LONGREAL { 24.9d0, 5.7d0, 1.0d0 };

CONST CornerLkgRatio = ARRAY Corner.T OF LONGREAL { 1.0d0, 2.0d0, 4.0d0 };

CONST CornerSigma = ARRAY Corner.T OF LONGREAL { -3.0d0, 0.0d0, +3.0d0 };

CONST LkgTempCoeff = 0.03d0; (* leakage increase ratio per kelvin *)

CONST SvsOffset = ARRAY Corner.T OF LONGREAL { +50.0d-3, 0.0d0, -40.0d-3 };
      (* from Karthik's STA measurements *)
      
END N7Tech.
