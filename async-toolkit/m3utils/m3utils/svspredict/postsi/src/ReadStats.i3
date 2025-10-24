(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ReadStats;
IMPORT LongRealSeq AS LRSeq;
IMPORT Lex, FloatMode, Rd, TextReader;

PROCEDURE DoStats(seq            : LRSeq.T;
                  VAR n          : CARDINAL;
                  VAR mean, sdev : LONGREAL);
  
PROCEDURE ReadFile(rd            : Rd.T;
                   READONLY into : ARRAY OF LRSeq.T;
                   VAR title     : TEXT)
  RAISES { Lex.Error, FloatMode.Trap, Rd.Failure, TextReader.NoMore };

END ReadStats.
