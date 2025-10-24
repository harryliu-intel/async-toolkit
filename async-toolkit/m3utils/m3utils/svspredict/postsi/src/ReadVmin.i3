(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE ReadVmin;
IMPORT Lex, FloatMode, Rd;
IMPORT LongRealSeq AS LRSeq;
IMPORT TextReader;

PROCEDURE Read(rd : Rd.T;
               Unit : LONGREAL; (* unit of V input *)
               VAR n : CARDINAL;
               VAR mean, sdev : LONGREAL;
               VAR title : TEXT)
  RAISES { Lex.Error, FloatMode.Trap, Rd.Failure, TextReader.NoMore };

END ReadVmin.
