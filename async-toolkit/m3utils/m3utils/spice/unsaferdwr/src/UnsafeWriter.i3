(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE UnsafeWriter;
IMPORT Wr;
IMPORT Thread;
IMPORT Word;

PROCEDURE WriteI(wr : Wr.T; q : INTEGER)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE WriteU64(wr : Wr.T; q : Word.T)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE WriteLRA(wr : Wr.T; READONLY q : ARRAY OF LONGREAL)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE WriteLR(wr : Wr.T; q : LONGREAL)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE WriteLRAAt(wr : Wr.T; READONLY q : ARRAY OF LONGREAL; index : CARDINAL)
  RAISES { Wr.Failure, Thread.Alerted };

END UnsafeWriter.
