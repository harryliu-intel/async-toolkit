(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StdfR4;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT StdfConstProc;
IMPORT Wr;

CONST Bytez = 4;
      Bits  = Bytez * 8;
      
TYPE T = REAL;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Format(t : T) : TEXT;

PROCEDURE Bytes(READONLY t : T) : CARDINAL;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure };

CONST Brand = "StdfR4";

END StdfR4.
