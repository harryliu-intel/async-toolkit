(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE StdfU1;
IMPORT Rd, StdfE;
IMPORT Word;
IMPORT Thread;
IMPORT Wr;

CONST Bytez = 1;
      Bits  = Bytez * 8;
      
TYPE T = [0..Word.Shift(1,Bits)-1];

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile };

PROCEDURE Bytes(READONLY t : T) : CARDINAL;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure };

PROCEDURE Format(t : T) : TEXT;

CONST Brand = "StdfU1";

END StdfU1.
