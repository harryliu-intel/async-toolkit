(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE StdfWr;
IMPORT Wr;
IMPORT Thread;
IMPORT Debug;

<*NOWARN*>VAR doDebug := Debug.DebugThis("StdfWr");

PROCEDURE U1(wr : Wr.T; READONLY x : [0..255])
  RAISES { Thread.Alerted, Wr.Failure } =
  BEGIN
    Wr.PutChar(wr, VAL(x,CHAR))
  END U1;

PROCEDURE U2(wr : Wr.T; READONLY x : [0..65535])
  RAISES { Thread.Alerted, Wr.Failure } =
  BEGIN
   (* little endian *)
    Wr.PutChar(wr, VAL(x MOD 256,CHAR));
    Wr.PutChar(wr, VAL(x DIV 256,CHAR));
  END U2;

BEGIN END StdfWr.
