(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE StdfU4;
IMPORT Rd, StdfE;
IMPORT Thread;
FROM Fmt IMPORT Int;
IMPORT StdfRd;
IMPORT Wr, StdfWr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF len = 0 THEN RETURN END;
    t :=     StdfRd.U1(rd, len);
    t := t + StdfRd.U1(rd, len) * 256;
    t := t + StdfRd.U1(rd, len) * 256 * 256;
    t := t + StdfRd.U1(rd, len) * 256 * 256 * 256;
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Int(t)
  END Format;

PROCEDURE Write(wr : Wr.T; READONLY u : T)
  RAISES { Thread.Alerted, Wr.Failure } =
  VAR
    t := u;
  BEGIN
    StdfWr.U1(wr, t MOD 256);
    t := t DIV 256;
    StdfWr.U1(wr, t MOD 256);
    t := t DIV 256;
    StdfWr.U1(wr, t MOD 256);
    t := t DIV 256;
    StdfWr.U1(wr, t MOD 256)
  END Write;

PROCEDURE Bytes(<*UNUSED*>READONLY t : T) : CARDINAL = BEGIN RETURN 4 END Bytes;

BEGIN END StdfU4.
