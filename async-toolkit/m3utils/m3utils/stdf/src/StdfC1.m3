(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE StdfC1;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Text;
IMPORT StdfRd;
IMPORT Wr, StdfWr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  BEGIN
    IF len = 0 THEN RETURN END;
    t[0] := StdfRd.Char(rd, len)
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Text.FromChar(t[0])
  END Format;

PROCEDURE Bytes(<*UNUSED*>READONLY t : T) : CARDINAL = BEGIN RETURN 1 END Bytes;
PROCEDURE Write(wr : Wr.T; READONLY t : T) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN StdfWr.Char(wr, t[0]) END Write;

BEGIN END StdfC1.
