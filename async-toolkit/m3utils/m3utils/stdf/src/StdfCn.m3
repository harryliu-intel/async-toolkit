(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE StdfCn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Text;
IMPORT StdfC1;
IMPORT StdfRd;
IMPORT Wr;
IMPORT StdfWr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    rlen : [0..255];
  BEGIN
    IF len = 0 THEN t := Default(); RETURN END;
    rlen := StdfRd.U1(rd, len);
    t := NEW(T, rlen);
    FOR i := FIRST(t^) TO LAST(t^) DO
      StdfC1.Parse(rd, len, SUBARRAY(t^,i,1))
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  BEGIN
    RETURN Brand & " : " & Text.FromChars(t^)
  END Format;

PROCEDURE Default() : T = BEGIN RETURN NEW(T, 0) END Default;

PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure } =
  BEGIN
    StdfWr.U1(wr, NUMBER(t^));
    FOR i := FIRST(t^) TO LAST(t^) DO
      StdfWr.Char(wr, t[i])
    END
  END Write;

PROCEDURE Bytes(READONLY t : T) : CARDINAL =
  BEGIN
    RETURN 1 + NUMBER(t^)
  END Bytes;
  
BEGIN END StdfCn.
