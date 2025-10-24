(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE StdfBn;
IMPORT Rd, StdfE;
IMPORT Thread;
IMPORT Word;
IMPORT StdfRd;
IMPORT Wx;
IMPORT Wr;
IMPORT StdfWr;

PROCEDURE Parse(rd : Rd.T; VAR len : CARDINAL; VAR t : T)
  RAISES { StdfE.E, Thread.Alerted, Rd.Failure, Rd.EndOfFile } =
  VAR
    u, v : [0..255];
  BEGIN
    IF len = 0 THEN t := Default(); RETURN END;
    u := StdfRd.U1(rd, len);
    t := NEW(REF ARRAY OF BOOLEAN, 8 * u);
    FOR i := FIRST(t^) TO LAST(t^) DO
      IF i MOD 8 = 0 THEN
        v  := StdfRd.U1(rd, len);
        t[i] := Word.And(v,1) = 1
      ELSE
        t[i] := Word.And(Word.RightShift(v, i MOD 8), 1) = 1
      END
    END
  END Parse;
  
PROCEDURE Format(t : T) : TEXT =
  VAR
    wx := Wx.New();
  BEGIN
    FOR i := FIRST(t^) TO LAST(t^) DO
      IF t[i] THEN
        Wx.PutChar(wx, 't')
      ELSE
        Wx.PutChar(wx, 'f')
      END
    END;
    RETURN Brand & " : " & Wx.ToText(wx)
  END Format;

PROCEDURE Default() : T = BEGIN RETURN NEW(T, 0) END Default;

PROCEDURE Bytes(READONLY t : T) : CARDINAL =
  BEGIN
    RETURN 1 + ((NUMBER(t^) - 1) DIV 8 + 1)
  END Bytes;
  
PROCEDURE Write(wr : Wr.T; READONLY t : T)
  RAISES { Thread.Alerted, Wr.Failure } =
  VAR
    n  := NUMBER(t^);
    nb : [0..255] := (n - 1) DIV 8 + 1;
  BEGIN
    StdfWr.U1(wr, nb);
    
    FOR i := 0 TO nb - 1 DO
      VAR
        b : [0..255] := 0;
        k := 0;
      CONST
        Sel = ARRAY [FALSE..TRUE] OF CARDINAL { 0, 1 };
      BEGIN
        FOR j := i * 8 TO MIN(i * 8 + 7, LAST(t^)) DO
          b := Word.Insert(b, Sel[t[8 * i + j]], k, 1);
          INC(k)
        END;
        StdfWr.U1(wr, b)
      END
    END
  END Write;

BEGIN END StdfBn.
