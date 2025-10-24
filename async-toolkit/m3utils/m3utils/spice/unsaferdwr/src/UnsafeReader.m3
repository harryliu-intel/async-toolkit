(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

UNSAFE MODULE UnsafeReader;
IMPORT Rd;
IMPORT Debug;
IMPORT Thread;
IMPORT Fmt;
IMPORT Word;

VAR doDebug := Debug.DebugThis("UnsafeReader");
  

PROCEDURE ReadI(rd : Rd.T) : INTEGER
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    ibuff := NEW(REF ARRAY OF CHAR, 4);
    (* hm the bit sizing is suspicious here *)
  BEGIN
    WITH n = Rd.GetSub(rd, ibuff^) DO
      <*ASSERT n <= 4*>
      IF n # 4 THEN RAISE Rd.EndOfFile END
    END;
    RETURN LOOPHOLE(ibuff, REF ARRAY OF INTEGER)[0]
  END ReadI;

PROCEDURE ReadU64(rd : Rd.T) : Word.T
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    ibuff := NEW(REF ARRAY OF CHAR, 8);
  BEGIN
    WITH n = Rd.GetSub(rd, ibuff^) DO
      <*ASSERT n <= 8*>
      IF n # 8 THEN RAISE Rd.EndOfFile END
    END;
    RETURN LOOPHOLE(ibuff, REF ARRAY OF Word.T)[0]
  END ReadU64;

PROCEDURE ReadLRA(rd : Rd.T; VAR q : ARRAY OF LONGREAL)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    buff        := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    IF doDebug THEN
      Debug.Out("ReadLRA start, NUMBER(q)=" & Fmt.Int(NUMBER(q)))
    END;
    FOR i := FIRST(q) TO LAST(q) DO
      IF doDebug THEN
        Debug.Out("ReadLRA attempting read of 4 bytes")
      END;
      WITH n = Rd.GetSub(rd, buff^) DO
        <*ASSERT n <= 4*>
        IF n # 4 THEN RAISE Rd.EndOfFile END
      END;
      q[i] := FLOAT(LOOPHOLE(buff, REF ARRAY OF REAL)[0], LONGREAL);
    END
  END ReadLRA;

PROCEDURE ReadLR(rd : Rd.T) : LONGREAL
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    arr : ARRAY [ 0..0 ] OF LONGREAL;
  BEGIN
    ReadLRA(rd, arr);
    RETURN arr[0]
  END ReadLR;
  
PROCEDURE ReadUA(rd : Rd.T; VAR q : ARRAY OF CARDINAL)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } =
  VAR
    buff        := NEW(REF ARRAY OF CHAR, 4);
  BEGIN
    IF doDebug THEN
      Debug.Out("ReadUA start, NUMBER(q)=" & Fmt.Int(NUMBER(q)))
    END;
    FOR i := FIRST(q) TO LAST(q) DO
      IF doDebug THEN
        Debug.Out("ReadUA attempting read of 4 bytes")
      END;
      WITH n = Rd.GetSub(rd, buff^) DO
        <*ASSERT n <= 4*>
        IF n # 4 THEN RAISE Rd.EndOfFile END
      END;
      q[i] := LOOPHOLE(buff, REF ARRAY OF U)[0];
    END
  END ReadUA;

BEGIN END UnsafeReader.
