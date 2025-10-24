(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Rep16Stream;
FROM Rep16 IMPORT Header, T;
IMPORT Rd, Wr;
IMPORT Thread;

PROCEDURE Bytes(t : T) : CARDINAL;

CONST HeaderBytes = 4 + 4 + 4 + 4;

PROCEDURE WriteT(wr : Wr.T; READONLY t : T)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE WriteHeader(wr : Wr.T; READONLY h : Header)
  RAISES { Wr.Failure, Thread.Alerted };

PROCEDURE ReadT(rd : Rd.T; VAR t : T) : CARDINAL
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted };

PROCEDURE ReadHeader(rd : Rd.T; VAR h : Header) : CARDINAL
  RAISES { Rd.Failure, Rd.EndOfFile, Thread.Alerted };
  
END Rep16Stream.
