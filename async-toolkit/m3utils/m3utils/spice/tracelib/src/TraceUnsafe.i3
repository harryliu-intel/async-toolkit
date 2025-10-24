(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TraceUnsafe;
IMPORT TraceHeader;
IMPORT Rd;

PROCEDURE GetHeader(tRd : Rd.T; nNodes : CARDINAL) : TraceHeader.T
  RAISES { Rd.Failure, Rd.EndOfFile };

PROCEDURE GetDataArray(tRd        : Rd.T;
                       READONLY h : TraceHeader.T;
                       id         : CARDINAL;
                       VAR arr    : ARRAY OF LONGREAL)
  RAISES { Rd.Failure, Rd.EndOfFile };

PROCEDURE GetBytes(rd : Rd.T; bytes : CARDINAL) : TEXT
  RAISES { Rd.Failure, Rd.EndOfFile };
  
END TraceUnsafe.
