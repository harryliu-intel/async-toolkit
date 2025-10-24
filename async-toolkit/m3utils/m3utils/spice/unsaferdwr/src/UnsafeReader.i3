(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE UnsafeReader;
IMPORT Rd;
IMPORT Thread;
IMPORT Word;

PROCEDURE ReadI(rd : Rd.T) : INTEGER
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } ;

PROCEDURE ReadU64(rd : Rd.T) : Word.T
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted } ;

PROCEDURE ReadLRA(rd : Rd.T; VAR q : ARRAY OF LONGREAL)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };
  (* if we get Thread.Alerted here, the file needs to be rewound, as we will
     have lost our place *)

PROCEDURE ReadLR(rd : Rd.T) : LONGREAL
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };
  (* if we get Thread.Alerted here, the file needs to be rewound, as we will
     have lost our place *)
  
CONST UintMax = Word.Shift(1, 32) - 1; (* won't work on 32-bit machine *)
      
TYPE U = BITS 32 FOR [0 .. UintMax];
  
PROCEDURE ReadUA(rd : Rd.T; VAR q : ARRAY OF CARDINAL)
  RAISES { Rd.EndOfFile, Rd.Failure, Thread.Alerted };
  (* if we get Thread.Alerted here, the file needs to be rewound, as we will
     have lost our place *)

END UnsafeReader.
