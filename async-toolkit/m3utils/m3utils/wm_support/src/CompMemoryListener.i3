(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE CompMemoryListener;
IMPORT CsrOp;
IMPORT Word;

TYPE
  T =  OBJECT METHODS
    callback(writeOp : CsrOp.T);
    hash() : Word.T;
    equal(t : T) : BOOLEAN;
  END;

CONST Brand = "CompMemoryListener";

PROCEDURE Hash(a : T) : Word.T;      (* calls a.hash() *)
PROCEDURE Equal(a, b : T) : BOOLEAN; (* calls a.equal(b) *)
  
END CompMemoryListener.
