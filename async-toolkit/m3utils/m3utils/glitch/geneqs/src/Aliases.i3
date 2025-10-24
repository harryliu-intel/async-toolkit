(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Aliases;
IMPORT Rd;
IMPORT Thread;

TYPE
  Tokenizer <: PubTokenizer;

  PubTokenizer = OBJECT METHODS
    init(rd : Rd.T) : Tokenizer;
    token(VAR tok : TEXT; VAR sep : CHAR) : BOOLEAN
      RAISES { Rd.Failure, Thread.Alerted };
    whatLine() : CARDINAL;
  END;

CONST Brand = "Aliases";

END Aliases.
