(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE Lexer;
IMPORT Rd;

CONST BufSize = 2048;

TYPE 
  State = RECORD
    rd : Rd.T;
    s    := 0;     (* next token to parse starts here *)
    e    := 0;     (* end of currently valid chars in buffer *)
    eof  := FALSE; (* done parsing *)
    lNo  := 1;
  END;

TYPE String = RECORD start, n : [0..BufSize-1] END;

PROCEDURE GetToken(VAR buff : ARRAY OF CHAR; 
                   VAR state : State;
                   VAR res : String) : BOOLEAN;

END Lexer.
