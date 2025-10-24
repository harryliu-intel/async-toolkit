(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE RecursiveParserRep;
IMPORT RecursiveParser;
FROM RecursiveLexer IMPORT Buffer, String, State;
IMPORT RecursiveLexer;
IMPORT ParseProcRec;

TYPE
  Rep = RecursiveParser.Public OBJECT 
    buff  : Buffer;
    token : String;
    state : State;
    eop   := FALSE; (* done parsing *)
    lexer : RecursiveLexer.T;
    lately := ParseProcRec.Default;
  END;

REVEAL 
  RecursiveParser.T = Rep BRANDED Brand OBJECT END;

CONST Brand = "RecursiveParserRep";

END RecursiveParserRep.
