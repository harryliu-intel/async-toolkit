(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DefLexer;
IMPORT RecursiveLexer;

CONST BaseSpecial    = SET OF CHAR { '(', ')', '{', '}', '-', '+', ';' };
CONST DefDivChar = '/';
CONST DefBusbitChars = ARRAY [0..1] OF CHAR { '[', ']' };

CONST DefSpecial = BaseSpecial + 
                   SET OF CHAR { DefDivChar } + 
                   SET OF CHAR { DefBusbitChars[0] } + 
                   SET OF CHAR { DefBusbitChars[1] };

CONST Digit = SET OF CHAR { '0' .. '9' };

TYPE 
  T <: Public;

  Public = RecursiveLexer.T OBJECT
    special     := DefSpecial;
    divChar     := DefDivChar;
    busbitChars := DefBusbitChars;
  END;

PROCEDURE DividerChar(s : T; c : CHAR);

PROCEDURE BusbitChars(s : T; c : ARRAY [0..1] OF CHAR);

CONST Brand = "DefLexer";

END DefLexer.
