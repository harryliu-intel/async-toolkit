(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

GENERIC INTERFACE Tokenizer(Defs);

(* 
   Generic fast lexical scanner (tokenizer)

   Standard implementation in the accompanying MODULE performs
   lexical scanning without any heap memory allocation.

   Suitable for building very fast parsers that will work on large 
   files and filter out specific syntactic structures.

   Author : Mika Nystrom <mika.nystroem@intel.com>
   November-December, 2020
*)

(* the Defs interface must contain the following symbols:

BufSiz      : CARDINAL    -- CARDINAL size of input buffer - needs to be at
                             least longest token + 1

Special     : SET OF CHAR -- special characters to recognize as tokens

White       : SET OF CHAR -- whitespace characters to skip

Ident1      : SET OF CHAR -- legal initial chars for identifiers

Ident2      : SET OF CHAR -- legal non-initial chars for identifiers

CComments   : BOOLEAN     -- recognize (and skip) C-style comments

DoString    : BOOLEAN     -- recognize strings

StringQuote : CHAR        -- quotation mark to use (backslash always escapes)

*)

IMPORT Rd, Thread;

TYPE
  State = RECORD
    rd       : Rd.T;                       (* input stream *)

    progress := TRUE;                      (* config *)

    (* debugging variables *)
    lev      : CARDINAL := 0;
    lineno   : CARDINAL := 1;
    bytes    : CARDINAL := 0;

    b        : CARDINAL := Defs.BufSiz;    (* buffer pointer  *)
    e        : CARDINAL := Defs.BufSiz;    (* end of buffer   *)
    s        : CARDINAL := Defs.BufSiz;    (* start of token  *)

    haveTok  := FALSE;
    string   := FALSE;
  END;

  Buffer = ARRAY [ 0 .. Defs.BufSiz-1 ] OF CHAR;

  E = RECORD file : TEXT; line : CARDINAL END;
  (* file and line of a syntax error within lexer *)
  
EXCEPTION Syntax(E); 

TYPE Token = RECORD s, n : CARDINAL END;

PROCEDURE GetAny(VAR buf : ARRAY OF CHAR; VAR st : State;
                 VAR tok : Token) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetExact(VAR buf : ARRAY OF CHAR; VAR st : State;
                   READONLY str : ARRAY OF CHAR) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetIdent(VAR buf : ARRAY OF CHAR; VAR st : State;
                   VAR str : Token) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };
  (* returns TRUE iff token matches {Ident1}{Ident2}* *)

PROCEDURE GetString(VAR buf : ARRAY OF CHAR; VAR st : State;
                   VAR str : Token) : BOOLEAN
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };
  (* str is the string including quotes and all internal escapes *)

PROCEDURE GetInt(VAR buf : ARRAY OF CHAR; VAR st : State;
                 VAR int : INTEGER) : BOOLEAN 
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

PROCEDURE GetFloat(VAR buf : ARRAY OF CHAR; VAR st : State;
                   VAR lr : LONGREAL) : BOOLEAN 
  RAISES { Syntax, Rd.EndOfFile, Rd.Failure, Thread.Alerted };

CONST Brand = "Tokenizer(" & Defs.Brand & ")";
        
END Tokenizer.
