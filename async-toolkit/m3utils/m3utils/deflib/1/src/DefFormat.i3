(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE DefFormat;
IMPORT Rd;
IMPORT ParseError;
IMPORT RecursiveParser;

TYPE
  T <: Public;

  Public = RecursiveParser.T OBJECT END;

PROCEDURE Parse(rd : Rd.T) : T RAISES { ParseError.E };

CONST Brand = "DefFormat";

PROCEDURE D(what : TEXT);

END DefFormat.
