(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

(* $Id$ *)

INTERFACE PGSQLScan;

(* scanning of peculiar PostgreSQL data types *)
(* really should include scanning of timestamps too *)

IMPORT Lex;

PROCEDURE Bool(txt : TEXT) : BOOLEAN RAISES { Lex.Error };
  (* t -> TRUE | f -> FALSE | RAISE Lex.Error *)

CONST Brand = "PGSQLScan";

END PGSQLScan.
