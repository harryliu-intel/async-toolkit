(* $Id$ *)

INTERFACE PGSQLScan;

(* scanning of peculiar PostgreSQL data types *)
(* really should include scanning of timestamps too *)

IMPORT Lex;

PROCEDURE Bool(txt : TEXT) : BOOLEAN RAISES { Lex.Error };
  (* t -> TRUE | f -> FALSE | RAISE Lex.Error *)

CONST Brand = "PGSQLScan";

END PGSQLScan.
