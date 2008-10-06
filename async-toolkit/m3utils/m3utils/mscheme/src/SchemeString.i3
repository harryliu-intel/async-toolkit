(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

INTERFACE SchemeString;

TYPE T = REF ARRAY OF CHAR;

CONST Brand = "SchemeString";

PROCEDURE FromText(txt : TEXT) : T;

END SchemeString.
