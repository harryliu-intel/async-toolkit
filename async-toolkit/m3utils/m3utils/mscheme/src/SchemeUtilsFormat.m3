(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)

MODULE SchemeUtilsFormat EXPORTS SchemeUtils;
FROM Scheme IMPORT Object;

PROCEDURE DebugFormat(x : Object) : TEXT =
  (* for debugging, something not really needed in the Java version since
     everything has a .toString there *)
  BEGIN
    RETURN "<DebugFormat>" (* ho hum... *)
  END DebugFormat;

BEGIN END SchemeUtilsFormat.
