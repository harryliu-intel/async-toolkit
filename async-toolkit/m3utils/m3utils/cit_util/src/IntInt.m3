(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: IntInt.m3,v 1.1 2007/05/31 12:00:53 mika Exp $ *)

MODULE IntInt;
IMPORT Integer;
PROCEDURE Compare(a, b: T): [-1..1] = 
  BEGIN
    RETURN Integer.Compare(a.key, b.key);
  END Compare;
BEGIN
END IntInt.
