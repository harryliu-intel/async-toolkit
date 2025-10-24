(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Test EXPORTS Main;
FROM Fmt IMPORT Int;
IMPORT IO;

VAR c : CARDINAL;
VAR d : [ FIRST(CARDINAL) .. LAST(CARDINAL) ];
VAR e : INTEGER;
VAR f : [ FIRST(INTEGER) .. LAST(INTEGER) ];
    
PROCEDURE P(VAR x : CARDINAL) =
  BEGIN  x := 12 END P;

PROCEDURE Q(VAR x : INTEGER) =
  BEGIN x := -12 END Q;
  
BEGIN
  P(c);
  P(d);
  IO.Put(Int(c) & "\n");
  Q(e);
  Q(f)
END Test.
