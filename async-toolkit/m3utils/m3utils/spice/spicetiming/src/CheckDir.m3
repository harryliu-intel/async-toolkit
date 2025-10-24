(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE CheckDir;
IMPORT Transition;
FROM Fmt IMPORT Int;
IMPORT Word;

PROCEDURE Fmt(c : T) : TEXT =
  VAR
    res := "{ ";
  BEGIN
    FOR i := FIRST(Transition.Dir) TO LAST(Transition.Dir) DO
      IF i IN c THEN
        res := res & Int(i) & " "
      END
    END;

    RETURN res & "}"
  END Fmt;

PROCEDURE Hash(t : T) : Word.T =
  VAR
    q := 1;
    res : Word.T := 0;
  BEGIN
    FOR i := FIRST(Transition.Dir) TO LAST(Transition.Dir) DO
      IF i IN t THEN
        res := Word.Plus(res, q)
      END;
      q := Word.Shift(q, 1);
    END;
    RETURN res
  END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;
  
BEGIN END CheckDir.
