(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE Metadata;

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    ofType := OfType;
  END;

PROCEDURE OfType(t : T; tc : Typecode) : T RAISES { NonUnique } =
  VAR
    p := t;
    res : T := NIL;
  BEGIN
    WHILE p # NIL DO
      IF TYPECODE(p) = tc THEN
        IF res = NIL THEN
          res := p
        ELSE
          RAISE NonUnique (* multiple *)
        END
      END;
      p := p.next
    END;
    IF res = NIL THEN
      RAISE NonUnique (* zero *)
    ELSE
      RETURN res
    END
  END OfType;

BEGIN END Metadata.
