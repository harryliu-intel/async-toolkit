(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE StdfTypeName;
IMPORT Text;

PROCEDURE GetByteLength(nm : TEXT) : [ NotAFixedLength..LAST(CARDINAL) ] =
  VAR
    len := Text.Length(nm);
  BEGIN
    IF len < 2 THEN
      RETURN NotAFixedLength
    ELSE
      VAR
        res := 0;
      BEGIN
        FOR k := 1 TO len - 1 DO
          WITH c = Text.GetChar(nm, k) DO
            IF c < '0' OR c > '9' THEN
              RETURN NotAFixedLength
            ELSE
              res := 10 * res + ORD(c) - ORD('0')
            END
          END
        END;
        RETURN res
      END
    END
  END GetByteLength;
  
BEGIN END StdfTypeName.
