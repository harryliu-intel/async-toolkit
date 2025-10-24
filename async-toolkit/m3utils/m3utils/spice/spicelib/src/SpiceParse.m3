(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE SpiceParse;
IMPORT Text;
FROM SpiceFileFormat IMPORT White;
IMPORT Debug;
FROM Fmt IMPORT Int, F;

PROCEDURE HavePrefix(READONLY line : ARRAY OF CHAR;
                     VAR         p : CARDINAL;
                     search        : TEXT) : BOOLEAN =
  VAR
    n := Text.Length(search);
  BEGIN
    IF NUMBER(line) - p < n THEN RETURN FALSE END;

    FOR i := 0 TO n-1 DO
      IF NOT CaseIns(Text.GetChar(search,i),line[p+i]) THEN
        RETURN FALSE
      END
    END;

    IF n # NUMBER(line) AND n > LAST(line) THEN
      Debug.Error(F("SpiceParse.HavePrefix(\"%s\", %s, \"%s\") : n %s > LAST(line) %s",
                    Text.FromChars(line),
                    Int(p),
                    search,
                    Int(n),
                    Int(LAST(line))))
    END;
    
    IF n # NUMBER(line) AND NOT line[n] IN White THEN RETURN FALSE END;
    
    INC(p,n);
    WHILE p < NUMBER(line) AND line[p] IN White DO
      INC(p)
    END;
    RETURN TRUE
  END HavePrefix;

PROCEDURE CaseIns(a, b : CHAR) : BOOLEAN =
  BEGIN
    IF a >= 'a' AND a <= 'z' THEN
      a := VAL(ORD(a)-ORD('a')+ORD('A'),CHAR)
    END;
    IF b >= 'a' AND b <= 'z' THEN
      b := VAL(ORD(b)-ORD('a')+ORD('A'),CHAR)
    END;
    RETURN a = b
  END CaseIns;

BEGIN END SpiceParse.
