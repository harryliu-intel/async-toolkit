(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE DrawnWidth;
IMPORT Scheme, SchemeLongReal;
IMPORT CardCardTbl;
IMPORT Pathname;

FROM Fmt IMPORT Int;
IMPORT Debug;

REVEAL
  T = Public BRANDED Brand OBJECT
    formula : TEXT;
    tab : CardCardTbl.T;
  OVERRIDES
    init := Init;
    eval := Eval;
  END;

PROCEDURE Init(t : T; defString : TEXT) : T =
  BEGIN
    t.formula := defString;
    t.tab := NEW(CardCardTbl.Default).init();
    RETURN t
  END Init;

PROCEDURE Eval(t : T; fins : [1..LAST(CARDINAL)]) : CARDINAL =
  VAR
    drawn : CARDINAL;
  BEGIN
    IF NOT t.tab.get(fins, drawn) THEN
      drawn := 0;
      TRY
        WITH formula = "(" & t.formula & " " & Int(fins) & ")",
             scm = NEW(Scheme.T).init(ARRAY OF Pathname.T{}) DO
          Debug.Out("formula: " & formula);
          WITH q = scm.loadEvalText(formula) DO
            drawn := SchemeLongReal.Card(q)
          END
        END
      EXCEPT
        Scheme.E(x) => Debug.Error("Caught unexpected Scheme.E : " & x)
      END;
      EVAL t.tab.put(fins, drawn)
    END;
    RETURN drawn
  END Eval;

BEGIN END DrawnWidth.
