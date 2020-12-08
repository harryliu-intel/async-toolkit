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

PROCEDURE Eval(t : T; fins : CARDINAL) : CARDINAL =
  VAR
    drawn : CARDINAL;
  BEGIN
    IF NOT t.tab.get(fins, drawn) THEN
      drawn := 0;
      WITH formula = "(" & t.formula & " " & Int(fins) & ")",
           scm = NEW(Scheme.T).init(ARRAY OF Pathname.T{}) DO
        Debug.Out("formula: " & formula);
        WITH q = scm.loadEvalText(formula) DO
          drawn := SchemeLongReal.Card(q)
        END
      END;
      EVAL t.tab.put(fins, drawn)
    END;
    RETURN drawn
  END Eval;

BEGIN END DrawnWidth.
