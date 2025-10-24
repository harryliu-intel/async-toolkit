(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE LibertyArcs;
IMPORT SchemePair;
IMPORT Text;
IMPORT SchemeUtils;
IMPORT Scan, SchemeLongReal, SchemeSymbol;
IMPORT Lex, FloatMode;
IMPORT SchemeObject;

TYPE State = { Field, Index };
VAR Atoms := ARRAY State OF SchemeSymbol.T { SchemeSymbol.FromText("field"),
                                             SchemeSymbol.FromText("get") };

PROCEDURE ToScheme(txt : TEXT) : SchemeObject.T =
  VAR
    obj : SchemeObject.T;
  BEGIN
    TRY
      obj := SchemeLongReal.FromI(Scan.Int(txt))
    EXCEPT
      Lex.Error, FloatMode.Trap => obj := SchemeSymbol.FromText(txt)
    END;
    RETURN obj
  END ToScheme;
  
PROCEDURE Parse(spec : TEXT) : SchemePair.T =

  PROCEDURE Push(i : CARDINAL) =
    BEGIN
      IF p # i THEN
        WITH this = NEW(SchemePair.T,
                        first := Atoms[parsing],
                        rest  := ToScheme(Text.Sub(spec, p, i - p))),
             cell = NEW(SchemePair.T,
                        first := this,
                        rest := res) DO
          res := cell
        END
      END;
      p := i + 1;
    END Push;
        
  VAR
    n := Text.Length(spec);
    p := 0;
    parsing := State.Field; (* no initial dot needed *)
    res : SchemePair.T := NIL;
  BEGIN
    FOR i := 0 TO n - 1 DO

      VAR
        c := Text.GetChar(spec, i);
      BEGIN
        IF c = '.' THEN
          Push(i);
          parsing := State.Field
        ELSIF c = '[' THEN
          Push(i);
          parsing := State.Index
        ELSIF c = ']' THEN
          Push(i)
        END
      END
    END;
    Push(n);
    RETURN SchemeUtils.Reverse(res)
  END Parse;

BEGIN END LibertyArcs.
