(* $Id$ *)

MODULE SchemeCommandRunner;
IMPORT Scheme;
FROM Scheme IMPORT E, Object;
IMPORT ProcUtils, TextWr, SchemeProcedure;
IMPORT SchemePrimitive, SchemePair, SchemeUtils, Wr;

PROCEDURE RunCommandApply(<*UNUSED*>proc : SchemeProcedure.T; 
                          interp : Scheme.T; 
                          args : Object) : Object RAISES { E } =
  VAR p := args;
      wr := TextWr.New();
  BEGIN
    WHILE ISTYPE(p, SchemePair.T) AND p # NIL DO
      WITH word = SchemeUtils.Stringify(SchemeUtils.First(p)) DO
        Wr.PutText(wr, word);
        p := SchemeUtils.Rest(p);
        IF p # NIL THEN Wr.PutChar(wr, ' ') END
      END
    END;

    WITH wr = NEW(TextWr.T).init(),
         writer = ProcUtils.WriteHere(wr),
         completion = ProcUtils.RunText(TextWr.ToText(wr),
                                        stdout := writer) DO

      TRY
        completion.wait()
      EXCEPT
        ProcUtils.ErrorExit(err) =>
      END
    END

    

  END RunCommandApply;

PROCEDURE Extend(definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner =
  BEGIN
    definer.addPrim("run-command",
                    NEW(SchemeProcedure.T,
                        apply := RunCommandApply),
                    2, LAST(CARDINAL))
  END Extend;

BEGIN END SchemeCommandRunner.
