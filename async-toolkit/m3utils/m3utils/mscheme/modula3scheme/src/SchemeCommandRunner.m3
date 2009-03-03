(* $Id$ *)

MODULE SchemeCommandRunner;
IMPORT Scheme;
FROM Scheme IMPORT E, Object;
IMPORT ProcUtils, TextWr, SchemeProcedure;
IMPORT SchemePrimitive, SchemePair, SchemeUtils, Wr, SchemeLongReal;
IMPORT Debug;
IMPORT TextRd;
IMPORT Time;
IMPORT Thread;

PROCEDURE RunCommandApply(proc : Procedure; 
                          interp : Scheme.T; 
                          args : Object) : Object RAISES { E } =
  BEGIN
    RETURN RunTimeoutCommandApply(proc,
                                  interp,
                                  NEW(SchemePair.T,
                                      first := SchemeLongReal.FromLR(LAST(Time.T)),
                                      rest := args))
  END RunCommandApply;

TYPE
  Done = { NotDone, Completed, TimedOut };

  Closure = Thread.Closure OBJECT
    c : Thread.Condition;
    mu : MUTEX;
    done : REF Done;
  END;

  WaitClosure = Closure OBJECT
    completion : ProcUtils.Completion;
  OVERRIDES
    apply := WCApply;
  END;

  TimeOClosure = Closure OBJECT
    timeo : Time.T;
  OVERRIDES
    apply := TOCApply;
  END;

PROCEDURE WCApply(wc : WaitClosure) : REFANY =
  BEGIN
    wc.completion.wait();
    LOCK wc.mu DO
      IF wc.done^ = Done.NotDone THEN wc.done^ := Done.Completed END
    END;
    Thread.Broadcast(wc.c);
    
    RETURN NIL
  END WCApply;

PROCEDURE TOCApply(toc : TimeOClosure) : REFANY =
  BEGIN
    WITH till = Time.Now() + toc.timeo DO
      WHILE Time.Now() < till DO 
        Thread.Pause(MIN(1.0d0, till-Time.Now()));
        LOCK toc.mu DO
          IF toc.done^ # Done.NotDone THEN RETURN NIL END
        END
      END
    END;
    LOCK toc.mu DO
      IF toc.done^ = Done.NotDone THEN toc.done^ := Done.TimedOut END
    END;

    Thread.Broadcast(toc.c);

    RETURN NIL
  END TOCApply;

PROCEDURE RunTimeoutCommandApply(proc : Procedure; 
                          interp : Scheme.T; 
                          args : Object) : Object RAISES { E } =
  VAR p := SchemeUtils.Rest(args);
      timeo := SchemeLongReal.FromO(SchemeUtils.First(args));
      wr := TextWr.New();
  BEGIN
    WHILE ISTYPE(p, SchemePair.T) AND p # NIL DO
      WITH word = SchemeUtils.StringifyQ(SchemeUtils.First(p),
                                         quoted := FALSE) DO
        Wr.PutText(wr, word);
        p := SchemeUtils.Rest(p);
        IF p # NIL THEN Wr.PutChar(wr, ' ') END
      END
    END;

    WITH owr = NEW(TextWr.T).init(),
         writer = ProcUtils.WriteHere(owr),
         cmdtext = TextWr.ToText(wr),
         completion = ProcUtils.RunText(cmdtext,
                                        stdout := writer) DO

      TRY
        IF timeo = LAST(Time.T) THEN
          completion.wait()
        ELSE
          WITH c = NEW(Thread.Condition),
               mu = NEW(MUTEX),
               done = NEW(REF Done),
               waitCl = NEW(WaitClosure, 
                            c := c, mu := mu, done := done,
                            completion := completion),
               timeoCl = NEW(TimeOClosure, 
                             c := c, mu := mu, done := done,
                             timeo := timeo) DO
            done^ := Done.NotDone;
            
            EVAL Thread.Fork(waitCl); EVAL Thread.Fork(timeoCl);

            LOCK mu DO
              LOOP
                Thread.Wait(mu,c);
                CASE done^ OF
                  Done.NotDone => (* skip *)
                |
                  Done.TimedOut => RAISE E("Timeout running \""& cmdtext & "\"")
                |
                  Done.Completed => EXIT
                END
              END
            END
          END
        END
      EXCEPT
        ProcUtils.ErrorExit(err) => RAISE E("ProcUtils.ErrorExit from running \"" & cmdtext & "\"")
      END;

      (* here we grab the results from running command and parse them...*)
      WITH result = TextWr.ToText(owr),
           rd = TextRd.New(result) DO
        Debug.Out("SchemeCommandRunner.RunCommandApply: cmd=\"" & cmdtext & 
          "\"; result: \"" & result & "\"");
        RETURN proc.outputParser.parseRd(rd)
      END
    END
  END RunTimeoutCommandApply;

TYPE 
  Procedure = SchemeProcedure.T OBJECT
    outputParser : OutputParser;
  END;

PROCEDURE Extend(op : OutputParser;
                 definer : SchemePrimitive.ExtDefiner) : SchemePrimitive.ExtDefiner =
  BEGIN
    definer.addPrim("run-command",
                    NEW(Procedure,
                        outputParser := op,
                        apply := RunCommandApply),
                    1, LAST(CARDINAL));

    definer.addPrim("run-command-with-timeout",
                    NEW(Procedure,
                        outputParser := op,
                        apply := RunTimeoutCommandApply),
                    2, LAST(CARDINAL));

    RETURN definer
  END Extend;

BEGIN END SchemeCommandRunner.
