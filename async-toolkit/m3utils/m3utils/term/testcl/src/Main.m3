MODULE Main;
IMPORT FloatMode;
IMPORT Term;
IMPORT TextList;
IMPORT Lex;
IMPORT Scan;
IMPORT Fmt;
FROM CommandLoop IMPORT Error;

(* custom-context command-loop created using generics;
   see m3makefile
*)
IMPORT MyContext;
IMPORT MyCommandLoop;

PROCEDURE Print(ctx: MyContext.T;
                <*UNUSED*>args: TextList.T;
                term: Term.T)
  <*NOWARN*>RAISES {Error} =
  BEGIN
    term.wr("acc = " & Fmt.Int(ctx.acc) & "\n");
  END Print;

PROCEDURE Add(ctx: MyContext.T;
              args: TextList.T;
              <*UNUSED*>term: Term.T)
  RAISES {Error} =
  BEGIN
    args := args.tail;
    IF TextList.Length(args) # 1 THEN
      RAISE Error("1 argument expected");
    END;
    TRY
      INC(ctx.acc, Scan.Int(args.head));
    EXCEPT FloatMode.Trap, Lex.Error =>
      RAISE Error("expected integer argument");
    END;
  END Add; 

PROCEDURE Run(context: MyContext.T) =
  VAR
    cl := NEW(MyCommandLoop.T).init(context, "cl-test> ");
  BEGIN

    (* define commands *)
    cl.c(Print,"print","-- display accumulator value");
    cl.c(Add,  "+ add","<val> -- add <val> to accumulator",
         %TEXT\%
add command: extended help
The accumulator is written with a new value,
which is the old value of the accumulator plus <val>.
%END%);

    cl.run();
  END Run;

BEGIN
  Run(NEW(MyContext.T));
END Main.
