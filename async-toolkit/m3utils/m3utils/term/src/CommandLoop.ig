GENERIC INTERFACE CommandLoop(Context);
IMPORT TextList;
IMPORT Term;
FROM CommandLoop IMPORT Error;

(* "CommandLoop"s for the Layman *)

TYPE
  T <: Public;
  Public = OBJECT
  METHODS
    init(ctx: Context.T; prompt := "> "): T;

    c(cmd: Command; names: TEXT;
      simpleHelp, extendedHelp: TEXT := NIL);
    (* define a new command *)

    run();
  END;

  Command = PROCEDURE (ctx: Context.T;  
                       args: TextList.T;
                       term: Term.T)
  RAISES {Error};

END CommandLoop.
