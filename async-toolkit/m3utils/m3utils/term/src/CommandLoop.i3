INTERFACE CommandLoop;
IMPORT Term;
IMPORT Pathname;
IMPORT TextList;
TYPE
  T <: Public;
  Public = OBJECT METHODS
    init(prompt := "> ";
         help := "help ?";
         quit := "quit ^D";
         load := "load input source";
         save := "save"): T;
    (* builtin features can be disabled by passing the empty TEXT. *)

    putCommand(names: TEXT; cmd: Command);
    (* "names" is a space-separated list of variants. *)

    run(source: Pathname.T := NIL; term: Term.T := NIL);
    (* If "source#NIL" then read from file instead of from terminal.
       If "term=NIL" then use "Term.Default()". *)
  END;


  (* User object.
     Default "execute" and "complete" gracefully do nothing.
     Default "help" uses "simpleHelp".

     Typically, the user will override "Command" adding a context field,
     which the user initializes to some object
     that remembers the application state.
  *)
  Command <: CommandPublic;
  CommandPublic = OBJECT
    simpleHelp: TEXT := NIL;
  METHODS
    execute(args: TextList.T; term: Term.T) RAISES {Error};
    (* "args.head" is the command name. *)

    help(args: TextList.T): TEXT RAISES {Error};
    (* "args.head" is the command name.
       result should have this form:
       "<arg1> <arg2> .. -- explanation" *) 

    complete(VAR input: TEXT) RAISES {Error};
  END;

EXCEPTION
  Error(TEXT);

END CommandLoop.
