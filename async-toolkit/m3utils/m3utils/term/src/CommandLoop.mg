GENERIC MODULE CommandLoop(Context);
FROM CommandLoop IMPORT Error;
IMPORT CommandLoop;
IMPORT TextList;
IMPORT Term;

(* "CommandLoop"s for the Layman *)

REVEAL
  T = Public BRANDED OBJECT
    ctx:  Context.T;
    cl: CommandLoop.T;
  OVERRIDES
    init := Init;
    c := PutCommand;
    run := Run;
  END;


PROCEDURE Init(self: T; ctx: Context.T; prompt := "> "): T =
  BEGIN
    self.cl := NEW(CommandLoop.T).init(prompt);
    self.ctx := ctx;
    RETURN self;
  END Init;

PROCEDURE PutCommand(self: T; cmd: Command; names: TEXT;
                     simpleHelp, extendedHelp: TEXT := NIL) =

  BEGIN
    self.cl.putCommand(names, NEW(CommandObject,
                                  cmd:=cmd,
                                  simpleHelp:=simpleHelp,
                                  extHelp := extendedHelp,
                                  hasExtendedHelp := (extendedHelp # NIL),
                                  ctx := self.ctx));
  END PutCommand;

TYPE
  CommandObject = CommandLoop.Command OBJECT
    ctx: Context.T;
    cmd: Command;
    extHelp: TEXT;
  OVERRIDES
    execute := Execute;
    extendedHelp := ExtendedHelp;
  END;

PROCEDURE Execute(co: CommandObject;
                  args: TextList.T;
                  term: Term.T)
  RAISES {Error} =
  BEGIN
    co.cmd(co.ctx, args, term);
  END Execute;

PROCEDURE ExtendedHelp(co: CommandObject;
                       <*UNUSED*>args: TextList.T): TEXT RAISES {Error} =
  BEGIN
    IF co.extHelp = NIL THEN
      RAISE Error("No extended help provided for this command.");
    END;
    RETURN co.extHelp;
  END ExtendedHelp;

PROCEDURE Run(self: T) =
  BEGIN
    self.cl.run();
  END Run;

BEGIN
END CommandLoop.
