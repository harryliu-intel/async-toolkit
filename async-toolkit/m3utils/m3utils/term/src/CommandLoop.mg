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

PROCEDURE PutCommand(self: T; cmd: Command; names, help: TEXT) =
  BEGIN
    self.cl.putCommand(names, NEW(CommandObject,
                                  cmd:=cmd,
                                  simpleHelp:=help,
                                  ctx := self.ctx));
  END PutCommand;

TYPE
  CommandObject = CommandLoop.Command OBJECT
    ctx: Context.T;
    cmd: Command;
  OVERRIDES
    execute := Execute;
  END;

PROCEDURE Execute(co: CommandObject;
                  args: TextList.T;
                  term: Term.T)
  RAISES {Error} =
  BEGIN
    co.cmd(co.ctx, args, term);
  END Execute;


PROCEDURE Run(self: T) =
  BEGIN
    self.cl.run();
  END Run;

BEGIN
END CommandLoop.
