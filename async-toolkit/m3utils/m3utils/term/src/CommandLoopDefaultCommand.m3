MODULE CommandLoopDefaultCommand EXPORTS CommandLoop;
IMPORT Term;
IMPORT TextList;


REVEAL
  Command = CommandPublic BRANDED OBJECT
  OVERRIDES
    execute             := Execute;
    help                := Help;
    complete            := Complete;
  END;

PROCEDURE Execute(<*UNUSED*>self: Command;
                  <*UNUSED*>args: TextList.T;
                  term: Term.T) RAISES {Error} =
  BEGIN
    term.wr("not implemented yet - skip",TRUE,TRUE);
  END Execute;

PROCEDURE Help(self: Command; <*UNUSED*>args: TextList.T) : TEXT =
  BEGIN
    IF self.simpleHelp # NIL THEN
      RETURN self.simpleHelp;
    ELSE
      RETURN "-- nobody really knows what this command does.";
    END;
  END Help;

PROCEDURE Complete(<*UNUSED*>self: Command;
                   <*UNUSED*>VAR input: TEXT) RAISES {Error} =
  BEGIN
  END Complete;

BEGIN
END CommandLoopDefaultCommand.
