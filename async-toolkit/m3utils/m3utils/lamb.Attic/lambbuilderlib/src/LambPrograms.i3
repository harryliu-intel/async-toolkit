INTERFACE LambPrograms;
IMPORT LambCommandSeq AS CommandSeq, Lamb;
IMPORT ParseParams;

TYPE Type = { Idle, Read, Write, ReadWrite };
CONST
  Names = ARRAY Type OF TEXT { "idle",
                               "read",
                               "write",
                               "rw"
  };

TYPE Proc = PROCEDURE(prog : CommandSeq.T; params : Lamb.T);

VAR Progs : ARRAY Type OF Proc;

PROCEDURE ParseFlag(pp : ParseParams.T) : Type RAISES { ParseParams.Error };

PROCEDURE WhichProgram() : Type;
  
END LambPrograms.
