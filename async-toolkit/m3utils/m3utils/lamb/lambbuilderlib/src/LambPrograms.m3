MODULE LambPrograms;
IMPORT LambCommandSeq AS CommandSeq;
IMPORT LambCommand AS Command, LambVerb AS Verb;
IMPORT Lamb;
IMPORT ParseParams;
IMPORT Debug;
IMPORT Text;

CONST TE = Text.Equal;

CONST Cycles = 100;

PROCEDURE IdleProgram(prog : CommandSeq.T; C : Lamb.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    FOR i := 1 TO Cycles DO
      AddCmd(  Cmd { V.Nop                     })
    END
  END IdleProgram;

(**********************************************************************)

PROCEDURE ReadProgram(prog : CommandSeq.T; C : Lamb.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    FOR i := 1 TO Cycles DO
      AddCmd(  Cmd { V.Read,             0     });
    END
  END ReadProgram;

  (**********************************************************************)

PROCEDURE WriteProgram(prog : CommandSeq.T; C : Lamb.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    FOR i := 1 TO Cycles DO
      AddCmd(  Cmd { V.Writ,             1, 1     });
    END
  END WriteProgram;

  (**********************************************************************)

PROCEDURE ReadWriteProgram(prog : CommandSeq.T; <*UNUSED*>C : Lamb.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    FOR i := 1 TO Cycles DO
      AddCmd(  Cmd { V.RdWr, 1, 1, 2                    });
    END
  END ReadWriteProgram;


VAR
  haveProgram := FALSE;
  theProgram : Type;
  
PROCEDURE ParseFlag(pp : ParseParams.T) : Type  RAISES { ParseParams.Error } =
  VAR
    pt : Type;
  BEGIN
    IF pp.keywordPresent("-prog") THEN
      VAR found := FALSE;
          nm := pp.getNext();
      BEGIN
        FOR i := FIRST(Type) TO LAST(Type) DO
          IF TE(Names[i], nm) THEN
            <*ASSERT NOT haveProgram*>
            pt := i; found := TRUE; theProgram := i; haveProgram := TRUE; EXIT
          END
        END;
        IF NOT found THEN Debug.Error("Unknown LAMB program \"" & nm & "\"") END
      END
    END;
    RETURN pt
  END ParseFlag;

PROCEDURE WhichProgram() : Type =
  BEGIN
    IF haveProgram THEN
      RETURN theProgram
    ELSE
      <*ASSERT FALSE*>
    END
  END WhichProgram;
  
BEGIN
  Progs := ARRAY OF Proc { IdleProgram,
                           ReadProgram,
                           WriteProgram,
                           ReadWriteProgram };
END LambPrograms.
