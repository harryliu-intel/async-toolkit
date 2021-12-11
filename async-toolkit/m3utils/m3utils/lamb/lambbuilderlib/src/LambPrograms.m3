MODULE LambPrograms;

IMPORT LambCommandSeq AS CommandSeq;
IMPORT LambCommand AS Command, LambVerb AS Verb;
IMPORT Lamb;
IMPORT ParseParams;
IMPORT Debug;
IMPORT Text;
IMPORT BitInteger;
IMPORT BigInt;
IMPORT BigIntOps;
FROM Fmt IMPORT F;

CONST TE = Text.Equal;
CONST SI = BitInteger.Small;
      
CONST Cycles = 100;

TYPE
  Cmd = Command.T;
  V   = Verb.T;

PROCEDURE HalfOnesBits(w, ob : CARDINAL) : BitInteger.T =
  VAR
    int := BigInt.Sub(BigIntOps.Exp2i(ob), BigInt.One);
    (* 2^w - 1 *)
  BEGIN
    <*ASSERT w >= ob*>
    RETURN BitInteger.Big(int, w)
  END HalfOnesBits;

PROCEDURE SharedCoda(AddCmd : PROCEDURE(c : Cmd)) =
  (* trigger a transition *)
  BEGIN
    AddCmd(  Cmd { V.Read,             SI(0)   }); 
  END SharedCoda;

PROCEDURE IdleProgram(prog : CommandSeq.T; <*UNUSED*>C : Lamb.T) =

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    FOR i := 1 TO Cycles DO
      AddCmd(  Cmd { V.Nop                     })
    END;
    SharedCoda(AddCmd)
  END IdleProgram;

(**********************************************************************)

PROCEDURE ReadProgram(prog : CommandSeq.T; C : Lamb.T) =

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  PROCEDURE Write(data, at : BitInteger.T) =
    BEGIN
      Debug.Out(F("ReadProgram.Write : Writing %s @ %s",
                  BitInteger.Format(data),
                  BitInteger.Format(at)));
      AddCmd( Cmd { V.Writ, data, at })
    END Write;
    
    
  VAR
    HalfOnesAddr := HalfOnesBits(C.LN, C.LN DIV 2);
    HalfOnesData := HalfOnesBits(C.W,  C.W  DIV 2);
    ZeroAddr := SI(0);
    ZeroData := SI(0);
  BEGIN
    Write(ZeroData, ZeroAddr);
    Write(HalfOnesData, HalfOnesAddr);

    FOR i := 1 TO Cycles BY 2 DO
      AddCmd(  Cmd { V.Read,             ZeroAddr     });
      AddCmd(  Cmd { V.Read,             HalfOnesAddr     });
    END;
    SharedCoda(AddCmd)
  END ReadProgram;

  (**********************************************************************)

PROCEDURE WriteProgram(prog : CommandSeq.T; C : Lamb.T) =

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    FOR i := 1 TO Cycles DO
      AddCmd(  Cmd { V.Writ,             SI(1), SI(1)     });
    END;
    AddCmd(  Cmd { V.Read,             SI(0)     }); (* trigger a transition *)
  END WriteProgram;

  (**********************************************************************)

PROCEDURE ReadWriteProgram(prog : CommandSeq.T; <*UNUSED*>C : Lamb.T) =

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    FOR i := 1 TO Cycles DO
      AddCmd(  Cmd { V.RdWr, SI(1), SI(1), SI(2)                    });
    END;
    SharedCoda(AddCmd)
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
