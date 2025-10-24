MODULE TcamPrograms;
IMPORT CommandSeq;
IMPORT Command, Verb;
FROM TcamSequencer IMPORT AddKey;
IMPORT Tcam;
IMPORT ParseParams;
IMPORT Debug;
IMPORT Text;

CONST TE = Text.Equal;

PROCEDURE SingleHitProgram(prog : CommandSeq.T; C : Tcam.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  0 }); (* 4.5 *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  1 }); (* 6.5 *)
    AddCmd(  Cmd { V.Nop                     });
    (* works if no more writes here *)

    AddCmd(  Cmd { V.Look,             0     });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });
  END SingleHitProgram;
    
PROCEDURE DefProgram(prog : CommandSeq.T; C : Tcam.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  0 }); (* 4.5 *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  1 }); (* 6.5 *)
    AddCmd(  Cmd { V.Nop                     });
    (* works if no more writes here *)

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Writ, 16_00c0edbabe,  2 });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ, 16_00c001d00d,  3 });
    AddCmd(  Cmd { V.Nop                     });

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Writ, 16_ffffffff00,  4 }); (* 10.5 *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  5 }); (* 12.5 *)
    AddCmd(  Cmd { V.Nop                     });

    AddCmd(  Cmd { V.Look,             0     });

    AddKey(prog, 10,        "0000000000000000000000000000000000000000", C.W); (* 16.5 *)

    AddCmd(  Cmd { V.Look,             0     }); (* 19.5 *)

    AddKey(prog, 11,        "0000000000000000000000000000000000000001", C.W);

    AddCmd(  Cmd { V.Look,             0     });

    AddKey(prog, 11,        "0000000000000000000000000000000000000?11", C.W);

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Read,             0     });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Read,             1     });
    AddCmd(  Cmd { V.Nop                     });

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000000     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000011     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000111     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000001111     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000000     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000011     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000111     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000001111     });

    (* do some illegal stuff *)
    AddCmd(  Cmd { V.Writ,             0,  0 }); (* 4.5 *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  0 }); (* 4.5 *)
    AddCmd(  Cmd { V.Look,             0     });
    AddCmd(  Cmd { V.Look,             0     });
    AddCmd(  Cmd { V.Read,             0     });
    AddCmd(  Cmd { V.Look,             0     });
    AddCmd(  Cmd { V.Look,             0     });

  END DefProgram;

(**********************************************************************)

PROCEDURE ShortProgram(prog : CommandSeq.T; C : Tcam.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  0 }); (* 4.5 *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  1 }); (* 6.5 *)
    AddCmd(  Cmd { V.Nop                     });
    (* works if no more writes here *)

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Writ, 16_ffffffff00,  4 }); (* 10.5 *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  5 }); (* 12.5 *)
    AddCmd(  Cmd { V.Nop                     });

    AddCmd(  Cmd { V.Look,             0     });

    AddKey(prog, 10,        "0000000000000000000000000000000000000000", C.W); (* 16.5 *)

    AddCmd(  Cmd { V.Look,             0     }); (* 19.5 *)

    AddKey(prog, 11,        "0000000000000000000000000000000000000001", C.W);

    AddCmd(  Cmd { V.Look,             0     });

    AddKey(prog, 11,        "0000000000000000000000000000000000000?11", C.W);

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Read,             0     });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Read,             1     });
    AddCmd(  Cmd { V.Nop                     });

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Look, 2_0000000000000000000000000000000000000000     })

  END ShortProgram;

  (**********************************************************************)

PROCEDURE AllMissProgram(prog : CommandSeq.T; C : Tcam.T) =
  TYPE
    Cmd = Command.T;
    V   = Verb.T;
  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;
  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Nop                     });
    FOR i := 0 TO C.N-1 DO
      AddCmd( Cmd { V.Writ, 0, i } ) 
    END;
    FOR i := 0 TO 100-1 DO
      AddCmd(  Cmd { V.Look,             0     });
    END;
    (* make sure we have enough sim time to get all transitions! *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });

  END AllMissProgram;

PROCEDURE ReadWriteProgram(prog : CommandSeq.T; <*UNUSED*>C : Tcam.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  0 }); 
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Read,            0,  0 });  (* should read all 1 *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  1 }); 
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  2 }); 
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  3 }); 
    AddCmd(  Cmd { V.Nop                     });

    (* now we should hit at both entry 0 and entry 1 *)

    AddCmd(  Cmd { V.Look,             0     });
    AddCmd(  Cmd { V.Writ,            0,  0 }); 
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Read,            1,  0 });  (* should read all 1 *)
    AddCmd(  Cmd { V.Nop                     });

    (* now we should miss *)
    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Read,            0,  0 }); 

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Writ,            -1,  0 }); 

    AddCmd(  Cmd { V.Look,             0     });

    AddCmd(  Cmd { V.Read,            0,  0 }); 

    (* make sure we have enough sim time to get all transitions! *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });

  END ReadWriteProgram;

PROCEDURE VarProgram(prog : CommandSeq.T; <*UNUSED*>C : Tcam.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Rset                    });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Writ,            -1,  0 }); 
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Read,            0,  0 });  (* should read all 1 *)
    (* make sure we have enough sim time to get all transitions! *)
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });
    AddCmd(  Cmd { V.Nop                     });

  END VarProgram;
  
(**********************************************************************)

PROCEDURE MinimalProgram(prog : CommandSeq.T; C : Tcam.T) =

  TYPE
    Cmd = Command.T;
    V   = Verb.T;

  PROCEDURE AddCmd(c : Cmd) =
    BEGIN prog.addhi(c) END AddCmd;

  BEGIN
    AddCmd(  Cmd { V.Rset                    });
    AddKey(prog,  0,        "0000000000000000000000000000000000000000", C.W);
    AddCmd(  Cmd { V.Look,             0     });
    AddCmd(  Cmd { V.Look,             1     });
    AddCmd(  Cmd { V.Look,             0     });
    AddCmd(  Cmd { V.Look,             1     });
  END MinimalProgram;

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
        IF NOT found THEN Debug.Error("Unknown TCAM program \"" & nm & "\"") END
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
  Progs := ARRAY OF Proc { DefProgram,
                           ShortProgram,
                           MinimalProgram,
                           ReadWriteProgram,
                           AllMissProgram,
                           VarProgram,
                           SingleHitProgram};
END TcamPrograms.
