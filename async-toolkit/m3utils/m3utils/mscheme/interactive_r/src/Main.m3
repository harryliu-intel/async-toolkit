(* $Id$ *)

(*
  Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.

  Author: Mika Nystrom <mika@alum.mit.edu>
*)


MODULE Main;
IMPORT Scheme, Params, Pathname, Csighandler;
IMPORT IP, NetObj, Thread;
IMPORT ReadLine, ReadLineError;
IMPORT SchemeInputPort, SchemeSymbol, SchemeUtils;
IMPORT Rd, AL, Debug;
IMPORT TextWr;
IMPORT OSError;
IMPORT Text, IntSeq;

<* FATAL Thread.Alerted *>

TYPE 
  Interrupter = Scheme.Interrupter OBJECT
  OVERRIDES
    interrupt := Interrupt;
  END;

PROCEDURE Interrupt(<*UNUSED*>i : Interrupter) : BOOLEAN =
  BEGIN
    IF Csighandler.have_signal() = 1 THEN 
      Csighandler.clear_signal();
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END Interrupt;

TYPE 
  InputPort = SchemeInputPort.T OBJECT
    rl : ReadLine.T;
    buff : IntSeq.T;
  METHODS
    init(rl : ReadLine.T) : InputPort:= InitPort; (* shadows normal init, whch
                                                     isnt called at all *)
  OVERRIDES
    getCh := GetCh;
    (* should override close, too *)
  END;

PROCEDURE InitPort(p : InputPort; rl : ReadLine.T) : InputPort =
  BEGIN
    p.rl := rl;
    p.buff := NEW(IntSeq.T).init();
    RETURN p
  END InitPort;

CONST DebugALL = FALSE;

PROCEDURE GetCh(p : InputPort) : INTEGER =
  BEGIN
    LOOP
      IF p.buff.size() > 0 THEN 
        WITH ch = p.buff.remlo() DO
          IF DebugALL THEN
            IF ch = SchemeInputPort.ChEOF THEN
              Debug.Out("GetCh = EOF")
            ELSE
              Debug.Out("GetCh = " & Text.FromChar(VAL(ch,CHAR)))
            END
          END;
          RETURN ch
        END
      END;
      
      WHILE p.buff.size() = 0 DO
        TRY
          WITH txt = p.rl.readLine() DO
            FOR i := 0 TO Text.Length(txt) - 1 DO
              p.buff.addhi(ORD(Text.GetChar(txt,i)))
            END;
            p.buff.addhi(ORD('\n'))
          END
        EXCEPT
          Rd.EndOfFile => p.buff.addhi(SchemeInputPort.ChEOF)
        |
          ReadLineError.E(err) => 
          Debug.Warning("Caught ReadLineError.E : " & AL.Format(err));
          p.buff.addhi(SchemeInputPort.ChEOF)
        |
          NetObj.Error(err) => Debug.Warning("Caught NetObj.Error : " & 
            AL.Format(err));
          p.buff.addhi(SchemeInputPort.ChEOF)
          
        END
      END
    END
  END GetCh;
  
PROCEDURE MainLoop(rl : ReadLine.T; scm : Scheme.T) RAISES { NetObj.Error,
                                                             ReadLineError.E }=
  VAR
    sip : SchemeInputPort.T;
  BEGIN
    rl.startProc();
    rl.display("M-Scheme interpreter (readline) ready.\n");
    rl.setPrompt(">");
    sip := NEW(InputPort).init(rl);

    scm.setInterrupter(NEW(Interrupter));

    TRY
      scm.bind(SchemeSymbol.Symbol("bang-bang"), NIL);

      LOOP
        WITH x = sip.read() DO
          IF SchemeInputPort.IsEOF(x) THEN RETURN END;
          TRY
            IF DebugALL THEN Debug.Out("Eval!") END;
            Csighandler.clear_signal();
            WITH res = scm.evalInGlobalEnv(x) DO
              WITH wr = NEW(TextWr.T).init() DO
                EVAL SchemeUtils.Write(res, wr, TRUE);
                rl.display(TextWr.ToText(wr) & "\n")
              END;
              scm.setInGlobalEnv(SchemeSymbol.Symbol("bang-bang"),res)
            END
          EXCEPT
            Scheme.E(e) => rl.display("EXCEPTION! " & e & "\n")
          END
        END
      END
    EXCEPT
      
      Scheme.E(e) =>
      (* only way we can get here is if we have a failure in t.input.read() *)
      TRY rl.display("READ FAILURE : "&e&" .\n") EXCEPT ELSE END;
      RETURN
    END
  END MainLoop;

BEGIN 
  Csighandler.install_int_handler();

  WITH arr = NEW(REF ARRAY OF Pathname.T, Params.Count-1) DO
    FOR i := 1 TO Params.Count-1 DO arr[i-1] := Params.Get(i) END;
    TRY
      WITH scm = NEW(Scheme.T).init(arr^) DO
        MainLoop(NEW(ReadLine.T).init(), scm)
      END
    EXCEPT
      Scheme.E(err) => Debug.Error("Caught Scheme.E : " & err)
    |
      IP.Error(err) => Debug.Error("Caught IP.Error : " & AL.Format(err))
    |
      OSError.E(err) => 
        Debug.Error("Caught NetObj.Error : " & AL.Format(err))
    |
      ReadLineError.E(err) => 
        Debug.Error("Caught ReadLineError.E : " & AL.Format(err))
    |
      NetObj.Error(err) => Debug.Error("Caught NetObj.Error : " & 
                                        AL.Format(err))
    END
  END

END Main.
