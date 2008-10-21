(* $Id$ *)

MODULE SchemeReadLine;
IMPORT Scheme, Csighandler;
IMPORT NetObj, Thread;
IMPORT ReadLine, ReadLineError;
IMPORT SchemeInputPort, SchemeSymbol, SchemeUtils;
IMPORT Rd, AL, Debug;
IMPORT TextWr;
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
            p.rl.setPrompt("- ");
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
    Csighandler.install_int_handler();
    rl.startProc();
    rl.display("LITHP ITH LITHENING.\n");
    rl.setPrompt("> ");
    sip := NEW(InputPort).init(rl);

    scm.setInterrupter(NEW(Interrupter));

    TRY
      scm.bind(SchemeSymbol.Symbol("bang-bang"), NIL);

      LOOP
        WITH x = sip.read() DO
          rl.setPrompt("> ");
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


BEGIN END SchemeReadLine.
