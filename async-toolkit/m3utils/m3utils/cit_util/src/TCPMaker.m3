(* $Id$ *)

MODULE TCPMaker;
IMPORT IP;
IMPORT Scan, Debug, Fmt, TCP, Thread;
IMPORT AL;
IMPORT TextReader;
IMPORT FloatMode, Lex;

PROCEDURE MakeMaker(maker : Default;
                    nameString : TEXT;
                    defaultPort : IP.Port) : Default RAISES { ConnErr } =
  VAR
    addr : IP.Address;
    ep : IP.Endpoint;
    port := defaultPort;
  BEGIN
    TRY
      VAR
        nameReader := NEW(TextReader.T).init(nameString);
        hostName := nameReader.nextE(":");
      BEGIN
        IF NOT nameReader.isEmpty() THEN
          port := Scan.Int(nameReader.nextE(":"))
        END;
        
        TRY 
          IF NOT IP.GetHostByName(hostName, addr) THEN
            RAISE IP.Error(NIL)
          END;
          Debug.Out("Attempting to connect to " & hostName & " port " &
            Fmt.Int(port));
          
          ep.addr := addr;
          ep.port := port;
          
          maker.ep := ep;

          RETURN maker
        EXCEPT
          IP.Error(err) => 
          RAISE ConnErr("Internet Protocol error:"&AL.Format(err)&" \n  Is host \"" & hostName & 
            "\" for the server at all sensible?")
        END
      END
    EXCEPT
      TextReader.NoMore => RAISE ConnErr("Null  server name.")
    |
      FloatMode.Trap, Lex.Error => 
      RAISE ConnErr("Non-numeric port name specified for .")
    END
  END MakeMaker;

REVEAL
  Default = PubDefault BRANDED Brand & " Default" OBJECT
    ep : IP.Endpoint;
  OVERRIDES
    makeTCP := MMT;
    init := MakeMaker;
  END;

PROCEDURE MMT(m : Default) : TCP.T RAISES { IP.Error } = 
  <* FATAL Thread.Alerted *>
  BEGIN RETURN TCP.Connect(m.ep) END MMT;
    
BEGIN END TCPMaker.
