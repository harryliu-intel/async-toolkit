MODULE Main;
IMPORT UDP, IP, Thread;
IMPORT Debug;
IMPORT Fmt, Text;
IMPORT AL;

CONST
  MyBasePort      = 31337;
  ClientPort      = MyBasePort;
  ServerPort      = MyBasePort + 1;
  
  MaxUdpBytes = 64*1024;
  
VAR
  myAddr := IP.GetHostAddr();

TYPE
  ServerCl = Thread.Closure OBJECT
    mu : MUTEX;
    c  : Thread.Condition;
    ready := FALSE;
  OVERRIDES
    apply := Server;
  END;

PROCEDURE Server(cl : ServerCl) : REFANY =
  <*FATAL Thread.Alerted, UDP.Timeout*>
  VAR
    udp : UDP.T;
    datagram : UDP.Datagram;
  BEGIN
    TRY
      udp := NEW(UDP.T).init(ServerPort);
    EXCEPT
      IP.Error(err) =>
      Debug.Error("Caught IP.Error creating server " & AL.Format(err));
    END;
    
    LOCK cl.mu DO
      (* we need to show, somehow, that we are ready to receive *)
      cl.ready := TRUE;
      Thread.Signal(cl.c)
    END;
    
    datagram.bytes := NEW(REF ARRAY OF CHAR, MaxUdpBytes);
    LOOP
      TRY
        udp.receive(datagram);
      EXCEPT
        IP.Error(err) =>
        Debug.Error("Server caught IP.Error listening for packet " & AL.Format(err));
      END;
      Debug.Out("Got datagram!")
    END
  END Server;

TYPE
  ClientCl = Thread.Closure OBJECT
  OVERRIDES
    apply := Client;
  END;

CONST
  NumPackets = 10;
      
PROCEDURE Client(<*UNUSED*>cl : ClientCl) : REFANY =
  VAR
    udp      : UDP.T;
    datagram : UDP.Datagram;
  BEGIN
    TRY
      udp := NEW(UDP.T).init(ClientPort);
    EXCEPT
      IP.Error(err) =>
      Debug.Error("Caught IP.Error creating client " & AL.Format(err));
    END;
          
    datagram.bytes := NEW(REF ARRAY OF CHAR, MaxUdpBytes);
    datagram.other := IP.Endpoint { addr := myAddr, port := ServerPort };
    FOR i := 0 TO NumPackets-1 DO
      (* format i in ASCII *)
      WITH str = Fmt.Int(i),
           len = Text.Length(str) DO
        Text.SetChars(datagram.bytes^, str);
        datagram.len := len
      END;

      TRY
        WITH sz = udp.send(datagram) DO
          <*ASSERT sz = datagram.len*>
        END
      EXCEPT
        IP.Error(err) =>
        Debug.Error("Client caught IP.Error sending packet " & AL.Format(err));
      END;
      Debug.Out("Sent datagram!")
    END;
    RETURN NIL
  END Client;

VAR
  cTh : Thread.T;
    
BEGIN
  WITH sCl = NEW(ServerCl, mu := NEW(MUTEX), c := NEW(Thread.Condition)),
       cCl = NEW(ClientCl) DO
    EVAL Thread.Fork(sCl);
    LOCK sCl.mu DO
      WHILE NOT sCl.ready DO
        Thread.Wait(sCl.mu, sCl.c)
      END
    END;
    cTh := Thread.Fork(cCl);
    EVAL Thread.Fork(sCl);

    (* wait for client to be done sending *)
    EVAL Thread.Join(cTh);
  END;

  (* and wait a while longer for server to receive everything *)
  Thread.Pause(5.0d0);

END Main.
