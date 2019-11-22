MODULE Main;
IMPORT UDP, IP, Thread;
IMPORT Debug;
IMPORT Fmt, Text;
IMPORT AL;
IMPORT UdpAdapter;

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
    ready := -1;
    port : IP.Port;
  OVERRIDES
    apply := Server;
  END;

PROCEDURE Server(cl : ServerCl) : REFANY =
  <*FATAL Thread.Alerted, UDP.Timeout*>
  VAR
    udp      : UdpAdapter.T;
    datagram : UDP.Datagram;
  BEGIN
    Debug.Out("Creating server");
    TRY
      udp := NEW(UdpAdapter.Default).init(cl.port);
    EXCEPT
      IP.Error(err) =>
      Debug.Error("Caught IP.Error creating server " & AL.Format(err));
    END;
    
    LOCK cl.mu DO
      (* we need to show, somehow, that we are ready to receive *)
      cl.ready := cl.port;
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
      Debug.Out("Got datagram: " & FmtDatagram(datagram))
    END
  END Server;

TYPE
  ClientCl = Thread.Closure OBJECT
    port   : IP.Port;
    other  : IP.Endpoint;
  OVERRIDES
    apply := Client;
  END;

CONST
  NumPackets = 10;
      
PROCEDURE Client(cl : ClientCl) : REFANY =
  VAR
    udp      : UdpAdapter.T;
    datagram : UDP.Datagram;
  BEGIN
    Debug.Out("Creating client");
    TRY
      udp := NEW(UdpAdapter.Default).init(cl.port);
    EXCEPT
      IP.Error(err) =>
      Debug.Error("Caught IP.Error creating client " & AL.Format(err));
    END;
          
    datagram.bytes := NEW(REF ARRAY OF CHAR, MaxUdpBytes);
    datagram.other := cl.other;
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
      Debug.Out("Sent datagram: " & FmtDatagram(datagram))
    END;
    RETURN NIL
  END Client;

PROCEDURE FmtDatagram(d : UDP.Datagram) : TEXT =
  BEGIN
    RETURN Fmt.F("ep={%s} len=%s txt=\"%s\"",
                 FmtEndpoint(d.other),
                 Fmt.Int(d.len),
                 Text.FromChars(SUBARRAY(d.bytes^,0,d.len)))
  END FmtDatagram;

PROCEDURE FmtEndpoint(ep : IP.Endpoint) : TEXT =
  BEGIN
    RETURN Fmt.F("a={%s,%s,%s,%s} p=%s",
                 Fmt.Int(ep.addr.a[0]),
                 Fmt.Int(ep.addr.a[1]),
                 Fmt.Int(ep.addr.a[2]),
                 Fmt.Int(ep.addr.a[3]),
                 Fmt.Int(ep.port))
  END FmtEndpoint;
  
VAR
  cTh : Thread.T;
    
BEGIN
  WITH sCl = NEW(ServerCl, mu := NEW(MUTEX), c := NEW(Thread.Condition),
                 port := ServerPort),
       cCl = NEW(ClientCl,
                 port := ClientPort,
                 other := IP.Endpoint { addr := myAddr, port := ServerPort }) DO
    EVAL Thread.Fork(sCl);
    LOCK sCl.mu DO
      WHILE sCl.ready < 0 DO
        Thread.Wait(sCl.mu, sCl.c)
      END
    END;
    cTh := Thread.Fork(cCl);

    (* wait for client to be done sending *)
    EVAL Thread.Join(cTh);
  END;

  (* and wait a while longer for server to receive everything *)
  Thread.Pause(5.0d0);

END Main.
