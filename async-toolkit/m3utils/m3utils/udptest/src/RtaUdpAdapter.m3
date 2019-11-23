MODULE RtaUdpAdapter;
IMPORT Thread;
IMPORT UdpAdapter;
IMPORT IP;
FROM UDP IMPORT Datagram, Timeout;
IMPORT NestedUdpAdapter;
IMPORT CardRefTbl;
IMPORT TCP;

REVEAL
  T = Public BRANDED Brand OBJECT
    underlying            : UdpAdapter.T;
    myEp                  : IP.Endpoint;
    man                   : Manager;
  METHODS
  OVERRIDES
    init      := Init;
    send      := Send;
    sendText  := SendText;
    receive   := Receive;
    close     := Close;
  END;

PROCEDURE Init(t            : T;
               myPort       : IP.Port;
               myAddr       : IP.Address;
               underlying   : UdpAdapter.T) : NestedUdpAdapter.T
  RAISES { IP.Error } =
  BEGIN
    t.underlying := underlying.init(myPort, myAddr);
    t.myEp := IP.Endpoint { addr := myAddr, port := myPort };
    t.man := GetManager(t);
    RETURN t
  END Init;
  
  (* default implementations follow *)

PROCEDURE Send(t : T; READONLY d : Datagram) : INTEGER
  RAISES { IP.Error } =
  BEGIN
    IF t.man # NIL THEN
      RETURN t.man.send(t.myEp, d)
    ELSE
      RETURN t.underlying.send(d)
    END
  END Send;

PROCEDURE SendText(t : T; READONLY other: IP.Endpoint; txt : TEXT): INTEGER
  RAISES { IP.Error } =
  BEGIN
    IF t.man # NIL THEN
      RETURN t.man.sendText(t.myEp, other, txt)
    ELSE
      RETURN t.underlying.sendText(other, txt)
    END
  END SendText;

PROCEDURE Receive(t : T; VAR (*INOUT*) d: Datagram; timeout: LONGREAL)
  RAISES { Timeout, IP.Error, Thread.Alerted } =
  BEGIN
    IF t.man # NIL THEN
      t.man.receive(t.myEp, d, timeout)
    ELSE
      t.underlying.receive(d, timeout)
    END
  END Receive;
   
PROCEDURE Close(t : T)
  RAISES { IP.Error } =
  BEGIN
    IF t.man # NIL THEN
      t.man.close(t.myEp)
    ELSE
      t.underlying.close()
    END
  END Close;

  (***********************************************************************)

PROCEDURE GetManager(t : T) : Manager =
  BEGIN
    IF managers = NIL THEN RETURN NIL END;
    FOR i := FIRST(managers^) TO LAST(managers^) DO
      WITH m    = managers[i],
           myEp = t.myEp DO
        IF myEp.port >= m.portRange.basePort AND
          myEp.port <  m.portRange.basePort + m.portRange.size THEN
          RETURN m
        END
      END
    END;
    RETURN NIL
  END GetManager;
 
TYPE
  Manager = Thread.Closure OBJECT
    portRange     : PortRange;
    mu            : MUTEX;
    c             : Thread.Condition;
    ready     := FALSE;
    underlyingTbl : CardRefTbl.T;
    tcp           : TCP.Connector;
  METHODS
    init(portRange : PortRange; tcp : TCP.Connector) : Manager := InitManager;
    
    (* must implement these! *)
    
    send(myEp : IP.Endpoint; READONLY d: Datagram): INTEGER
      RAISES {IP.Error};
    
    sendText(myEp : IP.Endpoint; READONLY other: IP.Endpoint; t: TEXT): INTEGER
      RAISES {IP.Error};

    receive(myEp : IP.Endpoint; VAR (*INOUT*) d: Datagram; timeout: LONGREAL := -1.0d0)
      RAISES {Timeout, IP.Error, Thread.Alerted};

    close(myEp : IP.Endpoint)
      RAISES {IP.Error};

  OVERRIDES
    apply := ManagerApply;
  END;

PROCEDURE InitManager(m : Manager; portRange : PortRange; tcp : TCP.Connector) : Manager =
  BEGIN
    m.portRange := portRange;
    m.mu := NEW(MUTEX);
    m.c := NEW(Thread.Condition);
    m.underlyingTbl := NEW(CardRefTbl.Default).init();
    m.tcp := tcp;
    RETURN m
  END InitManager;

PROCEDURE ManagerApply(cl : Manager) : REFANY =
  BEGIN
    LOOP
      Thread.Pause(1.0d0);
      LOCK cl.mu DO
        cl.ready := TRUE;
        Thread.Signal(cl.c)
      END
    END
  END ManagerApply;
  
PROCEDURE StartRtaManager(READONLY portRange : PortRange) : Manager
  RAISES { IP.Error } =
  BEGIN
    WITH manEp = IP.Endpoint { IP.NullAddress, portRange.listen },
         tcp   = TCP.NewConnector(manEp),
         manager = NEW(Manager).init(portRange, tcp) DO
      EVAL Thread.Fork(manager);
      LOCK manager.mu DO
        WHILE NOT manager.ready DO Thread.Wait(manager.mu, manager.c) END
      END;
      RETURN manager
    END
  END StartRtaManager;

VAR managers : REF ARRAY OF Manager := NIL;
  
PROCEDURE Initialize(READONLY portRanges : PortRanges)
  RAISES { IP.Error } =
  BEGIN
    managers := NEW(REF ARRAY OF Manager, NUMBER(portRanges));
    FOR i := FIRST(portRanges) TO LAST(portRanges) DO
      managers[i] := StartRtaManager(portRanges[i])
    END
  END Initialize;
  
BEGIN END RtaUdpAdapter.
