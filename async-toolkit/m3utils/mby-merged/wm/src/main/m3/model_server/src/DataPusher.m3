MODULE DataPusher;
IMPORT Thread, RefSeq, Wr, Rd, TCP, IP;
IMPORT ServerPacket AS Pkt;
IMPORT ConnRW;
IMPORT Debug;
FROM Fmt IMPORT Int, F;

REVEAL
  T = Public BRANDED Brand OBJECT
    mu       : MUTEX;
    c        : Thread.Condition;
    data     : RefSeq.T;
    wr       : Wr.T;
    rd       : Rd.T;
    conn     : TCP.T;
    hostname : TEXT;
    port     : IP.Port;
    dead     := FALSE;
  OVERRIDES
    init         := Init;
    exitCallback := NullCallback;
    forceExit    := ForceExit;
    push         := Push;
  END;

PROCEDURE Init(t        : T;
               hostname : TEXT;
               tcpPort  : IP.Port) : T
  RAISES { IP.Error, Thread.Alerted } =
  BEGIN
    t.mu       := NEW(MUTEX);
    t.c        := NEW(Thread.Condition);
    t.data     := NEW(RefSeq.T).init();
    t.hostname := hostname;
    t.port     := tcpPort;
    VAR
      addr : IP.Address;
      ep   : IP.Endpoint;
    BEGIN
      IF NOT IP.GetHostByName(hostname, addr) THEN
        RAISE IP.Error(NIL)
      END;
      ep.addr := addr;
      ep.port := tcpPort;
      t.conn    := TCP.Connect(ep);
      t.wr    := ConnRW.NewWr(t.conn);
      t.rd    := ConnRW.NewRd(t.conn);
    END;
    EVAL Thread.Fork(NEW(Closure  , t := t));
    EVAL Thread.Fork(NEW(RdClosure, t := t));
    RETURN t
  END Init;

PROCEDURE NullCallback(<*UNUSED*>t : T) = BEGIN END NullCallback;
  
PROCEDURE Push(t : T; packet : Pkt.T) =
  BEGIN
    LOCK t.mu DO
      t.data.addhi(packet);
      Thread.Signal(t.c)
    END
  END Push;

TYPE
  Closure = Thread.Closure OBJECT
    t : T;
  OVERRIDES
    apply := Apply;
  END;

  RdClosure = Thread.Closure OBJECT
    t : T;
  OVERRIDES
    apply := RdApply;
  END;

PROCEDURE Apply(cl : Closure) : REFANY =
  VAR
    d : Pkt.T;
  BEGIN
    TRY
      LOOP
        LOCK cl.t.mu DO
          WHILE NOT cl.t.dead AND cl.t.data.size() = 0 DO
            Thread.Wait(cl.t.mu, cl.t.c)
          END;
          IF cl.t.dead THEN RAISE Thread.Alerted END;
          d := cl.t.data.remlo()
        END;
        Pkt.Transmit(d, cl.t.wr)
      END
    EXCEPT
      Wr.Failure, Thread.Alerted =>
      Debug.Out(F("DataPusher(%s:%s) exiting", cl.t.hostname, Int(cl.t.port)));
      IF NOT cl.t.dead THEN cl.t.exitCallback() END;
      TRY Wr.Close(cl.t.wr) EXCEPT ELSE END;
      TRY Rd.Close(cl.t.rd) EXCEPT ELSE END;
      TCP.Close(cl.t.conn)
    END;
    RETURN NIL
  END Apply;

PROCEDURE ForceExit(t : T) =
  BEGIN
    TRY Wr.Close(t.wr) EXCEPT ELSE END;
    TRY Rd.Close(t.rd) EXCEPT ELSE END;
    LOCK t.mu DO
      t.data := t.data.init();
      t.dead := TRUE;
      Thread.Signal(t.c)
    END
  END ForceExit;

PROCEDURE RdApply(cl : RdClosure) : REFANY =
  BEGIN
    TRY
      LOOP
        EVAL Rd.GetChar(cl.t.rd)
      END
    EXCEPT
    ELSE (* skip *)
    END;
    RETURN NIL
  END RdApply;

BEGIN END DataPusher.
