MODULE ModelServer;
IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT CompAddr;
IMPORT Debug;
IMPORT Thread;
FROM Fmt IMPORT F;
IMPORT Rd, Wr, IP, TCP;
IMPORT RefSeq;
IMPORT ConnRW;
IMPORT Fmt;

REVEAL
  T = Public BRANDED Brand OBJECT
    h        : MapAddr.H;
    port     : IP.Port;
    conn     : TCP.Connector; (* listen here *)
    listener : Listener := NIL;
  OVERRIDES
    init       := Init;
    resetChip  := ResetChip;
    listenFork := ListenFork;
  END;

PROCEDURE ListenFork(t : T) : Listener =
  BEGIN
    WITH listener = NEW(Listener,
                        handlers := NEW(RefSeq.T).init(),
                        conn := t.conn) DO
      EVAL Thread.Fork(listener);
      RETURN listener
    END
  END ListenFork;

REVEAL
  Listener = PubListener BRANDED Brand & " Listener" OBJECT
    handlers : RefSeq.T;
    conn : TCP.Connector;
  OVERRIDES
    apply := LCApply;
  END;

TYPE
  Handler = Thread.Closure OBJECT
    rd : Rd.T;
    wr : Wr.T;
  OVERRIDES
    apply := HApply;
  END;

PROCEDURE LCApply(cl : Listener) : REFANY =
  BEGIN
    LOOP
      WITH tcp = TCP.Accept(cl.conn),
           wr = ConnRW.NewWr(tcp),
           rd = ConnRW.NewRd(tcp),
           h  = NEW(Handler, rd := rd, wr := wr) DO
        cl.handlers.addhi(h);
        EVAL Thread.Fork(h)
      END
    END
  END LCApply;

PROCEDURE Init(t : T) : T =
  BEGIN
    WITH ep   = IP.Endpoint { IP.NullAddress, IP.NullPort },
         conn = TCP.NewConnector(ep) DO
      t.port := TCP.GetEndPoint(conn).port;
      Debug.Out("PORT " & Fmt.Int(t.port));
      t.conn := conn
    END;
    Debug.Out(F("Creating %s ...",Map.Brand));
    t.h := NEW(MapAddr.H).init(CompAddr.Zero);
    RETURN t
  END Init;

PROCEDURE ResetChip(t : T) =
  BEGIN MapAddr.Reset(t.h.read, t.h.update) END ResetChip;

PROCEDURE HApply(cl : Handler) : REFANY =
  BEGIN
    Debug.Out("HApply");
    LOOP
      Thread.Pause(1.0d0)
    END
  END HApply;

BEGIN END ModelServer.
