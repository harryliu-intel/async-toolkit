MODULE ReorderUdpAdapter;
IMPORT RefSeq;
IMPORT IP;
IMPORT Random;
IMPORT UDP;
IMPORT UdpAdapter;
FROM UDP IMPORT Datagram, Timeout;
IMPORT Thread;
IMPORT Text;
IMPORT Debug;
IMPORT NestedUdpAdapter;

REVEAL
  T = Public BRANDED Brand OBJECT
    underlying            : UdpAdapter.T;
    reorderProb, dropProb, dupProb
                          : LONGREAL;
    buff                  : RefSeq.T;
    rand                  : Random.T;
  METHODS
    decide() : Decision              := Decide;
    pushBuffer() RAISES { IP.Error } := PushBuffer;
  OVERRIDES
    setParams := SetParams;
    init      := Init;
    send      := Send;
    sendText  := SendText;
    receive   := Receive;
    close     := Close;
  END;

PROCEDURE Init(t           : T;
               myPort      : IP.Port;
               myAddr      : IP.Address;
               underlying  : UdpAdapter.T) : NestedUdpAdapter.T
  RAISES { IP.Error } =
  BEGIN
    IF underlying = NIL THEN
      t.underlying := NEW(UdpAdapter.Default).init(myPort, myAddr)
    ELSE
      t.underlying := underlying.init(myPort, myAddr)
    END;
    
    (* alloc a default Random.T and a DatagramSeq *)
    t.buff := NEW(RefSeq.T).init();
    t.rand := NEW(Random.Default).init();
    t.reorderProb := 0.0d0;
    t.dropProb    := 0.0d0;
    t.dupProb     := 0.0d0;
    RETURN t
  END Init;

PROCEDURE SetParams(t                              : T;
                    reorderProb, dropProb, dupProb : LONGREAL;
                    rand                           : Random.T) : T =
  BEGIN
    t.reorderProb := reorderProb;
    t.dropProb    := dropProb;
    t.dupProb     := dupProb;
    IF rand # NIL THEN
      t.rand := rand
    END;
    RETURN t
  END SetParams;

TYPE Decision = { Go, Drop, Reorder, Dup };   

PROCEDURE Decide(t : T) : Decision =
  VAR
    res : Decision;
  BEGIN
    IF    t.dropProb    #0.0d0 AND t.rand.longreal(0.0d0,1.0d0) < t.dropProb    THEN
      res := Decision.Drop
    ELSIF t.dupProb     #0.0d0 AND t.rand.longreal(0.0d0,1.0d0) < t.dupProb     THEN
      res := Decision.Dup
    ELSIF t.reorderProb #0.0d0 AND t.rand.longreal(0.0d0,1.0d0) < t.reorderProb THEN
      res := Decision.Reorder
    ELSE
      res := Decision.Go
    END;
    RETURN res
  END Decide;

TYPE
  Rec = BRANDED OBJECT END;

  DatagramRec = Rec OBJECT
    d : Datagram;
  END;

  TextRec = Rec OBJECT
    other : IP.Endpoint;
    txt   : TEXT;
  END;
     
PROCEDURE Send(t : T; READONLY d : Datagram) : INTEGER
  RAISES { IP.Error } =
  BEGIN
    CASE t.decide() OF
      Decision.Drop =>
      RETURN d.len
    |
      Decision.Dup =>
      EVAL Send(t, d);
      RETURN Send(t, d)
    |
      Decision.Reorder =>
      (* copy the datagram's internal buffer *)
      VAR
        dd := d;
      BEGIN
        dd.bytes := NEW(REF ARRAY OF CHAR, d.len);
        dd.bytes^ := SUBARRAY(d.bytes^, 0, d.len);
        t.buff.addlo(NEW(DatagramRec, d := dd))
      END;
      RETURN d.len
    |
      Decision.Go =>
      t.pushBuffer();
      RETURN t.underlying.send(d)
    END
  END Send;

PROCEDURE PushBuffer(t : T)
  RAISES { IP.Error } =
  BEGIN
    FOR i := 0 TO t.buff.size()-1 DO
      WITH r = t.buff.get(i) DO
        TYPECASE r OF
          DatagramRec(dr) =>
          WITH res = t.underlying.send(dr.d) DO
            IF res # dr.d.len THEN
              Debug.Warning("send size mismatch for DatagramRec")
            END
          END
        |
          TextRec(tr) =>
          WITH res = t.underlying.sendText(tr.other, tr.txt) DO
            IF res # Text.Length(tr.txt) THEN
              Debug.Warning("send size mismatch for TextRec")
            END
          END
        ELSE
          <*ASSERT FALSE*>
        END
      END
    END;
    EVAL t.buff.init() (* clear buffer *)
  END PushBuffer;

PROCEDURE SendText(t : T; READONLY other: IP.Endpoint; txt : TEXT): INTEGER
  RAISES { IP.Error } =
  BEGIN
    CASE t.decide() OF
      Decision.Drop =>
      RETURN Text.Length(txt)
    |
      Decision.Dup =>
      EVAL SendText(t, other, txt);
      RETURN SendText(t, other, txt)
    |
      Decision.Reorder =>
      t.buff.addlo(NEW(TextRec, other := other, txt := txt));
      RETURN Text.Length(txt)
    |
      Decision.Go =>
      t.pushBuffer();
      RETURN t.underlying.sendText(other, txt)
    END
  END SendText;

PROCEDURE Receive(t : T; VAR (*INOUT*) d: Datagram; timeout: LONGREAL)
  RAISES { Timeout, IP.Error, Thread.Alerted } =
  BEGIN
    t.underlying.receive(d, timeout)
  END Receive;

PROCEDURE Close(t : T)
  RAISES { IP.Error } =
  BEGIN
    t.pushBuffer();
    t.underlying.close()
  END Close;
  
BEGIN END ReorderUdpAdapter.
