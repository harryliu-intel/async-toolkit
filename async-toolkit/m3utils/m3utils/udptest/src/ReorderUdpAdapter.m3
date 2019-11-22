MODULE ReorderUdpAdapter;
IMPORT UdpDatagramSeq;
IMPORT IP;
IMPORT Random;
IMPORT UDP;

REVEAL
  T = Public BRANDED Brand OBJECT
    reorderProb, dropProb : LONGREAL;
    buff                  : UdpDatagramSeq.T;
    rand                  : Random.T;
  OVERRIDES
    setParams := SetParams;
    eot       := Eot;
    init      := Init;
  END;

PROCEDURE SetParams(t : T;
                    reorderProb, dropProb : LONGREAL;
                    rand : Random.T) =
  BEGIN
    t.reorderProb := reorderProb;
    t.dropProb    := dropProb;
    IF rand # NIL THEN
      t.rand := rand
    END;
  END SetParams;

PROCEDURE Eot(t : T) RAISES { IP.Error } =
  BEGIN
    (* push any packets still in the buffer *)
  END Eot;

PROCEDURE Init(t : T; myPort: IP.Port; myAddr : IP.Address) : UDP.T
  RAISES { IP.Error } =
  BEGIN
    EVAL UDP.T.init(t, myPort, myAddr);

    (* alloc a default Random.T and a DatagramSeq *)
    t.buff := NEW(UdpDatagramSeq.T).init();
    t.rand := NEW(Random.Default).init();
    RETURN t
  END Init;
  

BEGIN END ReorderUdpAdapter.
