MODULE CspWorker;
IMPORT TCP;
IMPORT IP;

REVEAL
  T = Public BRANDED Brand OBJECT
    conn : TCP.Connector;
    ep   : IP.Endpoint;
  OVERRIDES
    init  := Init;
    getEp := GetEp;
  END;

PROCEDURE Init(t : T) : T =
  BEGIN
    WITH nullEp = IP.Endpoint { IP.NullAddress, IP.NullPort } DO
      t.conn := TCP.NewConnector(nullEp)
    END;
    t.ep   := TCP.GetEndPoint(t.conn);
    RETURN t
  END Init;

PROCEDURE GetEp(t : T) : IP.Endpoint =
  BEGIN
    RETURN t.ep
  END GetEp;
  
BEGIN END CspWorker.
