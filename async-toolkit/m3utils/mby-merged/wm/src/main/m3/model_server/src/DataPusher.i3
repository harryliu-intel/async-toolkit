INTERFACE DataPusher;
IMPORT ServerPacket;
IMPORT IP, Thread;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(hostname : TEXT; tcpPort : IP.Port) : T
      RAISES { IP.Error, Thread.Alerted } ;
    push(packet : ServerPacket.T);
    forceExit();
    exitCallback();
  END;

CONST Brand = "DataPusher";

END DataPusher.
