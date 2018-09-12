INTERFACE ModelServerClass;
IMPORT ModelServer;
IMPORT Rd;
IMPORT Wr;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT Thread;
IMPORT NetError;
IMPORT NetContext;

TYPE
  Instance <: PubInstance;

  PubInstance = Thread.Closure OBJECT
    rd      : Rd.T;
    wr      : Wr.T;
    sp      : Pkt.T;
    t       : ModelServer.T;
    lastHdr : FmModelMessageHdr.T;
  METHODS
    sendResponse() RAISES { Wr.Failure, Thread.Alerted };
    (* send a response packet from sp, formatted WITHOUT the outer header
       of type FmModelMessageHdr.T *)
  END;

CONST Brand = "ModelServerClass";

TYPE
  MsgHandler = OBJECT METHODS
    handle(READONLY hdr : FmModelMessageHdr.T;
           VAR cx       : NetContext.T;
           inst         : Instance) 
    RAISES { NetError.OutOfRange,
             Rd.EndOfFile,
             Rd.Failure,
             ModelServer.ParseError,
             Thread.Alerted,
             Wr.Failure }
  END;

END ModelServerClass.
