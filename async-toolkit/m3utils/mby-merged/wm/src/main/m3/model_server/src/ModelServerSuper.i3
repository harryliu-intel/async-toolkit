INTERFACE ModelServerSuper;

IMPORT Thread;
IMPORT CsrOp, CsrAccessStatus;

IMPORT FmModelMsgType, Wr;
IMPORT Rd, NetError, FmModelMessageHdr;
IMPORT ServerPacket AS Pkt;
IMPORT NetContext;
IMPORT Pathname;

TYPE
  T <: Public;
  
  Public = OBJECT
    quitOnLastClientExit : BOOLEAN;
  METHODS
    listenFork() : Listener;
    (* fork a listener on an arbitrarily chosen port *)

    csrOp(VAR op : CsrOp.T) : CsrAccessStatus.T;
    (* perform a CSR operation as requested.
       if a read, the read results are returned in the op itself. *)

    init(infoPath, infoFile : Pathname.T;
         READONLY handler : ARRAY FmModelMsgType.T OF MsgHandler) : T;
  END;

  Listener <: PubListener;

  PubListener = Thread.Closure OBJECT END;

  MsgHandler = OBJECT METHODS
    handle(READONLY hdr : FmModelMessageHdr.T;
           VAR cx       : NetContext.T;
           inst         : Instance) 
    RAISES { NetError.OutOfRange,
             Rd.EndOfFile,
             Rd.Failure,
             ParseError,
             Thread.Alerted,
             Wr.Failure }
  END;

  Instance <: PubInstance;

  PubInstance = Thread.Closure OBJECT
    t       : T;
    rd      : Rd.T;
    wr      : Wr.T;
    sp      : Pkt.T;
    lastHdr : FmModelMessageHdr.T;
  METHODS
    sendResponse() RAISES { Wr.Failure, Thread.Alerted };
    (* send a response packet from sp, formatted WITHOUT the outer header
       of type FmModelMessageHdr.T *)
  END;
  
EXCEPTION ParseError(TEXT);
          
CONST Brand = "ModelServerSuper";
      
END ModelServerSuper.
