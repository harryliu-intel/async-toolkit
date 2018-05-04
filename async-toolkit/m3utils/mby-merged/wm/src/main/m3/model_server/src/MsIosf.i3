INTERFACE MsIosf;
IMPORT ModelServer;
IMPORT ModelServerClass;
IMPORT FmModelMessageHdr;
IMPORT NetContext;
IMPORT Rd, Wr, Thread;

PROCEDURE HandleMsg(m            : ModelServerClass.MsgHandler;
                    READONLY hdr : FmModelMessageHdr.T;
                    VAR cx       : NetContext.T;
                    inst         : ModelServerClass.Instance)
  RAISES { Rd.EndOfFile, Rd.Failure, ModelServer.ParseError, Thread.Alerted, Wr.Failure };

CONST Brand = "MsIosf";
        
END MsIosf.
