INTERFACE MsIosf;
IMPORT ModelServer;
IMPORT ModelServerClass;
IMPORT FmModelMessageHdr;
IMPORT NetContext;
IMPORT Rd, Wr, Thread;

(* 
   White Model Model Server 

   IOSF message handling module:

   turns IOSF read, write, block read, block write 

   into CSR accesses 

   per HLP HAS, with one exception : top EH bit NOT required to be set to
   garner attention of this module.  (Per discussion with Andrea Grandi,
   April 2018) 

   Author : Mika Nystrom <mika.nystroem@intel.com>
   April, 2018
*)


PROCEDURE HandleMsg(m            : ModelServerClass.MsgHandler;
                    READONLY hdr : FmModelMessageHdr.T;
                    VAR cx       : NetContext.T;
                    inst         : ModelServerClass.Instance)
  RAISES { Rd.EndOfFile, Rd.Failure, ModelServer.ParseError, Thread.Alerted, Wr.Failure };

CONST Brand = "MsIosf";
        
END MsIosf.
