MODULE MbyModel;

IMPORT mby_top_map AS Map;
IMPORT mby_top_map_addr AS MapAddr;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT FmModelConstants;
IMPORT MbyModelServer;
IMPORT Debug;
IMPORT ModelServer;
IMPORT Coroutine;
IMPORT MbyParserStage;
IMPORT BaseModelStage;
IMPORT Metadata;

TYPE
  Mode = { Reflect, Flood, Pipe };

PROCEDURE HandlePacket(server           : MbyModelServer.T;
                       READONLY readA   : Map.T;
                       READONLY updateA : MapAddr.Update;
                       READONLY hdr     : FmModelMessageHdr.T;
                       pkt              : Pkt.T) =
  VAR
    sender : Sender := NIL;
    mode := Mode.Reflect;
  BEGIN
    CASE mode OF
      Mode.Flood => FloodPktHandler(hdr, pkt, sender)
    |
      Mode.Reflect =>
      (* simplest thing ever : just reflect the packet *)
      server.pushPacket(hdr, pkt)
    |
      Mode.Pipe =>
      IF pipe = NIL THEN InitPipe(server) END;
      WITH is = NARROW(pipe[FIRST(pipe^)],InitialStage) DO
        is.pkt := pkt;
        is.ready := TRUE
      END;
      LOOP
        VAR
          opkt : Pkt.T;
          ometa : Metadata.T;
          gotOne := pipe[LAST(pipe^)].poll(opkt, ometa);
        BEGIN
          IF gotOne THEN
            server.pushPacket(hdr (* wrong *),
                              opkt)
          ELSE
            EXIT (* done for now *)
          END
        END
      END(*POOL*) 
    END
  END HandlePacket;

VAR pipe : REF ARRAY OF BaseModelStage.T := NIL;

TYPE
  InitialStage = BaseModelStage.T OBJECT
    pkt : Pkt.T;
    ready := FALSE;
  OVERRIDES
    poll := ISPoll;
  END;

PROCEDURE ISPoll(is : InitialStage;
                 VAR out : Pkt.T; VAR meta : Metadata.T) : BOOLEAN =
  BEGIN
    IF is.ready THEN
      out := is.pkt;
      meta := NIL; (* ?? well we don't receive metadata, that's part of the 
                      point, no? *)
    END;
    RETURN is.ready
  END ISPoll;
  
PROCEDURE InitPipe(server : MbyModelServer.T) =
  BEGIN
    pipe := NEW(REF ARRAY OF BaseModelStage.T, 2);
    pipe[0] := NEW(InitialStage);
    pipe[1] := NEW(MbyParserStage.T).init(server.h, pipe[0])
  END InitPipe;

TYPE Sender = PROCEDURE(READONLY hHdr : FmModelMessageHdr.T;
                        ePkt : Pkt.T);

PROCEDURE FloodPktHandler(READONLY hdr : FmModelMessageHdr.T;
                          pkt          : Pkt.T;
                          sender       : Sender) =
  BEGIN
    FOR i := 0 TO FmModelConstants.NPhysPorts-1 DO
      IF i # hdr.port THEN
        (* send to all ports except ingress port *)
        VAR
          eHdr := hdr;
        BEGIN
          eHdr.port := i;
          sender(eHdr, pkt)
        END
      END
    END
  END FloodPktHandler;

PROCEDURE SetupMby(<*UNUSED*>server : MbyModelServer.T;
                   <*UNUSED*>READONLY read : Map.T;
                   READONLY update : MapAddr.Update) =
  BEGIN
    Debug.Out("SetupMby");
    
    (* fill this in *)
  END SetupMby;

BEGIN END MbyModel.
