GENERIC MODULE BaseModelServer(TheModel, Map, MapAddr);
IMPORT ModelServer;
(*
IMPORT HlpModel AS TheModel;
IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
*)
IMPORT CsrOp, CsrAccessStatus;
IMPORT Pathname;
IMPORT Debug;
IMPORT CompAddr;
FROM Fmt IMPORT F; IMPORT Fmt;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;

VAR doDebug := Debug.DebugThis(ModelServer.Brand);
    
REVEAL
  T = Public BRANDED Brand OBJECT
    h        : MapAddr.H;
  OVERRIDES
    resetChip    := ResetChip;
    init         := Init;
    csrOp        := DoCsrOp;
    handlePacket := HandlePacket;
  END;

PROCEDURE ResetChip(t : T) =
  BEGIN
    MapAddr.Reset(t.h.read, t.h.update);
    (* not sure this is right! *)
    t.setupChip(t.h.read, t.h.update);
  END ResetChip;

PROCEDURE Init(t : T;
               infoPath : Pathname.T;
               quitLast : BOOLEAN;
               infoFile : Pathname.T) : Super =
  BEGIN
    EVAL Super.init(t, infoPath, quitLast, infoFile);
    Debug.Out(F("Creating %s ...",Map.Brand));
    t.h := NEW(MapAddr.H).init(CompAddr.Zero);
    RETURN t
  END Init;

PROCEDURE DoCsrOp(t : T; VAR op : CsrOp.T) : CsrAccessStatus.T =
  BEGIN
    IF doDebug THEN Debug.Out("Doing op from server...") END;
    WITH res = t.h.csrOp(op) DO
      IF doDebug THEN Debug.Out("Did op from server, result=" & CsrOp.Format(op)) END;
      RETURN res
    END
  END DoCsrOp;

PROCEDURE HandlePacket(t : T; READONLY hdr : FmModelMessageHdr.T; pkt : Pkt.T) =
  BEGIN
    TheModel.HandlePacket(t.h.read, t.h.update, hdr, pkt)
  END HandlePacket;

BEGIN END BaseModelServer.
