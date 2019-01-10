GENERIC MODULE StageModelServer(ModelServer, TheModel, Map, MapAddr);
IMPORT CsrOp, CsrAccessStatus;
IMPORT Pathname;
IMPORT Debug;
IMPORT CompAddr;
FROM Fmt IMPORT F; IMPORT Fmt;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT UpdaterFactory;

VAR doDebug := Debug.DebugThis(ModelServer.Brand);
    
REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    reset        := Reset;
    init         := Init;
    csrOp        := DoCsrOp;
  END;

PROCEDURE Reset(t : T) =
  BEGIN
    (* not sure this is right! *)
    t.setup(t.h.read, t.h.update);
    MapAddr.Reset(t.h.read, t.h.update);
  END Reset;

PROCEDURE Init(t            : T;
               stageName    : TEXT;
               factory      : UpdaterFactory.T;
               infoPath     : Pathname.T;
               quitLast     : BOOLEAN;
               infoFile     : Pathname.T) : Super =
  BEGIN
    EVAL Super.init(t, stageName, factory, infoPath, quitLast, infoFile);
    t.topMapName := Map.Brand;
    Debug.Out(F("Creating %s ... stageName %s",Map.Brand, t.stageName));
    t.h := NEW(MapAddr.H).init(CompAddr.Zero, factory);
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

BEGIN END StageModelServer.
