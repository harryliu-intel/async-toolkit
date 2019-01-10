GENERIC MODULE StageModelServer(ModelServer,
                                TheModel,
                                Map,
                                MapAddr,
                                TheModelStagesC);
IMPORT CsrOp, CsrAccessStatus;
IMPORT Pathname;
IMPORT Debug;
IMPORT CompAddr;
FROM Fmt IMPORT F; IMPORT Fmt;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT UpdaterFactory;
IMPORT ModelStagesC;

VAR doDebug := Debug.DebugThis(ModelServer.Brand);
    
REVEAL
  T = Public BRANDED Brand OBJECT
    cInfo : ModelStagesC.Info;
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
    WITH foundIt = ModelStagesC.Lookup(t.topMapName, stageName, t.cInfo) DO
      IF NOT foundIt THEN
        Debug.Error(F("Unable to find stage model %s / %s",
                      t.topMapName, stageName))
      END
    END;

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

BEGIN
  TheModelStagesC.Registrar()
END StageModelServer.
