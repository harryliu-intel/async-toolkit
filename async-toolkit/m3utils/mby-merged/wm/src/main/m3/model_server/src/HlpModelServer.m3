MODULE HlpModelServer;
IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT CsrOp, CsrAccessStatus;
IMPORT Pathname;
IMPORT Debug;
IMPORT CompAddr;
FROM Fmt IMPORT F; IMPORT Fmt;

REVEAL
  T = Public BRANDED Brand OBJECT
    h        : MapAddr.H;
  OVERRIDES
    resetChip := ResetChip;
    init      := Init;
    csrOp     := DoCsrOp;
  END;

PROCEDURE ResetChip(t : T) =
  BEGIN
    MapAddr.Reset(t.h.read, t.h.update);
    (* not sure this is right! *)
    t.setupChip(t.h.read, t.h.update);
  END ResetChip;

PROCEDURE Init(t : T; infoPath : Pathname.T) : Super =
  BEGIN
    EVAL Super.init(t, infoPath);
    Debug.Out(F("Creating %s ...",Map.Brand));
    t.h := NEW(MapAddr.H).init(CompAddr.Zero);
    RETURN t
  END Init;

PROCEDURE DoCsrOp(t : T; VAR op : CsrOp.T) : CsrAccessStatus.T =
  BEGIN
    Debug.Out("Doing op from server...");
    WITH res = t.h.csrOp(op) DO
      Debug.Out("Did op from server, result=" & CsrOp.Format(op));
      RETURN res
    END
  END DoCsrOp;

BEGIN END HlpModelServer.
