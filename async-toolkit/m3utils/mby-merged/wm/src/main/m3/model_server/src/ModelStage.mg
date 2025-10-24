GENERIC MODULE ModelStage(TopAddr, Model);
IMPORT Coroutine;
IMPORT BaseModelStage;
IMPORT ModelStageResult, ModelStageResultClass;
IMPORT ServerPacket AS Pkt;
IMPORT Metadata;

PROCEDURE Init(t : T;
               h : TopAddr.H;
               indices : Model.Indices;
               prev : BaseModelStage.T) : T =
  BEGIN
    t.prev := prev;
    t.indices := indices; 
    WITH cl = NEW(Closure, h := h, t := t) DO
      t.res := NEW(Res).init(Coroutine.Create(cl))
    END;
    EVAL Coroutine.Call(t.res.co); (* setup part of coroutine *)
    RETURN t
  END Init;

REVEAL
  T = Public BRANDED Brand OBJECT
    indices : Model.Indices;
    res     : Res;
    prev    : BaseModelStage.T;
  OVERRIDES
    init := Init;
    poll := Poll;
  END;

TYPE
  Res = ModelStageResult.T OBJECT
    ready : BOOLEAN;
  END;

TYPE
  Closure = Coroutine.Closure OBJECT
    h : TopAddr.H;
    t : T;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(cl : Closure; from : Coroutine.T) : REFANY =
  BEGIN

    (* this code looks totally generic to me, should be moved 
       elsewhere if that is really so *)
    
    EVAL Coroutine.Call(from);
    LOOP
      (* Poll() brings us here if we are done handling a packet,
         else (if we are still handling a packet) it brings us somewhere
         into HandlePacket() *)
      VAR
        ipkt : Pkt.T;
        imd  : Metadata.T;
      BEGIN
        WITH haveInput = cl.t.prev.poll(ipkt, imd) DO
          IF haveInput THEN
            cl.t.res.ready := TRUE;
            Model.HandlePacket(ipkt,
                               cl.t.h,
                               cl.t.indices,
                               imd,
                               cl.t.res)
          END;
          cl.t.res.ready := FALSE;
        END
      END;
      EVAL Coroutine.Call(from)
    END
  END Apply;

PROCEDURE Poll(t : T; VAR out : Pkt.T; VAR meta : Metadata.T) : BOOLEAN =
  BEGIN
    EVAL Coroutine.Call(t.res.co);
    IF t.res.ready THEN
      out := t.res.opkt;
      meta := t.res.om
    END;
    RETURN t.res.ready
  END Poll;

BEGIN END ModelStage.
