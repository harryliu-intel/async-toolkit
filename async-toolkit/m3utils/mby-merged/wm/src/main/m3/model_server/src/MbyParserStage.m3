MODULE MbyParserStage;

IMPORT mby_top_map_addr AS TopAddr;

IMPORT mby_ppe_parser_map AS Map;
IMPORT mby_ppe_parser_map_addr AS MapAddr;

IMPORT ServerPacket AS Pkt;
IMPORT Metadata;
IMPORT MbyParserMeta;
IMPORT Coroutine;
IMPORT BaseModelStage;
IMPORT MbyModelStage;

REVEAL
  T = Public BRANDED Brand OBJECT
    indices : Indices;
    co : Coroutine.T;
    res : Res;
    prev : BaseModelStage.T;
  OVERRIDES
    init := Init;
    poll := Poll;
  END;

TYPE
  Indices = RECORD
    MptIdx : [0..2-1];
  END;

TYPE
  PrivateRes = OBJECT
    ready : BOOLEAN;
  END;
  
  Res = PrivateRes OBJECT
    opkt  : Pkt.T;
    om    : MbyParserMeta.T;
  END;

PROCEDURE Init(t : T; h : TopAddr.H; prev : BaseModelStage.T) : MbyModelStage.T =
  BEGIN
    t.res := NEW(Res);
    t.prev := prev;
    WITH cl = NEW(Closure, h := h, t := t) DO
      t.co := Coroutine.Create(cl)
    END;
    EVAL Coroutine.Call(t.co); (* setup part of coroutine *)
    RETURN t
  END Init;

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
            HandlePacketWrap(from,
                             ipkt,
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
    EVAL Coroutine.Call(t.co);
    IF t.res.ready THEN
      out := t.res.opkt;
      meta := t.res.om
    END;
    RETURN t.res.ready
  END Poll;

PROCEDURE HandlePacketWrap(from : Coroutine.T;
                           ipkt : Pkt.T;
                           h : TopAddr.H;
                           indices : Indices;
                           imd : Metadata.T;
                           out : Res) =
  BEGIN
    (* purpose of this routine is to map the records *)
    HandlePacket(from,
                 ipkt,
                 h.read  .Mpt[indices.MptIdx].RxPpe.Parser,
                 h.update.Mpt[indices.MptIdx].RxPpe.Parser,
                 imd,
                 out)
  END HandlePacketWrap;
  
PROCEDURE HandlePacket(from        : Coroutine.T;
                       ipkt        : Pkt.T;
                       READONLY r  : Map.T;
                       READONLY u  : MapAddr.U;
                       im          : Metadata.T;
                       out         : Res) =
  BEGIN
    (* duplicate, just to test *)
    FOR i := 1 TO 2 DO
      out.opkt := ipkt;
      out.om := NEW(MbyParserMeta.T);
      EVAL Coroutine.Call(from);
    END;
    out.opkt := NIL (* signal EOT *)
  END HandlePacket;

BEGIN END MbyParserStage.
