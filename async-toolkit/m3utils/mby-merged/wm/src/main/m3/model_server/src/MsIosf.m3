MODULE MsIosf;
IMPORT ModelServer;
IMPORT ModelServerClass;
IMPORT Rd;
IMPORT NetContext;
IMPORT ServerPacket AS Pkt;
IMPORT Word;
IMPORT Debug;
IMPORT Fmt; FROM Fmt IMPORT F, Int;
IMPORT CompAddr;
IMPORT Thread;
IMPORT Wr;

IMPORT FmModelMessageHdr;
IMPORT FmModelMsgType;

IMPORT IosfRegWriteReq;
IMPORT IosfRegReadReq;
IMPORT IosfRegBlkWriteReqHdr;
IMPORT IosfRegBlkReadReqHdr;
IMPORT IosfRegCompDataHdr;
IMPORT IosfRegCompNoData;
IMPORT CsrOp;
IMPORT IosfRegBlkAddr;
IMPORT IosfRegBlkData;

EXCEPTION ParseError;

TYPE Instance = ModelServerClass.Instance;

VAR doDebug := Debug.DebugThis(Brand);
          
PROCEDURE HandleMsg(<*UNUSED*>m  : ModelServerClass.MsgHandler;
                    READONLY hdr : FmModelMessageHdr.T;
                    VAR cx       : NetContext.T;
                    inst         : Instance)
  RAISES { Rd.EndOfFile, Rd.Failure, ModelServer.ParseError, Thread.Alerted,
           Wr.Failure } =
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.Iosf*>
    IF doDebug THEN Debug.Out(F("cx.rem=%s", Int(cx.rem))) END;

    TRY
      WITH inbound    = Pkt.FromRd(NEW(Pkt.T).init(), inst.rd, cx) DO
        (* packet data is loaded into inbound *)
        
        IF doDebug THEN Pkt.DebugOut(inbound) END;
        VAR
          rreq  : IosfRegReadReq.T;
          wreq  : IosfRegWriteReq.T;
          brreq : IosfRegBlkReadReqHdr.T;
          bwreq : IosfRegBlkWriteReqHdr.T;
        BEGIN
          IF    IosfRegBlkWriteReqHdr.ReadEB(inbound, Pkt.End.Front, bwreq) THEN
            HandleIosfBlkWriteReq(inst, inbound, bwreq)
          ELSIF IosfRegBlkReadReqHdr.ReadEB(inbound, Pkt.End.Front, brreq) THEN
            HandleIosfBlkReadReq(inst, inbound, brreq)
          ELSIF IosfRegWriteReq.ReadEB(inbound, Pkt.End.Front, wreq) THEN
            HandleIosfWriteReq(inst, inbound, wreq)
          ELSIF IosfRegReadReq.ReadEB (inbound, Pkt.End.Front, rreq) THEN
            HandleIosfReadReq(inst, rreq)
          END
        END
      END
    EXCEPT
      ParseError => RAISE ModelServer.ParseError(Brand)
    END
  END HandleMsg;

PROCEDURE ParseSbe(sbe : [0..16_f]) : [1..2] =
  BEGIN
    CASE sbe OF
      16_0 => RETURN 1
    |
      16_f => RETURN 2
    ELSE
      <*ASSERT FALSE*>
    END
  END ParseSbe;

PROCEDURE HandleIosfWriteReq(inst     : Instance;
                             inbound  : Pkt.T;
                             req      : IosfRegWriteReq.T)
  RAISES { ParseError, Thread.Alerted, Wr.Failure } =
  VAR 
    addr     := Word.Insert(req.addr0,req.addr1,16,12);
    respHdr  := MakeWriteRespHdr(req);
    ndw      := 2;
    doUpper  := ParseSbe(req.sbe) = 2;
  BEGIN
    IF doDebug THEN
      Debug.Out(">>> HandleIosfWriteReq >>>");
      Debug.Out("req    ="&IosfRegWriteReq.Format(req));
      Debug.Out("respHdr="&IosfRegCompNoData.Format(respHdr))
    END;
    IosfRegCompNoData.WriteE(inst.sp, Pkt.End.Back, respHdr);
    DoWrBlock(inst, inbound, addr, ndw, doLast := doUpper);
    inst.sendResponse()
  END HandleIosfWriteReq;

PROCEDURE HandleIosfReadReq(inst     : Instance;
                            req      : IosfRegReadReq.T)
  RAISES { Thread.Alerted, Wr.Failure } =
  VAR 
    addr     := Word.Insert(req.addr0,req.addr1,16,12);
    respHdr  := MakeReadRespHdr(req);
    ndw      := 2;
    doUpper  := ParseSbe(req.sbe) = 2;
  BEGIN
    IF doDebug THEN
      Debug.Out(">>> HandleIosfReadReq >>>");
      Debug.Out("req    ="&IosfRegReadReq.Format(req));
      Debug.Out("respHdr="&IosfRegCompDataHdr.Format(respHdr))
    END;
    IosfRegCompDataHdr.WriteE(inst.sp, Pkt.End.Back, respHdr);
    DoRdBlock(inst, addr, ndw, doLast := doUpper);
    inst.sendResponse();
  END HandleIosfReadReq;
  
  (**********************************************************************)

PROCEDURE DoRdBlock(inst : Instance;
                    addr : Word.T;
                    ndw : CARDINAL;
                    doLast := TRUE) =
  VAR
    ca       : CompAddr.T;
    top := addr + (ndw-1)*4 ;
  BEGIN
    FOR a := addr TO top BY 4 DO
      ca := CompAddr.FromBytes(a);
      IF doDebug THEN Debug.Out("ca=" & CompAddr.Format(ca,FALSE)) END;
      
      VAR
        csrOp := CsrOp.MakeRead(ca, 32, CsrOp.Origin.Software);
        w : Word.T;
      BEGIN
        IF a = top AND NOT doLast THEN
          w := 0;
          IF doDebug THEN Debug.Out("Padding top of result") END
        ELSE
          (* perform read on model *)
          EVAL inst.t.csrOp(csrOp);
        
          (* extract result from csrOp *)
          w := CsrOp.GetReadResult(csrOp);
          IF doDebug THEN Debug.Out("CsrOp read result = " & Fmt.Unsigned(w)) END
        END;
        
        (* put result at end of packet *)
        Pkt.PutWLE(inst.sp, Pkt.End.Back, w, 4)
      END
    END(*FOR*)
  END DoRdBlock;
  
PROCEDURE HandleIosfBlkReadReq(inst     : Instance;
                               inbound  : Pkt.T;
                               req      : IosfRegBlkReadReqHdr.T)
  RAISES { ParseError, Thread.Alerted, Wr.Failure } =

  VAR 
    addr     := Word.Insert(req.addr0,req.addr1,16,12);
    respHdr  := MakeBlkReadRespHdr(req);
    ndw      := req.ndw;
    blkAddr  : IosfRegBlkAddr.T;
  BEGIN
    IF doDebug THEN
      Debug.Out(">>> HandleIosfBlkReadReq >>>");
      Debug.Out("req    ="&IosfRegBlkReadReqHdr.Format(req));
      Debug.Out("respHdr="&IosfRegCompDataHdr.Format(respHdr))
    END;
    IosfRegCompDataHdr.WriteE(inst.sp, Pkt.End.Back, respHdr);

    DoRdBlock(inst, addr, ndw);

    WHILE inbound.size() # 0 DO
      WITH match = IosfRegBlkAddr.ReadEB(inbound,Pkt.End.Front,blkAddr) DO
        IF NOT match THEN RAISE ParseError END;
        DoRdBlock(inst, blkAddr.addr, blkAddr.ndw)
      END
    END;
    inst.sendResponse() (* send response on socket *)
  END HandleIosfBlkReadReq;
  
  (**********************************************************************)
  
PROCEDURE DoWrBlock(inst : Instance;
                    inbound : Pkt.T;
                    addr : Word.T;
                    ndw : CARDINAL;
                    doLast := TRUE) RAISES { ParseError } =
  VAR
    ca       : CompAddr.T;
    top := addr + (ndw-1)*4;
  BEGIN
    FOR a := addr TO top BY 4 DO
      ca := CompAddr.FromBytes(a);
      IF doDebug THEN Debug.Out("ca=" & CompAddr.Format(ca,FALSE)) END;
      
      VAR
        blkData : IosfRegBlkData.T;
        ok    := IosfRegBlkData.ReadEB(inbound,Pkt.End.Front,blkData);
        csrOp := CsrOp.MakeWrite(ca, 32, blkData.data, CsrOp.Origin.Software);
      BEGIN
        IF NOT ok THEN RAISE ParseError END;
        IF NOT (a = top AND NOT doLast) THEN EVAL inst.t.csrOp(csrOp) END;
      END
    END(*FOR*)
  END DoWrBlock;

PROCEDURE HandleIosfBlkWriteReq(inst     : Instance;
                                inbound  : Pkt.T;
                                req      : IosfRegBlkWriteReqHdr.T)
  RAISES { ParseError, Thread.Alerted, Wr.Failure } =

  VAR 
    addr     := Word.Insert(req.addr0,req.addr1,16,12);
    respHdr  := MakeBlkWriteRespHdr(req);
    ndw      := req.ndw;
    blkAddr  : IosfRegBlkAddr.T;
  BEGIN
    IF doDebug THEN
      Debug.Out(">>> HandleIosfBlkWriteReq >>>");
      Debug.Out("req    ="&IosfRegBlkWriteReqHdr.Format(req));
      Debug.Out("respHdr="&IosfRegCompNoData.Format(respHdr))
    END;
    IosfRegCompNoData.WriteE(inst.sp, Pkt.End.Back, respHdr);

    DoWrBlock(inst, inbound, addr, ndw);

    WHILE inbound.size() # 0 DO
      WITH match = IosfRegBlkAddr.ReadEB(inbound,Pkt.End.Front,blkAddr) DO
        IF NOT match THEN RAISE ParseError END;
        DoWrBlock(inst, inbound, blkAddr.addr, blkAddr.ndw)
      END
    END;
    inst.sendResponse() (* send response on socket *)
  END HandleIosfBlkWriteReq;

  (**********************************************************************)
  
PROCEDURE MakeReadRespHdr(READONLY req : IosfRegReadReq.T) :
  IosfRegCompDataHdr.T =
  VAR
    res : IosfRegCompDataHdr.T;
  BEGIN
    res.dest := req.source;
    res.source := req.dest;
    res.tag := req.tag;
    (* res.sai ? *)
    RETURN res
  END MakeReadRespHdr;

PROCEDURE MakeWriteRespHdr(READONLY req : IosfRegWriteReq.T) :
  IosfRegCompNoData.T =
  VAR
    res : IosfRegCompNoData.T;
  BEGIN
    res.dest := req.source;
    res.source := req.dest;
    res.tag := req.tag;
    (* res.sai ? *)
    RETURN res
  END MakeWriteRespHdr;

PROCEDURE MakeBlkReadRespHdr(READONLY req : IosfRegBlkReadReqHdr.T) :
  IosfRegCompDataHdr.T =
  VAR
    res : IosfRegCompDataHdr.T;
  BEGIN
    res.dest := req.source;
    res.source := req.dest;
    res.tag := req.tag;
    (* res.sai ? *)
    RETURN res
  END MakeBlkReadRespHdr;

PROCEDURE MakeBlkWriteRespHdr(READONLY req : IosfRegBlkWriteReqHdr.T) :
  IosfRegCompNoData.T =
  VAR
    res : IosfRegCompNoData.T;
  BEGIN
    res.dest := req.source;
    res.source := req.dest;
    res.tag := req.tag;
    (* res.sai ? *)
    RETURN res
  END MakeBlkWriteRespHdr;

BEGIN END MsIosf.
