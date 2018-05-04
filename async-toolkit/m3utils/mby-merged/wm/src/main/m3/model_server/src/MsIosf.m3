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

IMPORT IosfRegBlkWriteReqHdr;
IMPORT IosfRegBlkReadReqHdr;
IMPORT IosfRegCompDataHdr;
IMPORT IosfRegCompNoData;
IMPORT CsrOp;
IMPORT AL;
IMPORT IosfRegBlkAddr;
IMPORT IosfRegBlkData;

EXCEPTION ParseError;

TYPE Instance = ModelServerClass.Instance;
          
PROCEDURE HandleMsg(<*UNUSED*>m  : ModelServerClass.MsgHandler;
                    READONLY hdr : FmModelMessageHdr.T;
                    VAR cx       : NetContext.T;
                    inst         : Instance)
  RAISES { Rd.EndOfFile, Rd.Failure, ModelServer.ParseError, Thread.Alerted,
           Wr.Failure } =
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.Iosf*>

    Debug.Out(F("cx.rem=%s", Int(cx.rem)));

    TRY
      WITH port       = hdr.port,
           inbound    = Pkt.FromRd(NEW(Pkt.T).init(), inst.rd, cx) DO
        (* packet data is loaded into inbound *)
        
        Pkt.DebugOut(inbound);
        VAR
          brreq : IosfRegBlkReadReqHdr.T;
          bwreq : IosfRegBlkWriteReqHdr.T;
        BEGIN
          IF    IosfRegBlkWriteReqHdr.ReadEB(inbound, Pkt.End.Front, bwreq) THEN
            HandleIosfBlkWriteReq(inst, inbound, bwreq)
          ELSIF IosfRegBlkReadReqHdr.ReadEB(inbound, Pkt.End.Front, brreq) THEN
            HandleIosfBlkReadReq(inst, inbound, brreq)
          END
        END
        
      END
    EXCEPT
      ParseError => RAISE ModelServer.ParseError(Brand)
    END
    
  END HandleMsg;

PROCEDURE HandleIosfBlkWriteReq(inst     : Instance;
                                inbound  : Pkt.T;
                                req      : IosfRegBlkWriteReqHdr.T)
  RAISES { ParseError, Thread.Alerted, Wr.Failure } =

  PROCEDURE DoBlock(addr : Word.T; ndw : CARDINAL) RAISES { ParseError } =
    VAR
      ca       : CompAddr.T;
    BEGIN
      FOR a := addr TO addr + (ndw-1)*4 BY 4 DO
        ca := CompAddr.FromBytes(a);
        Debug.Out("ca=" & CompAddr.Format(ca,FALSE));
        
        VAR
          blkData : IosfRegBlkData.T;
          ok    := IosfRegBlkData.ReadEB(inbound,Pkt.End.Front,blkData);
          csrOp := CsrOp.MakeWrite(ca, 32, blkData.data, CsrOp.Origin.Software);
        BEGIN
          IF NOT ok THEN RAISE ParseError END;
          EVAL inst.t.csrOp(csrOp);
        END
      END(*FOR*)
    END DoBlock;

  VAR 
    addr     := Word.Insert(req.addr0,req.addr1,16,12);
    respHdr  := MakeBlkWriteRespHdr(req);
    ndw      := req.ndw;
    blkAddr  : IosfRegBlkAddr.T;
  BEGIN
    Debug.Out(">>> HandleIosfBlkWriteReq >>>");
    Debug.Out("req    ="&IosfRegBlkWriteReqHdr.Format(req));
    
    Debug.Out("respHdr="&IosfRegCompNoData.Format(respHdr));
    IosfRegCompNoData.WriteE(inst.sp, Pkt.End.Back, respHdr);

    DoBlock(addr, ndw);

    WHILE inbound.size() # 0 DO
      WITH match = IosfRegBlkAddr.ReadEB(inbound,Pkt.End.Front,blkAddr) DO
        IF NOT match THEN RAISE ParseError END;
        DoBlock(blkAddr.addr,blkAddr.ndw)
      END
    END;
    inst.sendResponse() (* send response on socket *)
  END HandleIosfBlkWriteReq;
  
PROCEDURE HandleIosfBlkReadReq(inst     : Instance;
                               inbound  : Pkt.T;
                               req      : IosfRegBlkReadReqHdr.T)
  RAISES { ParseError, Thread.Alerted, Wr.Failure } =

  PROCEDURE DoBlock(addr : Word.T; ndw : CARDINAL) =
    VAR
      ca       : CompAddr.T;
    BEGIN
      FOR a := addr TO addr + (ndw-1)*4 BY 4 DO
        ca := CompAddr.FromBytes(a);
        Debug.Out("ca=" & CompAddr.Format(ca,FALSE));
        
        VAR
          csrOp := CsrOp.MakeRead(ca, 32, CsrOp.Origin.Software);
          w : Word.T;
        BEGIN
          (* perform read on model *)
          EVAL inst.t.csrOp(csrOp);
          
          (* extract result from csrOp *)
          w := CsrOp.GetReadResult(csrOp);
          
          Debug.Out("CsrOp read result = " & Fmt.Unsigned(w));
          
          (* put result at end of packet *)
          Pkt.PutWLE(inst.sp, Pkt.End.Back, w, 4)
        END
      END(*FOR*)
    END DoBlock;
    
  VAR 
    addr     := Word.Insert(req.addr0,req.addr1,16,12);
    respHdr  := MakeBlkReadRespHdr(req);
    ndw      := req.ndw;
    blkAddr  : IosfRegBlkAddr.T;
  BEGIN
    Debug.Out(">>> HandleIosfBlkReadReq >>>");
    Debug.Out("req    ="&IosfRegBlkReadReqHdr.Format(req));
    
    Debug.Out("respHdr="&IosfRegCompDataHdr.Format(respHdr));
    IosfRegCompDataHdr.WriteE(inst.sp, Pkt.End.Back, respHdr);

    DoBlock(addr, ndw);

    WHILE inbound.size() # 0 DO
      WITH match = IosfRegBlkAddr.ReadEB(inbound,Pkt.End.Front,blkAddr) DO
        IF NOT match THEN RAISE ParseError END;
        DoBlock(blkAddr.addr,blkAddr.ndw)
      END
    END;
    inst.sendResponse() (* send response on socket *)
  END HandleIosfBlkReadReq;
  
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
