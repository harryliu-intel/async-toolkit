(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE ModelServer;
IMPORT ModelServerSuper;

IMPORT Debug;
IMPORT Thread;
FROM Fmt IMPORT F, Int;
IMPORT Rd, IP;
IMPORT Fmt;
IMPORT FmModelMessageHdr;
IMPORT Pathname;
IMPORT NetError;
IMPORT FmModelMsgType;
IMPORT RdNet, WrNet;
FROM NetTypes IMPORT U8, U16, U32;
IMPORT Text;
IMPORT NetContext;
IMPORT ServerPacket AS Pkt;
IMPORT MsIosf;
IMPORT FmModelSetEgressInfoHdr;
IMPORT FmModelConstants;
IMPORT FmModelPortLinkState;
IMPORT DataPusher, IntDataPusherTbl;
IMPORT DataPusherSet, DataPusherSetDef;
IMPORT Byte;
IMPORT FmModelDataType;
IMPORT FmModelSideBandData;
IMPORT UpdaterFactory;

FROM ModelServerSuper IMPORT ParseError, Instance, MsgHandler;
FROM ModelServerUtils IMPORT HandleMsgVersionInfo, HandleMsgCommandQuit,
                             GetStringC;

(* may keep : *)
<*FATAL Thread.Alerted*>

(* should not keep : for now also: *)
<*FATAL IP.Error*>
<*FATAL NetContext.Short*>
<*FATAL NetError.OutOfRange*>

REVEAL
  T = Public BRANDED Brand OBJECT
    egressPorts          : IntDataPusherTbl.T;
    pushers              : DataPusherSet.T; (* only used in shared mode *)
    portLinkState        : ARRAY [0..FmModelConstants.NPhysPorts-1] OF Byte.T;
    mu                   : MUTEX; (* protects egressPorts, portLinkState *)
  OVERRIDES
    init          := Init;
    pushPacket    := PushPacket;
    sendPacketEot := SendPacketEot;
  END;

PROCEDURE SendPacketEot(t : T) =
  CONST
    VersionNumber = 2; (* where does this really come from? *)
  VAR
    pusher : DataPusher.T;
    iter := t.pushers.iterate();
    pkt := NEW(Pkt.T).init();
    hdr := FmModelMessageHdr.T { FmModelMessageHdr.Length,
                                 VersionNumber,
                                 FmModelMsgType.T.PacketEot,
                                 0,
                                 LAST(U16) (* any *)
    };
  BEGIN
    FmModelMessageHdr.WriteE(pkt, Pkt.End.Front, hdr);
    WHILE iter.next(pusher) DO
      Debug.Out("SendPacketEOT : sending EOT");
      Pkt.DebugOut(pkt);
      pusher.push(pkt)
    END
  END SendPacketEot;
  
PROCEDURE PushPacket(t : T; READONLY hdrP : FmModelMessageHdr.T; pkt : Pkt.T) =
  VAR
    pusher : DataPusher.T;
    hdr := hdrP;
  BEGIN
    (* the reverse of DecodeTlvPacket here *)

    WrNet.PutU32G(pkt, Pkt.End.Front, pkt.size());
    FmModelDataType.WriteE(pkt, Pkt.End.Front, FmModelDataType.T.Packet);

    hdr.msgLength := pkt.size() + FmModelMessageHdr.Length;
    (* this is a little screwy, the length accounts for the length of the
       FmModelMessageHdr.T *)

    FmModelMessageHdr.WriteE(pkt, Pkt.End.Front, hdr);
    WITH hadIt = t.egressPorts.get(hdr.port, pusher) DO
      IF hadIt THEN
        Debug.Out("PushPacket : pushing packet: ");
        Debug.Out("header = " & FmModelMessageHdr.Format(hdr));
        Pkt.DebugOut(pkt);
        pusher.push(pkt)
      END
    END
  END PushPacket;

PROCEDURE Init(t                    : T;
               sharedSocket         : BOOLEAN;
               <*UNUSED*>factory    : UpdaterFactory.T;
               infoPath             : Pathname.T;
               quitOnLastClientExit : BOOLEAN;
               infoFile             : Pathname.T) : T =
  BEGIN
    EVAL ModelServerSuper.T.init(t,
                                 infoPath,
                                 infoFile,
                                 quitOnLastClientExit,
                                 handlers);
    t.sharedSocket := sharedSocket;
    IF sharedSocket THEN
      t.pushers := NEW(DataPusherSetDef.T).init()
    END;
    t.mu := NEW(MUTEX);
    t.egressPorts := NEW(IntDataPusherTbl.Default).init();
    t.portLinkState :=
        ARRAY [0..FmModelConstants.NPhysPorts-1] OF Byte.T {
                                   FmModelConstants.PortLinkUp , .. };
    RETURN t
  END Init;

VAR
  handlers :=  ARRAY FmModelMsgType.T OF MsgHandler {
  (* Packet                    *) NEW(MsgHandler,
                                      handle := HandlePacket),
  (* LinkState                 *) NEW(MsgHandler,
                                      handle := HandlePortLinkState),
  (* SwitchState               *) NIL,
  (* SetEgressInfo             *) NEW(MsgHandler, 
                                      handle := HandleSetEgressInfo),
  (* EnableAlternativeDataPath *) NIL,
  (* PacketLoopback            *) NIL,
  (* PacketEot                 *) NIL,
  (* Mgmt                      *) NIL,
  (* Attr                      *) NIL,
  (* GetInfo                   *) NIL,
  (* Error                     *) NIL,
  (* Iosf                      *) NEW(MsgHandler, 
                                      handle := MsIosf.HandleMsg),
  (* Ctrl                      *) NIL,
  (* VersionInfo               *) NEW(MsgHandler, 
                                      handle := HandleMsgVersionInfo),
  (* NvmRead                   *) NIL,
  (* CommandQuit               *) NEW(MsgHandler,
                                      handle := HandleMsgCommandQuit),
  (* StageData                 *) NIL
  };

  (**********************************************************************)

PROCEDURE HandlePacket(<*UNUSED*>m  : MsgHandler;
                       READONLY hdr : FmModelMessageHdr.T;
                       VAR cx       : NetContext.T;
                       inst         : Instance)
  RAISES { Rd.EndOfFile, Rd.Failure } =
  VAR
    sbData : FmModelSideBandData.T;
  BEGIN
    Debug.Out(F("HandlePacket inst.sp.size()=%s hdr=%s",
                Fmt.Int(inst.sp.size()), FmModelMessageHdr.Format(hdr)));
    <*ASSERT hdr.type = FmModelMsgType.T.Packet*>

    EVAL inst.sp.init(); (* clear out any crud from here *)
    
    FOR i := 0 TO cx.rem-1 DO
      inst.sp.addhi(RdNet.GetU8C(inst.rd, cx));
    END;
    Pkt.DebugOut(inst.sp);
    WITH ok = DecodeTlvPacket(inst.sp, sbData),
         t  = NARROW(inst.t, T) DO
      Debug.Out(F("DecodeTlvPacket success=%s", Fmt.Bool(ok)));
      IF ok THEN
        Debug.Out(F("DecodeTlvPacket returned sbData=%s pkt follows",
                    FmModelSideBandData.Format(sbData)));
        Pkt.DebugOut(inst.sp);
        t.handlePacket(hdr, inst.sp); (* do we need to duplicate here? *)
        IF t.sharedSocket THEN 
          t.sendPacketEot() (* signal end of packet handling *)
        END
      END
    END;
    <*ASSERT cx.rem=0*>
  END HandlePacket;

PROCEDURE DecodeTlvPacket((*INOUT*)pkt  : Pkt.T;
                          VAR sbDataP   : FmModelSideBandData.T) : BOOLEAN =
  VAR
    p                : CARDINAL := 0;
    type             : FmModelDataType.T;
    len              : U32;
    pktStart, pktLen            := LAST(CARDINAL);
    sbData           :=
        FmModelSideBandData.T { 0, 0 , ARRAY [0..32-1] OF U8 { 0, .. } };
  BEGIN
    WHILE p < pkt.size() DO
      WITH ok = FmModelDataType.ReadS(pkt, p, type) AND
                RdNet.GetU32S        (pkt, p, len)
       DO
        Debug.Out(F("DecodeTlvPacket p=%s ok=%s", Int(p), Fmt.Bool(ok)));
        IF NOT ok THEN RETURN FALSE END;

        Debug.Out(F("DecodeTlvPacket p=%s type=%s len=%s",
                    Int(p),
                    FmModelDataType.Format(type),
                    Int(len)));
        
        CASE type OF
          FmModelDataType.T.Packet =>
          pktStart := p;
          pktLen   := len;
          INC(p,len)
        |
          FmModelDataType.T.SbId =>
          <*ASSERT len=4*>
          IF NOT RdNet.GetU32S(pkt, p, sbData.idTag) THEN RETURN FALSE END
        |
          FmModelDataType.T.SbTc =>
          <*ASSERT len=1*>
          IF NOT RdNet.GetU8S(pkt, p, sbData.tc) THEN RETURN FALSE END
        |
          FmModelDataType.T.PacketMeta =>
          FOR i := 0 TO len-1 DO
            IF NOT RdNet.GetU8S(pkt, p, sbData.pktMeta[i]) THEN RETURN FALSE END
          END
        END
      END
    END;

    (* --------- ok --------- *)

    (* leave only the payload of the serverpacket *)
    FOR i := 0 TO pktStart-1  DO EVAL pkt.remlo() END;
    WHILE pkt.size() # pktLen DO EVAL pkt.remhi() END;

    (* and copy out the correct sbData *)
    sbDataP := sbData;
    
    RETURN TRUE
  END DecodeTlvPacket;
  
PROCEDURE HandlePortLinkState(<*UNUSED*>m  : MsgHandler;
                              READONLY hdr : FmModelMessageHdr.T;
                              VAR cx       : NetContext.T;
                              inst         : Instance)
  RAISES { Rd.EndOfFile, Rd.Failure, ParseError } =
  VAR
    oldState : U8 := 0;
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.LinkState*>
    WITH ls = FmModelPortLinkState.ReadC(inst.rd, cx),
         t  = NARROW(inst.t, T) DO
      LOCK t.mu DO
        IF hdr.port > LAST(t.portLinkState) THEN
          RAISE ParseError("PortOutOfRange:"&Int(hdr.port))
        END;
        oldState := t.portLinkState[hdr.port];
        IF oldState # ls.state THEN
          Debug.Out(F("PhysPort %s linkState %s oldState %s",
                      Int(hdr.port), Int(ls.state), Int(oldState)));
          t.portLinkState[hdr.port] := ls.state
        END
      END
    END
  END HandlePortLinkState;
  
PROCEDURE HandleSetEgressInfo(<*UNUSED*>m  : MsgHandler;
                              READONLY hdr : FmModelMessageHdr.T;
                              VAR cx       : NetContext.T;
                              inst         : Instance)
  RAISES { Rd.EndOfFile, Rd.Failure }=
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.SetEgressInfo*>
    WITH  eiHdr    = FmModelSetEgressInfoHdr.ReadC(inst.rd, cx),
          remBytes = cx.rem,
          hostname = GetStringC(inst.rd, remBytes, cx),
          t        = NARROW(inst.t, T) DO
      Debug.Out(F("Received %s, hostname=%s, forPort=%s",
                  FmModelSetEgressInfoHdr.Format(eiHdr),
                  hostname,
                  Int(hdr.port)));

      LOCK t.mu DO
        CASE t.sharedSocket OF
          FALSE => HandleSetEgressInfoNonShared(t,
                                                hdr.port,
                                                hostname,
                                                eiHdr.tcpPort)
        |
          TRUE  => HandleSetEgressInfoShared   (t, 
                                                hdr.port,
                                                hostname,
                                                eiHdr.tcpPort)
        END
      END
    END
  END HandleSetEgressInfo;

PROCEDURE HandleSetEgressInfoNonShared(t        : T;
                                       port     : U16;
                                       hostname : TEXT;
                                       tcpPort  : IP.Port) =
  (* t.mu is locked *)
  VAR
    dp : DataPusher.T;
    portTbl := t.egressPorts;
    disable := tcpPort = FmModelConstants.SocketPortDisable;
  BEGIN
    IF portTbl.delete(port, dp) THEN
      Debug.Out("Shutting down egress port " & Int(port));
      dp.forceExit();
    END;
    IF NOT disable THEN
      Debug.Out(F("Opening link for %s to %s:%s",
                  Int(port),
                  hostname,
                  Int(tcpPort)));
      WITH newPusher = NEW(MyDataPusher,
                           t       := t,
                           port    := port).init(hostname, tcpPort) DO
        EVAL portTbl.put(port, newPusher)
      END
    END
  END HandleSetEgressInfoNonShared;

PROCEDURE HandleSetEgressInfoShared(t        : T;
                                    port     : U16;
                                    hostname : TEXT;
                                    tcpPort  : IP.Port) =
  (* t.mu is locked *)

  PROCEDURE HaveIt(VAR pusher : DataPusher.T) : BOOLEAN =
    VAR
      iter := t.pushers.iterate();
      p : DataPusher.T;
    BEGIN
      WHILE iter.next(p) DO
        IF Text.Equal(hostname, p.getHostname()) AND p.getPort() = tcpPort THEN
          pusher := p;
          RETURN TRUE
        END
      END;
      RETURN FALSE
    END HaveIt;
    
  VAR
    odp, ndp : DataPusher.T;
    portTbl := t.egressPorts;
    disable := tcpPort = FmModelConstants.SocketPortDisable;
  BEGIN
    IF portTbl.delete(port, odp) THEN
      Debug.Out("Shutting down egress port " & Int(port))
    END;
    IF NOT disable THEN
      IF HaveIt(ndp) THEN
        Debug.Out(F("Reusing link for %s to %s:%s",
                    Int(port),
                    hostname,
                    Int(tcpPort)));
      ELSE
        Debug.Out(F("Opening link for %s to %s:%s",
                    Int(port),
                    hostname,
                    Int(tcpPort)));
        ndp := NEW(MyDataPusher,
                   t       := t,
                   port    := port).init(hostname, tcpPort);
        EVAL t.pushers.insert(ndp)
      END;
      EVAL portTbl.put(port, ndp)
    END
  END HandleSetEgressInfoShared;  

TYPE
  MyDataPusher = DataPusher.T OBJECT
    t    : T;
    port : CARDINAL;
  OVERRIDES
    exitCallback:= DPEC;
  END;

PROCEDURE DPEC(dp : MyDataPusher) =
  VAR
    x : DataPusher.T;
  BEGIN
    Debug.Out(F("Shutting down DataPusher: TCPport %s host %s phys_port %s",
                Int(dp.getPort()), dp.getHostname(), Int(dp.port)));
    LOCK dp.t.mu DO
      IF dp.t.sharedSocket THEN
        VAR
          iter := dp.t.egressPorts.iterate();
          p : DataPusher.T;
          port : INTEGER;
        BEGIN
          WHILE iter.next(port, p) DO
            IF p = dp THEN
              EVAL dp.t.egressPorts.delete(port, p);
              (* table was modified, must restart iterator *)
              iter := dp.t.egressPorts.iterate()
            END
          END;
          EVAL dp.t.pushers.delete(dp)
        END
      ELSE
        IF dp.t.egressPorts.delete(dp.port,x) THEN
          <*ASSERT x=dp*>
        END
      END
    END
  END DPEC;

BEGIN END ModelServer.
