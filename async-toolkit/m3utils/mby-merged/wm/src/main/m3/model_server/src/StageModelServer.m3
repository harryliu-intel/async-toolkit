(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

MODULE StageModelServer;

IMPORT ModelServerSuper;

IMPORT UpdaterFactory, Pathname;
IMPORT FmModelMsgType;

FROM ModelServerSuper IMPORT Instance, MsgHandler;
FROM ModelServerUtils IMPORT HandleMsgCommandQuit;

IMPORT Rd, FmModelMessageHdr, NetContext;
IMPORT MsIosf;
IMPORT ServerPacket AS Pkt;
IMPORT Debug; FROM Debug IMPORT UnNil;
IMPORT RdNet, WrNet;
IMPORT NetTypes;
IMPORT Fmt; FROM Fmt IMPORT F, Int;
IMPORT Byte;
IMPORT CharSeq;
IMPORT Text;
IMPORT Thread;

<*FATAL Thread.Alerted*>

REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;

PROCEDURE Init(t                    : T;
               stageName            : TEXT;
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
    t.stageName := stageName;
    RETURN t
  END Init;

PROCEDURE SeqToText(seq : CharSeq.T) : TEXT =
  VAR
    len := seq.size();
    q := NEW(REF ARRAY OF CHAR, len);
  BEGIN
    FOR i := 0 TO len-1 DO
      q[i] := seq.get(i)
    END;
    RETURN Text.FromChars(q^)
  END SeqToText;

TYPE
  StageData = RECORD
    len             : NetTypes.U32;
    nm              : TEXT;
    in              : REF ARRAY OF Byte.T;
    rxData          : REF ARRAY OF Byte.T;
  END;                                

PROCEDURE FmtStageData(READONLY sd : StageData) : TEXT =
  BEGIN
    RETURN F("len %s nm \"%s\" inSz %s rxDataSz %s",
             Int(sd.len),
             sd.nm,
             Int(NUMBER(sd.in^)),
             Int(NUMBER(sd.rxData^)));
    
  END FmtStageData;
  
PROCEDURE DecodeStageData(pkt : Pkt.T;
                          VAR stageData : StageData) : BOOLEAN =
  VAR
    temp : StageData;
    p : CARDINAL := 0;
  BEGIN
    IF NOT RdNet.GetU32S(pkt, p, temp.len) THEN RETURN FALSE END;

    WITH buff = NEW(CharSeq.T).init() DO
      WHILE p < pkt.size() DO
        WITH c = pkt.get(p) DO
          INC(p);
          IF ORD(c) = 0 THEN
            temp.nm := SeqToText(buff);
            EXIT
          ELSE
            buff.addhi(VAL(c,CHAR))
          END
        END;
      END
    END;

    Debug.Out("Got stage name " & UnNil(temp.nm));
    
    VAR
      inSz : NetTypes.U32;
    BEGIN
      IF NOT RdNet.GetU32S(pkt, p, inSz) THEN RETURN FALSE END;

      temp.in  := NEW(REF ARRAY OF Byte.T, inSz);

      FOR i := FIRST(temp.in^) TO LAST(temp.in^) DO
        temp.in[i] := pkt.get(p); INC(p)
      END
    END;

    VAR
      rxDataSz : NetTypes.U32;
    BEGIN
      IF NOT RdNet.GetU32S(pkt, p, rxDataSz) THEN RETURN FALSE END;

      temp.rxData := NEW(REF ARRAY OF Byte.T, rxDataSz);

      FOR i := FIRST(temp.rxData^) TO LAST(temp.rxData^) DO
        temp.rxData[i] := pkt.get(p); INC(p)
      END
    END;
    
    IF p = pkt.size() THEN
      stageData := temp;
      RETURN TRUE
    ELSE
      RETURN FALSE
    END
  END DecodeStageData;

PROCEDURE HandleMsgStageData(<*UNUSED*>m  : MsgHandler;
                             READONLY hdr : FmModelMessageHdr.T;
                             VAR cx       : NetContext.T;
                             inst         : Instance)
  RAISES { Rd.EndOfFile, Rd.Failure } =
  <*FATAL NetContext.Short*>
  VAR
    sd : StageData;
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.StageData*>
    EVAL inst.sp.init(); (* clear out any crud from here *)
    
    FOR i := 0 TO cx.rem-1 DO
      inst.sp.addhi(RdNet.GetU8C(inst.rd, cx));
    END;
    Pkt.DebugOut(inst.sp);
    
    WITH ok = DecodeStageData(inst.sp, sd),
         t  = NARROW(inst.t, T) DO
      Debug.Out(F("DecodeStageData success=%s", Fmt.Bool(ok)));
      Debug.Out("stageData=\n" & FmtStageData(sd));

      VAR
        out    : REF ARRAY OF Byte.T;
        txData : REF ARRAY OF Byte.T;
        nmLen := MIN(Text.Length(t.stageName)+1, 128);
        len : CARDINAL;
      BEGIN
        t.runStage(sd.in^, out, sd.rxData^, txData);

        EVAL inst.sp.init(); (* clear out any crud from here *)
        len := 12 + nmLen + NUMBER(out^) + NUMBER(txData^);
        Debug.Out(F("after runStage nmLen %s outSz %s txDataSz %s len %s",
                    Int(nmLen), Int(NUMBER(out^)), Int(NUMBER(txData^)),
                    Int(len)));

        WrNet.PutU32G(inst.sp, Pkt.End.Back, len);

        FOR i := 0 TO nmLen-2 DO
          WrNet.PutU8G(inst.sp, Pkt.End.Back, ORD(Text.GetChar(t.stageName,i)))
        END;
        WrNet.PutU8G(inst.sp, Pkt.End.Back, 0);

        WrNet.PutU32G(inst.sp, Pkt.End.Back, NUMBER(out^));
        FOR i := 0 TO NUMBER(out^)-1 DO
          WrNet.PutU8G(inst.sp, Pkt.End.Back, out[i])
        END;

        WrNet.PutU32G(inst.sp, Pkt.End.Back, NUMBER(txData^));
        FOR i := 0 TO NUMBER(txData^)-1 DO
          WrNet.PutU8G(inst.sp, Pkt.End.Back, out[i])
        END;

        Debug.Out("inst.sp.size()=" & Int(inst.sp.size()));

        inst.sendResponse()
      END
    END
    
  END HandleMsgStageData;
  
VAR
  handlers :=  ARRAY FmModelMsgType.T OF MsgHandler {
  (* Packet                    *) NIL,
  (* LinkState                 *) NIL,
  (* SwitchState               *) NIL,
  (* SetEgressInfo             *) NIL,
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
  (* VersionInfo               *) NIL,
  (* NvmRead                   *) NIL,
  (* CommandQuit               *) NEW(MsgHandler,
                                      handle := HandleMsgCommandQuit),
  (* StageData                 *) NEW(MsgHandler,
                                      handle := HandleMsgStageData)
  };

BEGIN END StageModelServer.
