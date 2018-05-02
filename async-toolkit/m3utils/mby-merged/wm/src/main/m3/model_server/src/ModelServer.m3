MODULE ModelServer;
IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT CompAddr;
IMPORT Debug;
IMPORT Thread;
FROM Fmt IMPORT F, Int, Unsigned;
IMPORT Rd, Wr, IP, TCP;
IMPORT RefSeq;
IMPORT ConnRW;
IMPORT Fmt;
IMPORT FmModelMessageHdr;
IMPORT Pathname;
IMPORT OSError, FileWr;
IMPORT NetError;
IMPORT FmModelMsgType;
IMPORT RdNet, WrNet;
IMPORT Text;
IMPORT NetContext;
IMPORT FmModelMsgVersionHdr;
IMPORT FmModelSideBandData;
IMPORT IosfOp;
IMPORT Wx;
IMPORT CharSeq;
IMPORT ServerPacket AS Pkt;
IMPORT Word;
FROM NetTypes IMPORT U8, U16, U32, U64, ByteOrder;

(* may keep : *)
<*FATAL Thread.Alerted*>

(* should not keep : for now also: *)
<*FATAL OSError.E, Wr.Failure, IP.Error, Rd.EndOfFile, Rd.Failure*>
<*FATAL NetContext.Short*>
<*FATAL NetError.OutOfRange*>

REVEAL
  T = Public BRANDED Brand OBJECT
    h        : MapAddr.H;
    port     : IP.Port;
    conn     : TCP.Connector; (* listen here *)
    listener : Listener := NIL;
    infoPath : Pathname.T;
  OVERRIDES
    init       := Init;
    resetChip  := ResetChip;
    listenFork := ListenFork;
  END;

PROCEDURE ListenFork(t : T) : Listener =
  BEGIN
    WITH listener = NEW(Listener,
                        t := t,
                        handlers := NEW(RefSeq.T).init(),
                        conn := t.conn) DO
      EVAL Thread.Fork(listener);
      RETURN listener
    END
  END ListenFork;

REVEAL
  Listener = PubListener BRANDED Brand & " Listener" OBJECT
    t : T;
    handlers : RefSeq.T;
    conn : TCP.Connector;
  OVERRIDES
    apply := LCApply;
  END;

TYPE
  Instance = Thread.Closure OBJECT
    rd : Rd.T;
    wr : Wr.T;
    sp : Pkt.T;
  METHODS
    init(rd : Rd.T; wr : Wr.T) : Instance := InitI;
    
    receiveMessage() RAISES { NetError.OutOfRange } := ReceiveMessage;

    sendResponse() RAISES { Wr.Failure, Thread.Alerted } := SendResponse;
    (* send a response packet from sp, formatted WITHOUT the outer header
       of type FmModelMessageHdr.T *)
  OVERRIDES
    apply := HApply;
  END;

PROCEDURE SendResponse(i : Instance) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    Debug.Out(F("SendResponse: %s bytes", Int(i.sp.size())));
    Pkt.Transmit(i.sp,i.wr);
    EVAL i.sp.init() (* clear buffer *)
  END SendResponse;

PROCEDURE InitI(i : Instance; rd : Rd.T; wr : Wr.T) : Instance =
  BEGIN
    i.rd := rd;
    i.wr := wr;
    i.sp := NEW(Pkt.T).init();
    RETURN i
  END InitI;

PROCEDURE LCApply(cl : Listener) : REFANY =
  BEGIN
    WriteInfo(cl.t.infoPath, cl.t.port);

    LOOP
      WITH tcp = TCP.Accept(cl.conn),
           wr = ConnRW.NewWr(tcp),
           rd = ConnRW.NewRd(tcp),
           h  = NEW(Instance).init(rd, wr) DO
        cl.handlers.addhi(h);
        EVAL Thread.Fork(h)
      END
    END
  END LCApply;

PROCEDURE WriteInfo(dirPath : Pathname.T; port : IP.Port)
  RAISES { OSError.E, Wr.Failure } =
  VAR
    infoPath := dirPath & "/" & InfoFileName;
  BEGIN
    Debug.Out("Writing info to " & infoPath);
    WITH wr = FileWr.Open(infoPath) DO
      Wr.PutText(wr, F("0:localhost:%s\n", Int(port)));
      Wr.Close(wr)
    END
  END WriteInfo;
  
PROCEDURE Init(t : T; infoPath : Pathname.T) : T =
  BEGIN
    t.infoPath := infoPath;
    WITH ep   = IP.Endpoint { IP.NullAddress, IP.NullPort },
         conn = TCP.NewConnector(ep) DO
      t.port := TCP.GetEndPoint(conn).port;
      Debug.Out("PORT " & Int(t.port));
      t.conn := conn
    END;
    Debug.Out(F("Creating %s ...",Map.Brand));
    t.h := NEW(MapAddr.H).init(CompAddr.Zero);
    RETURN t
  END Init;

PROCEDURE ResetChip(t : T) =
  BEGIN MapAddr.Reset(t.h.read, t.h.update) END ResetChip;

PROCEDURE HApply(cl : Instance) : REFANY =
  BEGIN
    Debug.Out("HApply");
    TRY
      LOOP
        cl.receiveMessage()
      END
    EXCEPT
      NetError.OutOfRange(w) =>
      Debug.Out("Caught NetError.OutOfRange : " & Unsigned(w));
      TRY Rd.Close(cl.rd) EXCEPT ELSE END;
      TRY Wr.Close(cl.wr) EXCEPT ELSE END;
      RETURN NIL
    END
  END HApply;

  (**********************************************************************)

PROCEDURE DisplayChar(c : CHAR) : CHAR =
  BEGIN
    IF ORD(c) >= 16_20 AND ORD(c) <= 16_7e THEN
      RETURN c
    ELSE
      RETURN '.'
    END
  END DisplayChar;
  
PROCEDURE DebugCharSeq(seq : CharSeq.T) =

  PROCEDURE Push() =
    BEGIN
      IF n = 0 THEN RETURN END;
      WHILE n < CharsPerBlock * BlocksPerLine DO
        Put(' ', TRUE)
      END;
      Debug.Out(F("%s | %s", Wx.ToText(l), Wx.ToText(r)));
      l := Wx.New(); r := Wx.New();
      n := 0
    END Push;

  PROCEDURE Put(c : CHAR; silent := FALSE) =
    BEGIN
      IF n # 0 AND n MOD CharsPerBlock = 0 THEN
        Wx.PutChar(r, ' ');
        Wx.PutChar(l, ' ');
      END;
      Wx.PutChar(l, DisplayChar(c));
      IF silent THEN
        Wx.PutText(r, "   ")
      ELSE
        Wx.PutText(r, " " & Fmt.Pad(Int(ORD(c),base := 16),
                                    length:=2,
                                    padChar:= '0'))
      END;
      INC(n)
    END Put;
    
  CONST
    CharsPerBlock = 4;
    BlocksPerLine = 2;
  VAR
    n := 0;
    l, r := Wx.New();
  BEGIN
    FOR i := 0 TO seq.size()-1 DO
      IF i = 0 THEN
        (* skip *)
      ELSIF i MOD (CharsPerBlock*BlocksPerLine) = 0 THEN
        Push()
      END;
      WITH c = seq.get(i) DO
        Put(c)
      END
    END;
    Push()
  END DebugCharSeq;

  (**********************************************************************)
  
PROCEDURE ReceiveMessage(cl : Instance) RAISES { NetError.OutOfRange } =
  VAR
    hdr : FmModelMessageHdr.T;
    cx  : NetContext.T;
  BEGIN
    hdr := FmModelMessageHdr.Read(cl.rd);
    Debug.Out("Received " & FmModelMessageHdr.Format(hdr));
    cx.rem := hdr.msgLength - FmModelMessageHdr.Length;
    WITH h = handler[hdr.type] DO
      IF h = NIL THEN
        Debug.Out(F("handler for message type %s is NIL",
                    FmModelMsgType.Names[hdr.type]))
      ELSE
        h.handle(hdr, cx, cl)
      END;

      Debug.Out(F("Child code left %s bytes unparsed", Int(cx.rem)));
      VAR seq := NEW(CharSeq.T).init();
      BEGIN
        FOR i := 0 TO cx.rem-1 DO
          seq.addhi(VAL(RdNet.GetU8C(cl.rd, cx),CHAR));
        END;
        DebugCharSeq(seq)
      END;
      <*ASSERT cx.rem=0*>
    END
  END ReceiveMessage;

TYPE
  MsgHandler = OBJECT METHODS
    handle(READONLY hdr : FmModelMessageHdr.T;
           VAR cx       : NetContext.T;
           inst         : Instance) 
    RAISES { NetError.OutOfRange }
  END;

VAR
  handler :=  ARRAY FmModelMsgType.T OF MsgHandler {
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NIL,
  NEW(MsgHandler, handle := HandleMsgIosf),
  NIL,
  NEW(MsgHandler, handle := HandleMsgVersionInfo),
  NIL
  };

PROCEDURE HandleMsgVersionInfo(m            : MsgHandler;
                               READONLY hdr : FmModelMessageHdr.T;
                               VAR cx       : NetContext.T;
                               inst         : Instance) =
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.VersionInfo*>
    WITH versionHdr = FmModelMsgVersionHdr.ReadC(inst.rd, cx),
         remBytes   = cx.rem,
         versionTag = GetStringC(inst.rd,remBytes, cx) DO
      
      Debug.Out("Received FmModelMsgVersionHdr " &
        FmModelMsgVersionHdr.Format(versionHdr));
      Debug.Out(F("versionNum=%s remBytes=%s",
                  Int(versionHdr.versionNum), Int(remBytes)));
      Debug.Out(F("versionTag=%s", GetStringC(inst.rd,remBytes, cx)));
      (* format and send return message *)
      WrNet.PutU16G(inst.sp, Pkt.End.Back, versionHdr.versionNum);
      PutStringS(inst.sp, Pkt.End.Back, versionTag);
      inst.sendResponse()
    END
  END HandleMsgVersionInfo;

PROCEDURE HandleMsgIosf(m            : MsgHandler;
                        READONLY hdr : FmModelMessageHdr.T;
                        VAR cx       : NetContext.T;
                        inst         : Instance) =
  CONST
    DestOff   = 0;
    SourceOff = 1;
    OpcodeOff = 2;
    TagOff    = 3;
    SaiOff    = 5;

    (* following are relative to dataOff *)
    NdwOff      = 0;
    CompDataOff = 0;
  CONST
    MaxReadBaseNdw = 124;
    MaxReadRepNdw  =  14;
    
  TYPE
    Rsp = { Success, Fail };
    U4 = [ 0..16_F ];
  VAR
    dataOff := 4;
    rsp := Rsp.Success;

    expHdr, fbe, sbe : U4;
    sai : U16 := LAST(U16); (* not really is it??? *)

    ndw, ndwOff : CARDINAL;
    eDataStart : CARDINAL;
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.Iosf*>

    Debug.Out(F("cx.rem=%s", Int(cx.rem)));
    
    (* this is basically from model_server.c 
       we can do better!  I think... *)

    WITH port       = hdr.port,
         inbound    = Pkt.FromRd(NEW(Pkt.T).init(), inst.rd, cx),
         iDataLen   = inbound.size(),
         remBytes   = cx.rem,
         saiHdr     = TRUE,
         dest       = ORD(inbound.get(DestOff)),
         opcode     = ORD(inbound.get(OpcodeOff)),
         tagBar     = ORD(inbound.get(TagOff)),
         tag        = Word.Extract(ORD(tagBar), 0, 3),
         bar        = Word.Extract(ORD(tagBar), 3, 3)
     DO
      
      Debug.Out(F("remBytes=%s", Int(remBytes)));
      Debug.Out(F("dest=%s opcode=%s tag=%s bar=%s",
                  Int(dest), Int(opcode), Int(tag), Int(bar)));

      IF saiHdr THEN
        expHdr := Word.Extract(ORD(inbound.get(4)),4,7); (* !?!? *)
        fbe    := Word.Extract(ORD(inbound.get(8)),0,4);
        sbe    := Word.Extract(ORD(inbound.get(8)),4,4);
        sai    := RdNet.GetU16S(inbound, SaiOff, bo := ByteOrder.LE);

        Debug.Out(F("expHdr=%s fbe=%s sbe=%s sai=%s",
                    Int(expHdr), Int(fbe), Int(sbe), Int(sai)));
        dataOff := 8
      END;

      IF bar # 0 THEN rsp := Rsp.Fail END; (* shld log *)

      IF saiHdr THEN
        IF expHdr # 0 THEN rsp := Rsp.Fail END; (* shld log *)

        IF opcode = IosfOp.RegRead OR opcode = IosfOp.RegWrite THEN
          (* shld log *)
          IF fbe # 16_f OR sbe # 16_f AND sbe # 16_0 THEN
            rsp := Rsp.Fail
          END
        END
      END;

      WITH hAddr = Word.RightShift(RdNet.GetU32S(inbound, dataOff+4),12) DO
        Debug.Out("hAddr="&Int(hAddr));
        IF hAddr # 0 THEN rsp := Rsp.Fail END
      END;

      Debug.Out("ORD(rsp)="&Int(ORD(rsp)));

      eDataStart := dataOff + CompDataOff;
      
      IF rsp = Rsp.Success THEN
        WITH addr = RdNet.GetU32S(inbound, dataOff+2) DO
          Debug.Out("addr=0x"&Unsigned(addr));

          CASE opcode OF
            IosfOp.RegBlkRead =>
            Pkt.Put(inst.sp, OpcodeOff, VAL(IosfOp.CompNoData,CHAR));
            ndw := ORD(inbound.get(dataOff + NdwOff));
            ndwOff := dataOff + 8;

            IF ndw > MaxReadBaseNdw THEN
              rsp := Rsp.Fail 
            ELSE
              <*ASSERT inst.sp.size()=eDataStart*>
              REPEAT
                IF Word.And(ndw, 1) = 1 THEN rsp := Rsp.Fail; EXIT END;
                FOR cnt := 0 TO ndw DIV 2 - 1 DO

                  WITH w = 0 (* DoSwitchReadCSR64(sw, addr + cnt*2*4) *)DO
                    WrNet.PutU64G(inst.sp,Pkt.End.Back,w,bo := ByteOrder.LE)
                  END
                END  
                
                




              UNTIL TRUE;
            END(*IF*)
          ELSE
            Debug.Out("Unknown opcode 0x"&Unsigned(opcode))
          END
        END
      END
    END
  END HandleMsgIosf;

PROCEDURE GetStringC(rd      : Rd.T;
                     bufflen : CARDINAL;
                     VAR cx  : NetContext.T) : TEXT =
  VAR
    str := NEW(REF ARRAY OF CHAR, bufflen);
    n := bufflen;
  BEGIN
    FOR i := 0 TO bufflen-1 DO
      WITH c = VAL(RdNet.GetU8C(rd,cx),CHAR) DO
        IF ORD(c) = 0 AND n = bufflen THEN n := i END;
        str[i] := c
      END
    END;
    RETURN Text.FromChars(SUBARRAY(str^, 0, n))
  END GetStringC;
  
PROCEDURE PutStringS(sp : Pkt.T; end : Pkt.End; str : TEXT) =
  BEGIN
    CASE end OF
      Pkt.End.Front =>
      sp.addlo(VAL(0,CHAR));
      FOR i := Text.Length(str)-1 TO 0 BY -1 DO
        sp.addlo(Text.GetChar(str,i))
      END
    |
      Pkt.End.Back =>
      FOR i := 0 TO Text.Length(str)-1 DO
        sp.addhi(Text.GetChar(str,i))
      END;
      sp.addhi(VAL(0,CHAR))
    END
  END PutStringS;
  
BEGIN END ModelServer.
