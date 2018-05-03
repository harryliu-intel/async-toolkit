MODULE ModelServer;
IMPORT hlp_top_map AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT CompAddr;
IMPORT Debug;
IMPORT Thread;
FROM Fmt IMPORT F, Int, Unsigned, Bool;
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
IMPORT ByteSeq;
IMPORT ServerPacket AS Pkt;
IMPORT Word;
FROM NetTypes IMPORT U8, U16, U32, U64, ByteOrder;
IMPORT IosfRegBlkWriteReqHdr;
IMPORT IosfRegBlkReadReqHdr;
IMPORT CsrOp;

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
    t  : T;
  METHODS
    init(t : T; rd : Rd.T; wr : Wr.T) : Instance := InitI;
    
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

PROCEDURE InitI(i : Instance; t : T; rd : Rd.T; wr : Wr.T) : Instance =
  BEGIN
    i.t  := t;
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
           h  = NEW(Instance).init(cl.t, rd, wr) DO
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
  
PROCEDURE DebugByteSeq(seq : ByteSeq.T) =

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
        Put(VAL(c,CHAR))
      END
    END;
    Push()
  END DebugByteSeq;

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
      VAR seq := NEW(ByteSeq.T).init();
      BEGIN
        FOR i := 0 TO cx.rem-1 DO
          seq.addhi(RdNet.GetU8C(cl.rd, cx));
        END;
        DebugByteSeq(seq)
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
  TYPE
    Rsp = { Success, Fail };
    U4 = [ 0..16_F ];
  VAR
    rsp := Rsp.Success;
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.Iosf*>

    Debug.Out(F("cx.rem=%s", Int(cx.rem)));

    WITH port       = hdr.port,
         inbound    = Pkt.FromRd(NEW(Pkt.T).init(), inst.rd, cx) DO
      (* packet data is loaded into inbound *)

      DebugByteSeq(inbound);
      
      VAR req : IosfRegBlkWriteReqHdr.T;
          match := IosfRegBlkWriteReqHdr.ReadSB(inbound, 0, req);
      BEGIN
        Debug.Out("match = " & Bool(match))
      END;
      VAR req      : IosfRegBlkReadReqHdr.T;
          match    := IosfRegBlkReadReqHdr.ReadSB(inbound, 0, req);
          addr     := Word.Insert(req.addr0,req.addr1,16,12);
          ca       : CompAddr.T;
      BEGIN
        Debug.Out("match = " & Bool(match));

        IF match THEN
          Debug.Out(IosfRegBlkReadReqHdr.Format(req));

          FOR a := addr TO addr + (req.ndw-1)*4 BY 4 DO
            ca := CompAddr.FromBytes(a);
            Debug.Out("ca=" & CompAddr.Format(ca,FALSE));

            WITH csrOp = CsrOp.MakeRead(ca, 32, CsrOp.Origin.Software) DO
              EVAL inst.t.h.csrOp(csrOp)
            END
            
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
      sp.addlo(0);
      FOR i := Text.Length(str)-1 TO 0 BY -1 DO
        sp.addlo(ORD(Text.GetChar(str,i)))
      END
    |
      Pkt.End.Back =>
      FOR i := 0 TO Text.Length(str)-1 DO
        sp.addhi(ORD(Text.GetChar(str,i)))
      END;
      sp.addhi(0)
    END
  END PutStringS;
  
BEGIN END ModelServer.
