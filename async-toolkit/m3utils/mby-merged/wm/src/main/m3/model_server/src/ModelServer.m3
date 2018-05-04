MODULE ModelServer;
IMPORT ModelServerClass;

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
IMPORT ByteSeq;
IMPORT ServerPacket AS Pkt;
IMPORT AL;
IMPORT MsIosf;

(* may keep : *)
<*FATAL Thread.Alerted*>

(* should not keep : for now also: *)
<*FATAL OSError.E, Wr.Failure, IP.Error*>
<*FATAL NetContext.Short*>
<*FATAL NetError.OutOfRange*>

REVEAL
  T = Public BRANDED Brand OBJECT
    port     : IP.Port;
    conn     : TCP.Connector; (* listen here *)
    listener : Listener := NIL;
    infoPath : Pathname.T;
  OVERRIDES
    init       := Init;
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
    t        : T;
    handlers : RefSeq.T;
    conn     : TCP.Connector;
  OVERRIDES
    apply := LCApply;
  END;

TYPE
  Instance = ModelServerClass.Instance;

REVEAL
  ModelServerClass.Instance = ModelServerClass.PubInstance
     BRANDED ModelServerClass.Brand & " Instance"
  OBJECT METHODS
    init(t : T; rd : Rd.T; wr : Wr.T) : Instance := InitI;
    
    receiveMessage()
      RAISES { NetError.OutOfRange, Rd.EndOfFile, Rd.Failure, ParseError }
      := ReceiveMessage;
  OVERRIDES
    sendResponse := SendResponse;
    apply := HApply;
  END;

PROCEDURE SendResponse(i : Instance) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    Debug.Out(F("SendResponse: %s bytes", Int(i.sp.size())));
    (* add outer header *)
    VAR
      hdr := i.lastHdr;
    BEGIN
      hdr.msgLength := i.sp.size() + FmModelMessageHdr.Length;
      FmModelMessageHdr.WriteE(i.sp, Pkt.End.Front, hdr)
    END;

    Pkt.DebugOut(i.sp);
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
    RETURN t
  END Init;

PROCEDURE HApply(cl : Instance) : REFANY =

  PROCEDURE DropClient() =
    BEGIN
      TRY Rd.Close(cl.rd) EXCEPT ELSE END;
      TRY Wr.Close(cl.wr) EXCEPT ELSE END;
    END DropClient;
  
  BEGIN
    Debug.Out("HApply");
    TRY
      LOOP
        cl.receiveMessage()
      END
    EXCEPT
      ParseError(txt) =>
      Debug.Out("Caught ParseError : " & txt);
      DropClient()
    |
      NetError.OutOfRange(w) =>
      Debug.Out("Caught NetError.OutOfRange : " & Unsigned(w));
      DropClient()
    |
      Rd.EndOfFile =>
      Debug.Out("Client disconnected");
      DropClient()
    |
      Rd.Failure(x) =>
      Debug.Out("Error communicating with client disconnected : Rd.Failure : "&
        AL.Format(x));
      DropClient()
    END;
    RETURN NIL
  END HApply;

  (**********************************************************************)

PROCEDURE ReceiveMessage(cl : Instance) RAISES { NetError.OutOfRange,
                                                 Rd.EndOfFile,
                                                 Rd.Failure,
                                                 ParseError } =
  VAR
    hdr : FmModelMessageHdr.T;
    cx  : NetContext.T;
  BEGIN
    hdr := FmModelMessageHdr.Read(cl.rd);
    Debug.Out("Received " & FmModelMessageHdr.Format(hdr));
    cl.lastHdr := hdr;
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
        Pkt.DebugOut(seq)
      END;
      <*ASSERT cx.rem=0*>
    END
  END ReceiveMessage;

TYPE MsgHandler = ModelServerClass.MsgHandler;
     
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
  NEW(MsgHandler, handle := MsIosf.HandleMsg),
  NIL,
  NEW(MsgHandler, handle := HandleMsgVersionInfo),
  NIL
  };

PROCEDURE HandleMsgVersionInfo(<*UNUSED*>m  : MsgHandler;
                               READONLY hdr : FmModelMessageHdr.T;
                               VAR cx       : NetContext.T;
                               inst         : Instance)
  RAISES { Rd.EndOfFile, Rd.Failure }=
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

PROCEDURE GetStringC(rd      : Rd.T;
                     bufflen : CARDINAL;
                     VAR cx  : NetContext.T) : TEXT
  RAISES { Rd.EndOfFile, Rd.Failure } =
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
