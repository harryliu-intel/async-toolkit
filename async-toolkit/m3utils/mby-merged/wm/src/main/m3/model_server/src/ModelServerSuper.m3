MODULE ModelServerSuper;

IMPORT TCP;
IMPORT Thread, RefSeq;
IMPORT Debug, AL, ConnRW, Wr;
FROM Fmt IMPORT F, Int, Unsigned;
IMPORT OSError, Pathname, IP, FileWr;
IMPORT Rd, NetError;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr, FmModelMsgType;
IMPORT Process;
IMPORT NetContext;
IMPORT RdNet;
IMPORT ByteSeq;
IMPORT Compiler;

(* may keep : *)
<*FATAL Thread.Alerted*>

(* should not keep : for now also: *)
<*FATAL OSError.E, IP.Error*>
<*FATAL NetContext.Short*>

VAR doDebug := Debug.DebugThis(Brand);

REVEAL
  T = Public BRANDED Brand OBJECT
    port                 : IP.Port;
    conn                 : TCP.Connector; (* listen here *)
    infoPath             : Pathname.T;
    infoFile             : Pathname.T;
    handler : ARRAY FmModelMsgType.T OF MsgHandler;
  OVERRIDES
    listenFork    := ListenFork;
    init := Init;
  END;

PROCEDURE Init(t                    : T;
               infoPath, infoFile   : Pathname.T;
               quitOnLastClientExit : BOOLEAN;
               READONLY handler     : ARRAY FmModelMsgType.T OF MsgHandler) : T =
  BEGIN
    t.infoFile := infoFile;
    t.infoPath := infoPath;
    t.handler := handler;
    t.quitOnLastClientExit := quitOnLastClientExit;
    WITH ep   = IP.Endpoint { IP.NullAddress, IP.NullPort },
         conn = TCP.NewConnector(ep) DO
      t.port := TCP.GetEndPoint(conn).port;
      Debug.Out("PORT " & Int(t.port));
      t.conn := conn
    END;
    RETURN t
  END Init;

REVEAL
  Listener = PubListener BRANDED Brand & " Listener" OBJECT
    t        : T;
    handlers : RefSeq.T;
    conn     : TCP.Connector;
  OVERRIDES
    apply := LCApply;
  END;

PROCEDURE ListenFork(t : T) : Listener =
  BEGIN
    WITH listener = NEW(Listener,
                        t        := t,
                        handlers := NEW(RefSeq.T).init(),
                        conn     := t.conn) DO
      EVAL Thread.Fork(listener);
      RETURN listener
    END
  END ListenFork;

VAR
  clientCountMu := NEW(MUTEX);
  clientCount   : CARDINAL := 0; (* global count of all clients connected to 
                                    this process *)

PROCEDURE LCApply(cl : Listener) : REFANY =
  BEGIN
    TRY
      WriteInfo(cl.t.infoPath, cl.t.infoFile, cl.t.port);
    EXCEPT
      Wr.Failure(x) =>
      Debug.Error(F("Caught Wr.Failure attempting to write to \"%s\" : %s",
                    cl.t.infoPath, AL.Format(x)))
    END;

    LOOP
      WITH tcp = TCP.Accept(cl.conn),
           wr = ConnRW.NewWr(tcp),
           rd = ConnRW.NewRd(tcp),
           h  = NEW(Instance).init(cl.t, rd, wr) DO
        LOCK clientCountMu DO
          INC(clientCount)
        END;
        cl.handlers.addhi(h);
        EVAL Thread.Fork(h)
      END
    END
  END LCApply;

PROCEDURE WriteInfo(dirPath, fileName : Pathname.T; port : IP.Port)
  RAISES { OSError.E, Wr.Failure } =
  VAR
    infoPath := dirPath & "/" & fileName;
  BEGIN
    Debug.Out("Writing info to " & infoPath);
    WITH wr = FileWr.Open(infoPath) DO
      Wr.PutText(wr, F("0:localhost:%s\n", Int(port)));
      Wr.Close(wr)
    END
  END WriteInfo;

REVEAL
  Instance = PubInstance
     BRANDED Brand & " Instance"
  OBJECT METHODS
    init(t : T; rd : Rd.T; wr : Wr.T) : Instance := InitI;
    
    receiveMessage()
      RAISES { NetError.OutOfRange, Rd.EndOfFile, Rd.Failure, ParseError,
               Wr.Failure}
      := ReceiveMessage;
  OVERRIDES
    sendResponse := SendResponse;
    apply := HApply;
  END;

PROCEDURE InitI(i : Instance; t : T; rd : Rd.T; wr : Wr.T) : Instance =
  BEGIN
    i.t  := t;
    i.rd := rd;
    i.wr := wr;
    i.sp := NEW(Pkt.T).init();
    RETURN i
  END InitI;

PROCEDURE SendResponse(i : Instance) RAISES { Wr.Failure, Thread.Alerted } =
  BEGIN
    IF doDebug THEN Debug.Out(F("SendResponse: %s bytes",Int(i.sp.size()))) END;
    (* add outer header *)
    VAR
      hdr := i.lastHdr;
    BEGIN
      hdr.msgLength := i.sp.size() + FmModelMessageHdr.Length;
      FmModelMessageHdr.WriteE(i.sp, Pkt.End.Front, hdr)
    END;

    IF doDebug THEN Pkt.DebugOut(i.sp) END;
    Pkt.Transmit(i.sp,i.wr);
    EVAL i.sp.init() (* clear buffer *)
  END SendResponse;

PROCEDURE HApply(cl : Instance) : REFANY =

  PROCEDURE DropClient() =
    BEGIN
      TRY Rd.Close(cl.rd) EXCEPT ELSE END;
      TRY Wr.Close(cl.wr) EXCEPT ELSE END;
      LOCK clientCountMu DO
        DEC(clientCount);
        IF cl.t.quitOnLastClientExit AND clientCount = 0 THEN
          Debug.Out("Quitting on last client disconnect");
          Process.Exit(0)
        END
      END
    END DropClient;
  
  BEGIN
    Debug.Out("HApply");
    TRY
      LOOP
        cl.receiveMessage()
      END
    EXCEPT
      ParseError(txt) =>
      Debug.Warning("Caught ParseError : " & txt);
      DropClient()
    |
      NetError.OutOfRange(w) =>
      Debug.Warning("Caught NetError.OutOfRange : " & Unsigned(w));
      DropClient()
    |
      Rd.EndOfFile =>
      Debug.Warning("Client disconnected");
      DropClient()
    |
      Rd.Failure(x) =>
      Debug.Warning("Error communicating with client disconnected : Rd.Failure : "&
        AL.Format(x));
      DropClient()
    |
      Wr.Failure(x) =>
      Debug.Warning("Error communicating with client disconnected : Wr.Failure : "&
        AL.Format(x));
      DropClient()
    END;
    RETURN NIL
  END HApply;

PROCEDURE ReceiveMessage(cl : Instance) RAISES { NetError.OutOfRange,
                                                 Rd.EndOfFile,
                                                 Rd.Failure,
                                                 ParseError,
                                                 Wr.Failure } =
  VAR
    hdr : FmModelMessageHdr.T;
    cx  : NetContext.T;
  BEGIN
    hdr := FmModelMessageHdr.Read(cl.rd);
    IF doDebug THEN Debug.Out("Received " & FmModelMessageHdr.Format(hdr)) END;
    cl.lastHdr := hdr;
    cx.rem := hdr.msgLength - FmModelMessageHdr.Length;
    WITH h = cl.t.handler[hdr.type] DO
      IF h = NIL THEN
        Debug.Warning(F("handler for message type %s is NIL",
                    FmModelMsgType.Names[hdr.type]))
      ELSE
        h.handle(hdr, cx, cl)
      END;

      IF cx.rem # 0 THEN
        Debug.Warning(F("%s:%s: child <%s> code left %s bytes unparsed",
                        Compiler.ThisFile(),
                        Int(Compiler.ThisLine()),
                        FmModelMsgType.Names[hdr.type],
                        Int(cx.rem)))
      END;
      VAR
        seq := NEW(ByteSeq.T).init();
      BEGIN
        FOR i := 0 TO cx.rem-1 DO
          seq.addhi(RdNet.GetU8C(cl.rd, cx));
        END;
        IF doDebug THEN Pkt.DebugOut(seq) END
      END;
      <*ASSERT cx.rem=0*>
    END
  END ReceiveMessage;

BEGIN END ModelServerSuper.
