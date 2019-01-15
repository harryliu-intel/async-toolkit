MODULE ModelServerUtils;
FROM ModelServerSuper IMPORT Instance, MsgHandler;
IMPORT NetContext;
IMPORT Rd, Wr;
IMPORT Debug, Process;
FROM Fmt IMPORT F, Int;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMsgType;
IMPORT FmModelMessageHdr;
IMPORT FmModelMsgVersionHdr;
IMPORT Text;
IMPORT Thread;
IMPORT RdNet, WrNet;

<*FATAL Thread.Alerted*>

VAR doDebug := Debug.DebugThis(Brand);
  
PROCEDURE HandleMsgCommandQuit(<*UNUSED*>m      : MsgHandler;
                               READONLY hdr     : FmModelMessageHdr.T;
                               <*UNUSED*>VAR cx : NetContext.T;
                               <*UNUSED*>inst   : Instance)
  RAISES { } =
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.CommandQuit*>
    (* not elegant *)
    Debug.Out("Received CommandQuit from socket stream, going down.");
    Process.Exit(0)
  END HandleMsgCommandQuit;
  
PROCEDURE HandleMsgVersionInfo(<*UNUSED*>m  : MsgHandler;
                               READONLY hdr : FmModelMessageHdr.T;
                               VAR cx       : NetContext.T;
                               inst         : Instance)
  RAISES { Rd.EndOfFile, Rd.Failure, Wr.Failure }=
  BEGIN
    <*ASSERT hdr.type = FmModelMsgType.T.VersionInfo*>
    WITH versionHdr = FmModelMsgVersionHdr.ReadC(inst.rd, cx),
         remBytes   = cx.rem,
         versionTag = GetStringC(inst.rd,remBytes, cx) DO

      IF doDebug THEN
        Debug.Out("Received FmModelMsgVersionHdr " &
          FmModelMsgVersionHdr.Format(versionHdr));
        Debug.Out(F("versionNum=%s remBytes=%s",
                    Int(versionHdr.versionNum), Int(remBytes)));
        Debug.Out(F("versionTag=%s", GetStringC(inst.rd,remBytes, cx)));
      END;
      
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
  
BEGIN END ModelServerUtils.
