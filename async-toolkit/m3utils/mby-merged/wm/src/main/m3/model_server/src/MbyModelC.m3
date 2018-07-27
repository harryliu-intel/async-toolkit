UNSAFE MODULE MbyModelC EXPORTS MbyModel, MbyModelC;

IMPORT mby_top_map AS Map;
IMPORT mby_top_map_addr AS MapAddr;
IMPORT ServerPacket AS Pkt;
IMPORT FmModelMessageHdr;
IMPORT FmModelConstants;
IMPORT MbyModelServer;
IMPORT Debug;
IMPORT MbyParserStage;
IMPORT BaseModelStage;
IMPORT Metadata;
IMPORT MbyParserStageModel;
IMPORT mby_top_map_c;
IMPORT Fmt; FROM Fmt IMPORT Int, F;
IMPORT Word;
IMPORT IntSeq AS AddrSeq;
IMPORT UpdaterSeq;
IMPORT UnsafeUpdater;
IMPORT Updater;
IMPORT CompPath;
IMPORT UpdaterFactory;

PROCEDURE HandlePacket(server           : MbyModelServer.T;
                       READONLY readA   : Map.T;
                       READONLY updateA : MapAddr.Update;
                       READONLY hdr     : FmModelMessageHdr.T;
                       pkt              : Pkt.T) =
  BEGIN
  END HandlePacket;

PROCEDURE SetupMby(<*UNUSED*>server : MbyModelServer.T;
                   <*UNUSED*>READONLY read : Map.T;
                   READONLY update : MapAddr.Update) =
  BEGIN
    Debug.Out("SetupMby");
    mby_top_map_c.BuildMain(BuildCallback);
    Debug.Out(F("Mapped %s fields in C struct",Int(addrSeq.size())));
  END SetupMby;

VAR
  addrSeq := NEW(AddrSeq.T).init();
  upSeq   := NEW(UpdaterSeq.T).init();
  
PROCEDURE BuildCallback(addr : ADDRESS) =
  BEGIN
    WITH w = LOOPHOLE(addr,Word.T) DO
      IF FALSE THEN
        Debug.Out("BuildCallback " & Fmt.Unsigned(w))
      END;
      NARROW(upSeq.get(addrSeq.size()),MyUpdater).caddr := addr;
      addrSeq.addhi(w)
    END
  END BuildCallback;

TYPE
  MyUpdater = UnsafeUpdater.T OBJECT
    caddr : ADDRESS;
    w     : [1..64];
  OVERRIDES
    init := MUInit;
  END;

  MyUpdaterFactory = UpdaterFactory.T OBJECT
  OVERRIDES
    buildT := BuildT;
  END;

PROCEDURE BuildT(<*UNUSED*>f : MyUpdaterFactory) : Updater.T =
  BEGIN RETURN NEW(MyUpdater, doSync := TRUE, sync := MUSync) END BuildT;

PROCEDURE GetUpdaterFactory() : UpdaterFactory.T =
  BEGIN
    RETURN NEW(MyUpdaterFactory);
  END GetUpdaterFactory;

PROCEDURE MUInit(up : MyUpdater; base : REFANY; fieldAddr : ADDRESS; width : CARDINAL; nm : CompPath.T) : UnsafeUpdater.T =
  BEGIN
    EVAL UnsafeUpdater.T.init(up, base, fieldAddr, width, nm);
    up.w := width;
    upSeq.addhi(up);
    RETURN up
  END MUInit;

PROCEDURE MUSync(up : MyUpdater) =
  BEGIN
    (* this routine is called to copy the Modula-3 value to C *)
    (* in particular it is called by CsrAccess, because doSync is TRUE *)
    MUUpdate(up, up.value())
  END MUSync;
  
PROCEDURE MUUpdate(up : MyUpdater; to : Word.T) =
  VAR
    ptr := up.caddr;
  BEGIN
    UnsafeUpdater.T.update(up, to);
    CASE up.w OF
      1..8 =>
      LOOPHOLE(ptr, UNTRACED REF [0..16_ff])^ := to
    |
      9..16 =>
      LOOPHOLE(ptr, UNTRACED REF [0..16_ffff])^ := to
    |
      17..32 =>
      LOOPHOLE(ptr, UNTRACED REF [0..16_ffffffff])^ := to
    |
      33..64 =>
      LOOPHOLE(ptr, UNTRACED REF Word.T)^ := to;
    ELSE
      <*ASSERT FALSE*>
    END
  END MUUpdate;

BEGIN END MbyModelC.
