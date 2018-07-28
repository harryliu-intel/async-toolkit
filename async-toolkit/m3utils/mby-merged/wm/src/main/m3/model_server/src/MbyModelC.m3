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
IMPORT ModelCWrite;
IMPORT SortedIntUpdaterTbl AS AddrUpdaterTbl;

(* much of the code in here is really quite generic and should not
   be prefixed Mby...  we should considering moving it out into a
   more generically named module at some point *)

PROCEDURE HandlePacket(server           : MbyModelServer.T;
                       READONLY readA   : Map.T;
                       READONLY updateA : MapAddr.Update;
                       READONLY hdr     : FmModelMessageHdr.T;
                       pkt              : Pkt.T) =
  BEGIN
  END HandlePacket;

VAR
  rp, wp : UNTRACED REF ADDRESS := NEW(UNTRACED REF ADDRESS);
  
PROCEDURE SetupMby(<*UNUSED*>server : MbyModelServer.T;
                   <*UNUSED*>READONLY read : Map.T;
                   READONLY update : MapAddr.Update) =
  BEGIN
    Debug.Out("SetupMby");
    mby_top_map_c.BuildMain(BuildCallback, rp, wp);
    Debug.Out(F("Mapped %s fields in C struct",Int(addrSeq.size())));
    (* now call the C setup, if any *)
    mby_top_map_c.Setup(rp^, wp^)
  END SetupMby;

(* In the following we are setting up linkage to the C struct --
   we provide bidirectional linkage, so that:

   -- writes from m3 to the m3 record are mirrored in the C struct

   -- writes from C go through our m3 Updater, so that they go
      the normal path for us (and are mirrored to C by the mechanism
      above)

   -- writes from sideband go to both the m3 and C records, by 
      forcing a sync() after every write (doSync = TRUE)

   The writes from the C-side are most of what's going on here.

   On initialization, the C struct is walked in the same order that
   the m3 struct is walked, building a correspondence from every
   UnsafeUpdater to the byte address of each field of the C struct.

   On writes, the C-side calls a special write routine write_field
   that passes the pointer that is to be written to the m3 side.
   The m3 side looks up the pointer in a hash table and calls the
   corresponding Updater's update method, which sets the correct
   sequence in action to update all three state representations.

   (Recall that the M3 side already has both a flat view and a 
   record view---we are adding a third view in the C struct
   view.)
*)
  
VAR
  addrSeq        := NEW(AddrSeq.T).init();
  upSeq          := NEW(UpdaterSeq.T).init();
  addrUpdaterTbl := NEW(AddrUpdaterTbl.Default).init();
  
PROCEDURE BuildCallback(addr : ADDRESS) =
  BEGIN
    WITH w = LOOPHOLE(addr,Word.T) DO
      IF FALSE THEN
        Debug.Out("BuildCallback " & Fmt.Unsigned(w))
      END;
      WITH up = NARROW(upSeq.get(addrSeq.size()),MyUpdater) DO
        up.caddr := addr;
        WITH hadIt = addrUpdaterTbl.put(w, up) DO <*ASSERT NOT hadIt*> END
      END;
      addrSeq.addhi(w)
    END
  END BuildCallback;

TYPE
  MyUpdater = UnsafeUpdater.T OBJECT
    caddr : ADDRESS;  (* address of the field in the C struct *)
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

PROCEDURE MUInit(up : MyUpdater;
                 base : REFANY;
                 fieldAddr : ADDRESS;
                 width : CARDINAL;
                 nm : CompPath.T) : UnsafeUpdater.T =
  BEGIN
    (* build a regular UnsafeUpdater but add extra linkage for the
       C struct *)
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

PROCEDURE C2M3Callback(addr : ADDRESS; val : Word.T) =
  VAR
    up : Updater.T;
  BEGIN
    WITH hadIt = addrUpdaterTbl.get(LOOPHOLE(addr,Word.T),up) DO
      <*ASSERT hadIt*>
      up.update(val)
    END
  END C2M3Callback;

BEGIN
  ModelCWrite.SetCallback(C2M3Callback)
END MbyModelC.
