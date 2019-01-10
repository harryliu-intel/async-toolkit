MODULE StageModelServer;

IMPORT ModelServerSuper;

IMPORT UpdaterFactory, Pathname;
IMPORT FmModelMsgType;

FROM ModelServerSuper IMPORT Instance, MsgHandler;
FROM ModelServerUtils IMPORT HandleMsgCommandQuit;
IMPORT MsIosf;


REVEAL
  T = Public BRANDED Brand OBJECT
  OVERRIDES
    init := Init;
  END;

  PROCEDURE Init(t                    : T;
               <*UNUSED*>factory    : UpdaterFactory.T;
               infoPath             : Pathname.T;
               quitOnLastClientExit : BOOLEAN;
               infoFile             : Pathname.T) : T =
  BEGIN
    EVAL ModelServerSuper.T.init(t, infoPath, infoFile, handlers);
    t.quitOnLastClientExit := quitOnLastClientExit;
    RETURN t
  END Init;

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
                                      handle := HandleMsgCommandQuit)
  };

BEGIN END StageModelServer.
