MODULE Main;
IMPORT HlpModelServer;
IMPORT Thread;
IMPORT Pathname, Env;
IMPORT hlp_top_map      AS Map;
IMPORT hlp_top_map_addr AS MapAddr;
IMPORT Debug;

PROCEDURE SetupHlp(<*UNUSED*>server : HlpModelServer.T;
                   <*UNUSED*>READONLY read : Map.T;
                   READONLY update : MapAddr.Update) =
  BEGIN
    Debug.Out("SetupHlp");
    
    update.Imn.BsmScratch3[509].Data.u(16_1109);
    (* match fpps_mgmt.c:546 *)
    
    update.Imn.FuseData[3].Data.u(16_6);
    (* match fpps_switch.c:481 *)
    
  END SetupHlp;

VAR
  modelServer : HlpModelServer.T;
  infoPath : Pathname.T;
BEGIN
  infoPath := Env.Get("WMODEL_INFO_PATH");
  IF infoPath = NIL THEN infoPath := "." END;

  modelServer := NEW(HlpModelServer.T,
                     setupChip := SetupHlp)
                .init(infoPath := infoPath);

  modelServer.resetChip();

  EVAL modelServer.listenFork();

  LOOP Thread.Pause(1.0d0) END
  
END Main.
