INTERFACE HlpModelServer;
IMPORT ModelServer;
IMPORT hlp_top_map      AS Map;
IMPORT hlp_top_map_addr AS MapAddr;

TYPE
  T <: Public;

  Super = ModelServer.T;
  
  Public = Super OBJECT METHODS
    setupChip(READONLY read : Map.T; READONLY update : MapAddr.Update);
  END;

CONST Brand = "HlpModelServer";

END HlpModelServer.
