GENERIC INTERFACE BaseModelServer(Map, MapAddr);
IMPORT ModelServer;

TYPE
  T <: Public;

  Super = ModelServer.T;
  
  Public = Super OBJECT METHODS
    setupChip(READONLY read : Map.T; READONLY update : MapAddr.Update);
  END;

CONST Brand = Map.Brand & "/ ModelServer";

END BaseModelServer.
