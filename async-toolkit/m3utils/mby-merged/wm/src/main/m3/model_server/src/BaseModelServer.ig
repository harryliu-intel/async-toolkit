GENERIC INTERFACE BaseModelServer(ModelServer, Map, MapAddr);

TYPE
  T <: Public;

  Super = ModelServer.T;
  
  Public = Super OBJECT
    h        : MapAddr.H;
  METHODS
    setup(READONLY read : Map.T; READONLY update : MapAddr.Update);
  END;

CONST Brand = Map.Brand & "/ ModelServer";

END BaseModelServer.
