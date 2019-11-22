INTERFACE UdpAdapter;
IMPORT UDP;
IMPORT IP;

TYPE
  T = UDP.T OBJECT METHODS
  END;

  Default <: T;
  (* an adapter wrapped around a regular UDP.T *)
  
CONST Brand = "UdpAdapter";

END UdpAdapter.
      
