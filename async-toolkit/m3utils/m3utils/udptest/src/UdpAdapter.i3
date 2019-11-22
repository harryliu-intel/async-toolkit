INTERFACE UdpAdapter;
IMPORT UDP;
IMPORT IP;

TYPE
  T = UDP.T OBJECT METHODS
    eot()
      RAISES {IP.Error};
    (* client calls eot to indicate that no further transmission is 
       forthcoming *)
  END;

  Default <: T;
  (* an adapter wrapped around a regular UDP.T *)
  
CONST Brand = "UdpAdapter";

END UdpAdapter.
      
