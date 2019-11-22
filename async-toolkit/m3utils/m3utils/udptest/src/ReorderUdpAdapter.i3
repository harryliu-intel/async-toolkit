INTERFACE ReorderUdpAdapter;
IMPORT UdpAdapter;
IMPORT Random;
IMPORT IP;

TYPE
  T <: Public;

  Public = UdpAdapter.T OBJECT METHODS
    init(myPort: IP.Port;
         myAddr := IP.NullAddress;
         underlying : UdpAdapter.T := NIL;
         reorderProb, dropProb, dupProb := 0.0d0;
         rand : Random.T := NIL): T
      RAISES {IP.Error};
    setParams(reorderProb, dropProb, dupProb := 0.0d0;
              rand       : Random.T := NIL);
  END;

  (* init, underlying is the underlying UDP implementation.
     if underlying is NIL, a new UdpAdapter.Default will be
     allocated *)
  
CONST Brand = "ReorderUdpAdapter";

END ReorderUdpAdapter.
