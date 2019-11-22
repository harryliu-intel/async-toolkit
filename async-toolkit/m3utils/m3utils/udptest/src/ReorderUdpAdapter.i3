INTERFACE ReorderUdpAdapter;
IMPORT UdpAdapter;
IMPORT Random;

TYPE
  T <: Public;

  Public = UdpAdapter.T OBJECT METHODS
    setParams(reorderProb, dropProb := 0.0d0;
              rand       : Random.T := NIL);
  END;

CONST Brand = "ReorderUdpAdapter";

END ReorderUdpAdapter.
