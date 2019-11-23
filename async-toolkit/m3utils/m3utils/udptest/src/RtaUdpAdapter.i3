INTERFACE RtaUdpAdapter;
IMPORT NestedUdpAdapter;
IMPORT IP;

TYPE
  T <: Public;

  Public = NestedUdpAdapter.T OBJECT METHODS
  END;

CONST Brand = "RtaUdpAdapter";

CONST RtaManagerPort : IP.Port = 17748;
  (* well-known TCP port for RTA manager *)

(* the following describes UDP port ranges that are managed by this interfaces *)
      
TYPE
  PortRange = RECORD
    listen   : IP.Port;
    basePort : IP.Port;
    size     : CARDINAL;
  END;

  PortRanges = ARRAY OF PortRange;

CONST
  TestPortRanges = PortRanges {
    PortRange { RtaManagerPort,       0, 32768 },
    PortRange { RtaManagerPort+1, 32768, 32768 }
  };

  RealPortRanges = PortRanges {
    PortRange { RtaManagerPort, FIRST(IP.Port), NUMBER(IP.Port) }
  };

PROCEDURE Initialize(READONLY portRanges : PortRanges);
      
END RtaUdpAdapter.
