INTERFACE UdpDatagram;
IMPORT UDP;

TYPE T = UDP.Datagram;

CONST Brand = "UdpDatagram";

PROCEDURE Copy(READONLY a : T) : T;
  (* perform a "deep copy" of a *)

END UdpDatagram.
