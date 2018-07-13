INTERFACE MbyTypes;
IMPORT Byte AS ByteIntf;

TYPE
  PacketLen    = [0..16_ffff]; (* somewhat arbitrary *)
  Port         = [0..16_7f];   (* somewhat arbitrary *)
  RxEplFlags   = [0..16_f];
  Byte         = ByteIntf.T;
  SegmentLen   = [0..16_ffff]; (* cant be right *)

CONST Brand = "MbyTypes";

END MbyTypes.
