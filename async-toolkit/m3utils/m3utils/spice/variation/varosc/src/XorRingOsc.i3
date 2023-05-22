INTERFACE RingOsc;

CONST
  NP = 4;
  N  = NT * 2 * NP; (* = 80 *)

  XaVar     = ARRAY [0..NP-1] OF TEXT { "gh:ghmat", "gh:lermat", "noia:noiv", "fms:vtsingle" };
  HspiceVar = ARRAY [0..NP-1] OF TEXT { "ghmat", "lermat", "noiv", "vtsingle" };

  NT = 10;

  XaTranUlvt = ARRAY [ 0..NT-1] OF TEXT {
  "MMg1.qna.mn1:@:nhpbulvt.1",
  "MMg1.qpa.mp1:@:phpbulvt.1",
  "MMg2.qns.mn1:@:nhpbulvt.1",
  "MMg2.qpsb.mp1:@:phpbulvt.1",
  "MMg3.qna.mn1:@:nhpbulvt.1",
  "MMg3.qpa.mp1:@:phpbulvt.1",
  "MMg4.qns.mn1:@:nhpbulvt.1",
  "MMg4.qpsb.mp1:@:phpbulvt.1",
  "MMg5.qna.mn1:@:nhpbulvt.1",
  "MMg5.qpa.mp1:@:phpbulvt.1"
};

  HspiceTranUlvt = ARRAY [ 0..NT-1] OF TEXT {
  "MMg1.qna.mn1:@:nhpbulvt",
  "MMg1.qpa.mp1:@:phpbulvt",
  "MMg2.qns.mn1:@:nhpbulvt",
  "MMg2.qpsb.mp1:@:phpbulvt",
  "MMg3.qna.mn1:@:nhpbulvt",
  "MMg3.qpa.mp1:@:phpbulvt",
  "MMg4.qns.mn1:@:nhpbulvt",
  "MMg4.qpsb.mp1:@:phpbulvt",
  "MMg5.qna.mn1:@:nhpbulvt",
  "MMg5.qpa.mp1:@:phpbulvt"
};

  XaTranLvt = ARRAY [ 0..NT-1] OF TEXT {
  "MMg1.qna.mn1:@:nhpblvt.1",
  "MMg1.qpa.mp1:@:phpblvt.1",
  "MMg2.qns.mn1:@:nhpblvt.1",
  "MMg2.qpsb.mp1:@:phpblvt.1",
  "MMg3.qna.mn1:@:nhpblvt.1",
  "MMg3.qpa.mp1:@:phpblvt.1",
  "MMg4.qns.mn1:@:nhpblvt.1",
  "MMg4.qpsb.mp1:@:phpblvt.1",
  "MMg5.qna.mn1:@:nhpblvt.1",
  "MMg5.qpa.mp1:@:phpblvt.1"
};

  HspiceTranLvt = ARRAY [ 0..NT-1] OF TEXT {
  "MMg1.qna.mn1:@:nhpblvt",
  "MMg1.qpa.mp1:@:phpblvt",
  "MMg2.qns.mn1:@:nhpblvt",
  "MMg2.qpsb.mp1:@:phpblvt",
  "MMg3.qna.mn1:@:nhpblvt",
  "MMg3.qpa.mp1:@:phpblvt",
  "MMg4.qns.mn1:@:nhpblvt",
  "MMg4.qpsb.mp1:@:phpblvt",
  "MMg5.qna.mn1:@:nhpblvt",
  "MMg5.qpa.mp1:@:phpblvt"
};

END RingOsc.
