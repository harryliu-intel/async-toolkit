INTERFACE N7Tech;
IMPORT Corner;

TYPE
  Transistor = { Ulvt, Lvt, Svt };

CONST TranLeakageRatio = ARRAY Transistor OF LONGREAL { 24.9d0, 5.7d0, 1.0d0 };

CONST CornerLkgRatio = ARRAY Corner.T OF LONGREAL { 4.0d0, 2.0d0, 1.0d0 };

CONST CornerSigma = ARRAY Corner.T OF LONGREAL { -3.0d0, 0.0d0, +3.0d0 };

CONST LkgTempCoeff = 0.03d0; (* leakage increase per kelvin *)

END N7Tech.
