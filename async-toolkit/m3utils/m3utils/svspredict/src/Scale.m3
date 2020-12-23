MODULE Scale;
IMPORT Math;
IMPORT N7Tech AS Tech; (* make generic... *)

PROCEDURE LkgPwrByT(from, to : LONGREAL) : LONGREAL =
  BEGIN
    RETURN Math.pow(1.0d0 + Tech.LkgTempCoeff, to - from)
  END LkgPwrByT;

PROCEDURE LkgPwrByV(from, to : LONGREAL) : LONGREAL =
  BEGIN
    RETURN DynPwrByV(from, to) (* dont have better idea right now *)
  END LkgPwrByV;

PROCEDURE DynPwrByT(<*UNUSED*>from, to : LONGREAL) : LONGREAL =
  BEGIN RETURN 1.0d0 END DynPwrByT;

PROCEDURE DynPwrByV(from, to : LONGREAL) : LONGREAL =
  BEGIN RETURN from * from / to / to END DynPwrByV;

BEGIN END Scale.
