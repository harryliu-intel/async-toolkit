(* $Id: SchemeProfilerSysDepPM3.m3,v 1.1 2009/03/18 19:01:35 mika Exp $ *)
UNSAFE MODULE SchemeProfilerSysDepPM3 EXPORTS SchemeProfilerSysDep;
FROM Uresource IMPORT struct_rusage, struct_rusage_star;
IMPORT Uresource;
FROM Ctypes IMPORT int;
IMPORT Uerror;

PROCEDURE GetErrno() : INTEGER = BEGIN RETURN Uerror.errno END GetErrno;

PROCEDURE getrusage(who: int; VAR r: struct_rusage): int = 
  BEGIN RETURN Uresource.getrusage(who,LOOPHOLE(ADR(r),struct_rusage_star)) END getrusage;

BEGIN END SchemeProfilerSysDepPM3.

