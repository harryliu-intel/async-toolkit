(* $Id: SchemeProfilerSysDepCM3.m3,v 1.1 2009/03/17 17:14:08 mika Exp $ *)
MODULE SchemeProfilerSysDepCM3 EXPORTS SchemeProfilerSysDep;
FROM Uresource IMPORT struct_rusage;
IMPORT Uresource;
FROM Ctypes IMPORT int;
IMPORT Cerrno;

PROCEDURE GetErrno() : INTEGER = BEGIN RETURN Cerrno.GetErrno() END GetErrno;

PROCEDURE getrusage(who: int; VAR r: struct_rusage): int = 
  BEGIN RETURN Uresource.getrusage(who,r) END getrusage;

BEGIN END SchemeProfilerSysDepCM3.

