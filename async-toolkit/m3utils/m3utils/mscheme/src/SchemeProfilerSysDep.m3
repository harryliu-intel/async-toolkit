(* $Id: SchemeProfilerSysDep.m3,v 1.3 2009/07/09 17:09:55 mika Exp $ *)
MODULE SchemeProfilerSysDep;
IMPORT SchemeUnixDeps;
FROM Ctypes IMPORT int;

PROCEDURE getrusage(who: int; VAR r: SchemeUnixDeps.struct_rusage): int = 
  BEGIN RETURN SchemeUnixDeps.getrusage(who,r) END getrusage;

BEGIN END SchemeProfilerSysDep.

