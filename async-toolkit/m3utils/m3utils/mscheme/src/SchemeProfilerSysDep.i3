(* $Id: SchemeProfilerSysDep.i3,v 1.3 2009/07/09 17:09:55 mika Exp $ *)

INTERFACE SchemeProfilerSysDep;
FROM SchemeUnixDeps IMPORT struct_rusage;
FROM Ctypes IMPORT int;

PROCEDURE getrusage(who: int; VAR r: struct_rusage): int;

CONST Brand = "SchemeProfilerSysDep";

END SchemeProfilerSysDep.
