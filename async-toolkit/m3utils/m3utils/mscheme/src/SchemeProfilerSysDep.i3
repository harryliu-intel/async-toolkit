(* $Id$ *)

INTERFACE SchemeProfilerSysDep;
FROM Uresource IMPORT struct_rusage;
FROM Ctypes IMPORT int;

PROCEDURE GetErrno() : INTEGER;

PROCEDURE getrusage(who: int; VAR r: struct_rusage): int;

CONST Brand = "SchemeProfilerSysDep";

END SchemeProfilerSysDep.
