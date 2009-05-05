(* $Id$ *)

INTERFACE CTZ;
FROM Ctypes IMPORT const_char_star, char_star;

<*EXTERNAL setenv*>
PROCEDURE setenv(name, value : const_char_star; overwrite : INTEGER) : INTEGER;

<*EXTERNAL getenv*>
PROCEDURE getenv(name : const_char_star) : char_star;

END CTZ.
