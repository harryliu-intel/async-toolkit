(* $Id: CTZ.i3,v 1.3 2011/02/10 00:16:47 mika Exp $ *)

INTERFACE CTZ;
FROM Ctypes IMPORT const_char_star, char_star;

<*EXTERNAL setenv*>
PROCEDURE setenv(name, value : const_char_star; overwrite : INTEGER) : INTEGER;

<*EXTERNAL getenv*>
PROCEDURE getenv(name : const_char_star) : char_star;

<*EXTERNAL CTZ__setTZ *>
PROCEDURE setTZ(name : const_char_star);

<*OBSOLETE*>
<*EXTERNAL CTZ__setenv_TZ_America_New_York *>
PROCEDURE setenv_TZ_America_New_York();

END CTZ.
