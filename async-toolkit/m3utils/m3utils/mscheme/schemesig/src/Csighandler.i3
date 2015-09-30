(* $Id: Csighandler.i3,v 1.1 2008/10/14 07:51:11 mika Exp $ *)

INTERFACE Csighandler;

<*EXTERNAL Csighandler_have_signal*>
PROCEDURE have_signal() : INTEGER;

<*EXTERNAL Csighandler_clear_signal*>
PROCEDURE clear_signal();

<*EXTERNAL Csighandler_install_int_handler*>
PROCEDURE install_int_handler();

END Csighandler.
