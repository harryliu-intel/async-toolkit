/* $Id: sighandler.c,v 1.1 2008/10/13 12:06:15 mika Exp $ */

#include <signal.h>

static int sig = 0;

static void 
handler(int s) { 
	sig = 1; 
}

int
Csighandler_have_signal(void) { return sig; }

void
Csighandler_clear_signal(void) { sig = 0; }

void
Csighandler_install_int_handler(void) { signal(SIGINT, handler); }
