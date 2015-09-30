(* $Id: CITArgs.i3,v 1.1 2009/04/01 07:16:26 mika Exp $ *)

INTERFACE CITArgs;

(* This implements a subset of the m3tk "Args" interface.
   Mika didn't like m3tk, and anyway it appears to use TextF,
   which cm3 does not support. *)


TYPE
  T = ARRAY OF TEXT;


(* Command line arguments *)

PROCEDURE CommandLine(): REF T RAISES {};
(* Returns the arguments on the command line when the current program was
invoked. The program name is not included; element zero of the array is the
first argument (the program name can be obtained from the 'Params'
interface). Each call of this procedure returns a newly allocated array.  The
contents of the array - i.e. the texts - are only allocated once. *)


END CITArgs.
