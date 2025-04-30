INTERFACE Mpz;

(*
 * Author : Mika Nystrom <mika.nystroem@intel.com> April 2025
 *)

TYPE
  T <: REFANY;

PROCEDURE New() : T;


CONST Brand = "Mpz";


TYPE FormatBase = { Binary, Octal, Decimal, Hexadecimal };
     
PROCEDURE Format(t : T; base := FormatBase.Decimal) : TEXT;
  
PROCEDURE FormatDecimal(t : T) : TEXT;

PROCEDURE FormatHexadecimal(t : T) : TEXT;

PROCEDURE FormatOctal(t : T) : TEXT;
  
END Mpz.
