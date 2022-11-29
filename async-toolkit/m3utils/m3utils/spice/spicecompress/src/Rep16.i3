INTERFACE Rep16;
IMPORT Word;

(* a waveform, whose coordinates lie in the range [0,1] (real valued) is 
   represented as a polynomial and encoded as follows

   <header>
   <data word>
   ...
   <data word>

   The header is laid out as follows

   <n words> (32) // data array is 2 x this in bytes
   <n points> (32)
   <min point> (IEEE fp32)
   <max point> (IEEE fp32)

   The <data> is in two types

   First word

   < count (13) | order (3) >

   which defines a polynomial of order 0..7

   following which are data words

   <coefficient (16)>

   coefficients have weight 
  
   p0    p1    p2    p3    p4    p5    p6    p7
   2^0, 2^1,  2^0, 2^-1, 2^-2, 2^-3, 2^-4, 2^-5

   Each polynomial segment starts at 0.

   If the polynomial is of order 0, the only data word is the p0 coefficient,
   which is unsigned.

   If the polynomial is of order 1 or higher, the order 0 word is left out.
   In these cases, the starting point is taken by extrapolating the previous
   polynomial.  Order 1 and higher coefficients are signed, 2's complement.

   Interpretation of the data is that the value 2^16 - 1 is exactly max, and
   0 is exactly min.  This is necessary because max and min do appear in the
   data stream and max would not be representable if we went with 2^16 as the
   representation of max, since 2^16 cannot be represented in 16 bits.
   Note that under this interpretation, the ULP of 2^16-1 has half extent, as
   does 0, because no real number rounds down to 2^16-1 nor up to 0.

   Evaluation of the polynomial is done in integer form, and it saturates
   at 2^16 - 1.

   An error specification, external to this form, may be used to allow
   for certain quantization error.

   A use-case recommendation is to leave as many trailing zeros as
   possible, in order to allow the data to be compressed,
   subsequently, using Huffman or arithmetic coding.

*)

(* note that where this interface uses LONGREAL, the external storage is
   in REAL (IEEE fp32) *)

(* external storage is always LITTLE ENDIAN, that is, 16-bit quantities
   are represented in external storage with the less significant byte 
   first
*)

CONST
  Bits = 16;
  CountBits = 13;
  OrderBits = Bits - CountBits;

  MaxPower = Word.Shift(1, OrderBits) - 1;
      
TYPE
  Header = RECORD
    nwords   : CARDINAL;
    npoints  : CARDINAL; (* hmm, maybe we should have this.. *)
    min, max : LONGREAL;
  END;

  Base = [ 0 .. Word.Shift(1, Bits) - 1 ];

  Unsigned = Base;

  
  Signed = [ -Word.Shift(1, Bits-1) + 1  .. Word.Shift(1, Bits-1) - 1 ];

  Array = ARRAY OF Base;

  Count = [ 0 .. Word.Shift(1, 13) - 1 ];

  Order = [ 0 .. Word.Shift(1, 3) - 1 ];

  T  = RECORD
    count : [ 1 .. LAST(Count) ] ;
    order : Order;
    c0    : Base;
    c     : ARRAY [ 1 .. MaxPower ] OF Signed;
  END;

PROCEDURE ExpandFixed(READONLY a : Array; VAR b : Array);
  (* do we need this routine??? *)

PROCEDURE Expand(READONLY t : T; VAR b : Array);
  (* if t.order = 0 then we expand per t.c0, else we use b[0] to expand;
     either way NUMBER(b) must be equal to t.count 
     
     do we need this routine at all???
  *)

PROCEDURE EvalPoly(READONLY t : T; x0 : CARDINAL) : LONGREAL;

PROCEDURE ToFloat(x : Signed; pow : [1..LAST(Order)]) : LONGREAL;

PROCEDURE FromFloat(x : LONGREAL; pow : [1..LAST(Order)]) : Signed;

PROCEDURE ToFloat0(x : Base) : LONGREAL;

PROCEDURE FromFloat0(x : LONGREAL) : Unsigned;

PROCEDURE FromSingle(x : LONGREAL) : T;

CONST Zero = ARRAY [ 1 .. MaxPower ] OF Signed { 0 , .. };

PROCEDURE Format(READONLY a : T) : TEXT;
      
END Rep16.
