INTERFACE IndexBits;
IMPORT Word;

TYPE
  I = [0..63];
  T = SET OF I;

(* an IndexBits.T is a set of bits in a byte address needed to span a
   certain type of collective address 

   for example, a 64-bit register spans eight bytes, therefore the bits needed
   to reach any bit in the register is bits 0..2

   for example, an array with a size of 16 and a stride of 1024 bytes needs
   to be addressed by bits 10..13

   for example, an array with a size of 16 and a stride of 1000 bytes needs
   to be addressed by bits 3..13 (1000 = 5 * 5 * 5 * 8)
   ---we might approximate this with bits 0..13 instead.
*)

CONST
  Zero = T {};

PROCEDURE FromArray(n : CARDINAL; stride : Word.T) : T;

PROCEDURE FromReg(bitSz : CARDINAL) : T;

PROCEDURE Hi(t : T) : [ -1..LAST(I) ];

PROCEDURE Lo(t : T) : [ 0..LAST(I)+1 ];

PROCEDURE FormatMask(bits : [0..64]; t : T) : TEXT;

PROCEDURE FormatBaseQ(bits : [0..64]; t : T; base : Word.T) : TEXT;

CONST Brand = "IndexBits";

END IndexBits.
