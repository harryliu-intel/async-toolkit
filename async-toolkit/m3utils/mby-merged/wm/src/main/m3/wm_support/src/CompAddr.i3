INTERFACE CompAddr;
IMPORT Word;
CONST Brand = "CompAddr";

TYPE
  T = RECORD
    word : Word.T;
    bit  : [0..BITSIZE(Word.T)-1];
  END;
  
  Addressing = { Regalign, Compact, Fullalign };

CONST
  Unspecified = LAST(CARDINAL);

  DefaultAddressing = Addressing.Regalign;

  AddressingNames = ARRAY Addressing OF TEXT { "Regalign",
                                               "Compact",
                                               "Fullalign" };

  Base = BITSIZE(Word.T);
  
VAR initCount := 0; (* count how many address fields have been initialized *)

PROCEDURE Plus(a, b : T) : T;

PROCEDURE Minus(minuend, subtrahend : T) : T;

PROCEDURE PlusBytes(a : T; bytes : CARDINAL) : T;

PROCEDURE PlusBits(a : T; bits : CARDINAL) : T;

PROCEDURE ModAlign(at : T; byteMod : CARDINAL) : T;
  (* align to a given multiple in bytes *)

PROCEDURE FromBytes(bytes : CARDINAL) : T;

PROCEDURE FromBits(bits : CARDINAL) : T;

PROCEDURE DeltaBytes(a, b : T; truncOK := FALSE) : CARDINAL;
  (* if truncOK then OK to throw away bits offset *)

PROCEDURE NextPower(q : CARDINAL) : CARDINAL;
  (* round up to next power of 2 *)
  
CONST Zero = T { 0, 0 };

PROCEDURE Format(t : T; bytes : BOOLEAN) : TEXT;

PROCEDURE Max(a, b : T) : T;

PROCEDURE Min(a, b : T) : T;

PROCEDURE Compare(a, b : T) : [-1..1];

PROCEDURE Find(READONLY a : ARRAY OF T; addr : T) : [-1..LAST(CARDINAL)];
  (* given an address addr and an array of addresses a sorted in increasing
     order, return the index of the array that is the highest entry that
     is <= the probe.

     If no entry is <= the probe, return -1
  *)

PROCEDURE FindIndirect(READONLY a : ARRAY OF T;
                       READONLY c : ARRAY OF CARDINAL;
                       addr : T) : [-1..LAST(CARDINAL)];
  (* given an address addr and an array of addresses a sorted in increasing
     order[c], return the index of the array c that is the highest entry that
     is <= the probe.

     If no entry is <= the probe, return -1
  *)
  
END CompAddr.
