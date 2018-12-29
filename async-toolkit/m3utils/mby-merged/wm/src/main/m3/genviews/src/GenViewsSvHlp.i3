INTERFACE GenViewsSvHlp;
IMPORT GenViews;
IMPORT Rd;
IMPORT Word;

TYPE
  T <: Public;

  Public = GenViews.T OBJECT
    fieldAddrRd      : Rd.T;
    addrBits         : AddrBits;
    baseAddressBytes : Word.T;
  END;

  AddrBits = [1..64];

END GenViewsSvHlp.
