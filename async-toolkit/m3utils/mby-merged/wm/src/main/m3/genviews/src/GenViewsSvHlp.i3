INTERFACE GenViewsSvHlp;
IMPORT GenViews;
IMPORT Rd;
IMPORT Word;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = GenViews.T OBJECT
    fieldAddrRd      : Rd.T;
    addrBits         : AddrBits;
    baseAddressBytes : Word.T;
    packageName      : TEXT;
    outFileName      : Pathname.T;
  END;

  AddrBits = [1..64];

END GenViewsSvHlp.
