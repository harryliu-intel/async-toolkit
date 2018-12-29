INTERFACE GenViewsSvHlp;
IMPORT GenViews;
IMPORT Rd;

TYPE
  T <: Public;

  Public = GenViews.T OBJECT
    fieldAddrRd : Rd.T;
    addrBits    : AddrBits;
  END;

  AddrBits = [1..64];

END GenViewsSvHlp.
