INTERFACE GenViewsSvHlp;
IMPORT GenViews;
IMPORT Rd;

TYPE
  T <: Public;

  Public = GenViews.T OBJECT
    fieldAddrRd : Rd.T;
  END;

END GenViewsSvHlp.
