INTERFACE GenViewsScheme;
IMPORT GenViews;
IMPORT Rd;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = GenViews.T OBJECT
    fieldAddrRd : Rd.T;
    scmFiles : REF ARRAY OF Pathname.T;
  END;

END GenViewsScheme.
