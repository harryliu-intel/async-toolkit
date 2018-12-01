INTERFACE GenViewsScheme;
IMPORT GenViews;
IMPORT Rd;

TYPE
  T <: Public;

  Public = GenViews.T OBJECT
    fieldAddrRd : Rd.T;
  END;

END GenViewsScheme.
