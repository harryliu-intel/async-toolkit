INTERFACE GenViewsScheme;
IMPORT GenViewsPass2;
IMPORT GenViews;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = GenViewsPass2.T OBJECT
    scmFiles : REF ARRAY OF Pathname.T;
  END;

END GenViewsScheme.
