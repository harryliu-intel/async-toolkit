INTERFACE GenViewsScheme;
IMPORT GenViewsSvHlp;
IMPORT GenViews;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = GenViewsSvHlp.Public OBJECT
    scmFiles : REF ARRAY OF Pathname.T;
  END;

END GenViewsScheme.
