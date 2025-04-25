INTERFACE FileFinder;
IMPORT Pathname;
IMPORT SchemePair;
IMPORT RegEx;

PROCEDURE Find(dirpath : Pathname.T; pattern : TEXT) : SchemePair.T
  RAISES { RegEx.Error };

  

END FileFinder.
