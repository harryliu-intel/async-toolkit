INTERFACE FileFinder;
IMPORT Pathname;
IMPORT SchemePair;
IMPORT RegEx;
IMPORT OSError;

PROCEDURE Find(dirpath : Pathname.T; pattern : TEXT) : SchemePair.T
  RAISES { RegEx.Error, OSError.E };

  

END FileFinder.
