INTERFACE UnixUtils;
IMPORT OSError;
IMPORT Pathname;

PROCEDURE SymLink(name1, name2: Pathname.T)  RAISES {OSError.E};

END UnixUtils.
