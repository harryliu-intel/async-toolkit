INTERFACE UnixGid;
IMPORT Utypes;
IMPORT Word;

TYPE T = Utypes.gid_t;

PROCEDURE Hash(t : T) : Word.T;

PROCEDURE Equal(a, b : T) : BOOLEAN;

CONST Brand = "UnixGid";

END UnixGid.
