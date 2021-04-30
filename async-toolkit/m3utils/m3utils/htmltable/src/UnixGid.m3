MODULE UnixGid;
IMPORT Word;

PROCEDURE Hash(t : T) : Word.T = BEGIN RETURN t END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

BEGIN END UnixGid.
