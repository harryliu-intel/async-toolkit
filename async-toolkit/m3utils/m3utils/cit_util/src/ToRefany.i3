
INTERFACE ToRefany;

IMPORT Word;
IMPORT ToRefanyClass;

TYPE T = REFANY;

PROCEDURE Hash(a : T) : Word.T;
PROCEDURE Equal(a, b : T) : BOOLEAN;

PROCEDURE AddType(type : ToRefanyClass.T);

END ToRefany.
