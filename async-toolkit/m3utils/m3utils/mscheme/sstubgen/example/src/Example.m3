MODULE Example;
IMPORT IO;

TYPE
  UU = T OBJECT 
  OVERRIDES
  END;

PROCEDURE TTT(x : TT) =
  BEGIN
    T.hello(x)
  END TTT;

PROCEDURE TTHello(tt : TT) = BEGIN END TTHello;

PROCEDURE THello(t : T) = 
  BEGIN 
    IO.Put("hi there!\n")
  END THello;


BEGIN 
  EVAL NEW(UU, hello := THello)
END Example.
