UNSAFE INTERFACE ContextC;
IMPORT Ctypes;
FROM Coroutine IMPORT Arg;

TYPE T = ADDRESS;

<*EXTERNAL ContextC__New*>
PROCEDURE New() : T;

<*EXTERNAL ContextC__MakeContext*>
PROCEDURE MakeContext(p      : PROCEDURE(arg : Arg);
                      stack  : ADDRESS;
                      ssize  : Ctypes.int;
                      resume : T;
                      arg    : Arg): T;

<*EXTERNAL ContextC__Current*>
PROCEDURE Current() : T;

<*EXTERNAL ContextC__SwapContext*>
PROCEDURE SwapContext(from, to : T);  

<*EXTERNAL ContextC__DisposeContext*>
PROCEDURE DisposeContext(ctx : T);
  
END ContextC.
