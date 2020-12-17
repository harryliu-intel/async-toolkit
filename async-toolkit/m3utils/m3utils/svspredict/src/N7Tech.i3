INTERFACE N7Tech;

TYPE
  Transistor = { Ulvt, Lvt, Svt };

CONST  LeakageRatio = ARRAY Transistor OF LONGREAL { 24.9d0, 5.7d0, 1.0d0 };

END N7Tech.
