INTERFACE Schmoozer;
IMPORT Schmooze;

VAR simP : Param;

TYPE
  TA = ARRAY OF TEXT;
  LA = ARRAY OF LR;
  LR = LONGREAL;
  I01  = ARRAY [0..1] OF INTEGER;
  PA = ARRAY OF RealParam;

  RP01 = ARRAY [0..1] OF RealParam;
  LR01 = ARRAY [0..1] OF LR;

  RP02 = ARRAY [0..2] OF RealParam;
  LR02 = ARRAY [0..2] OF LR;

  RP03 = ARRAY [0..3] OF RealParam;
  LR03 = ARRAY [0..3] OF LR;
  
TYPE Tool = { Script,         (* must be parsed by runspice shell script  *)
              SpiceBuilder,   (* default---goes to spicebuilder           *)
              PostSpice       (* a post-spice step, spice can be memoized *)
  };

TYPE
  Param <: PublicParam;

  PublicParam = OBJECT 
    nm   : TEXT;
    flag : TEXT;
    tool := Tool.SpiceBuilder; (* tool that uses the flag *)
  METHODS
    init() : Param;
  END;

  RealParam = Param OBJECT
    saneMin, saneMax : LONGREAL;
  END;
  
  DiscreteParam = Param OBJECT
    vals : REF TA;
  END;

  IntParam = Param OBJECT
    min, max : INTEGER;
  END;

TYPE
  Settings = BRANDED OBJECT 
  METHODS
    n() : CARDINAL;
  END;

  SingleSettings = Settings OBJECT
    param : Param;
  END;

  Variety <: SingleSettings OBJECT
    cover : REF TA;
  END;

  Sweep <: SingleSettings OBJECT
    min, max, step : LONGREAL;
  END;

  SweepSpecific <: SingleSettings OBJECT
    v : REF ARRAY OF LONGREAL;
  END;

  PublicSchmoo = Settings OBJECT
    param                                : REF ARRAY OF RealParam;
    min, max, minStep, minRatio, maxStep : REF ARRAY OF LONGREAL;
  END;

  Schmoo <: PublicSchmoo;

PROCEDURE RT(READONLY z : TA) : REF TA;

PROCEDURE RL(READONLY z : LA) : REF LA;

PROCEDURE RP(READONLY z : PA) : REF PA;

PROCEDURE Add(s : Settings);

PROCEDURE Process(READONLY a : ARRAY OF Schmooze.T);

PROCEDURE Setup(simP : DiscreteParam);

PROCEDURE RunAll();
  
END Schmoozer.
