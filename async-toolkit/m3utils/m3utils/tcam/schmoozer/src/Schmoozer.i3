INTERFACE Schmoozer;
IMPORT Schmooze;
IMPORT ParseParams, OSError;

VAR simP : Param;

TYPE
  TA = ARRAY OF TEXT;
  LA = ARRAY OF LR;
  IA = ARRAY OF INTEGER;
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
    base     := 10;
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

  SpecificInt <: SingleSettings OBJECT
    v : REF ARRAY OF INTEGER;
  END;

  PublicSchmoo = Settings OBJECT
    param                                : REF ARRAY OF RealParam;
    min, max, minStep, minRatio, maxStep : REF ARRAY OF LONGREAL;
  END;

  Schmoo <: PublicSchmoo;

  PublicVarOpt = Settings OBJECT
    radius : REF ARRAY OF LONGREAL;
    stopParam : RealParam := NIL;
    stopRatio : LONGREAL;
    optMult   : LONGREAL := LAST(LONGREAL); (* s.b. +1 (minimize) or -1 (maximize) *)
    varName : TEXT := NIL; (* must set this, based on which variation set 
                              is chosen.  The transistor parameters must be
                              provided in the Simulation M3 file (at least for
                              now) *)
    dims : DimGetter;
  END;

  VarOpt <: PublicVarOpt;

  DimGetter = OBJECT METHODS
    dims(varName, modelName : TEXT) : REF ARRAY OF TEXT;
  END;

PROCEDURE RT(READONLY z : TA) : REF TA;

PROCEDURE RL(READONLY z : LA) : REF LA;

PROCEDURE RP(READONLY z : PA) : REF PA;

PROCEDURE RI(READONLY z : IA) : REF IA;

PROCEDURE Add(s : Settings);

PROCEDURE Process(pp : ParseParams.T;
                  READONLY a : ARRAY OF Schmooze.T) RAISES { ParseParams.Error };

PROCEDURE Setup(pp : ParseParams.T;
                simP : DiscreteParam) RAISES { ParseParams.Error };

PROCEDURE RunAll(pp : ParseParams.T) RAISES { ParseParams.Error, OSError.E };

PROCEDURE SetExtraCmdHack(to : TEXT);

END Schmoozer.
