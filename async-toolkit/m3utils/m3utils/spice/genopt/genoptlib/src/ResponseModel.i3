INTERFACE ResponseModel;

IMPORT SchemeSymbol;
IMPORT SurfaceRep;
IMPORT LRMatrix2 AS M;
IMPORT LRVector;

TYPE
  Type = { Constant, Linear, Quadratic };

  T = RECORD
    symbol : SchemeSymbol.T;
    type   : Type;
  END;

CONST TypeNames = ARRAY Type OF TEXT { "Constant", "Linear", "Quadratic" };

CONST Brand = "ResponseModel";

TYPE DofFunc = PROCEDURE (n : CARDINAL) : CARDINAL;

CONST Dof = ARRAY Type OF DofFunc { SurfaceRep.Cdofs,
                                    SurfaceRep.Ldofs,
                                    SurfaceRep.Qdofs };

TYPE M2QFunc = PROCEDURE(n : CARDINAL; READONLY b : M.M) : REF M.M;
     
CONST M2Q = ARRAY Type OF M2QFunc { SurfaceRep.C2Q,
                                    SurfaceRep.L2Q,
                                    SurfaceRep.Q2Q };

TYPE IndepsFunc = PROCEDURE(p             : LRVector.T;
                            row           : CARDINAL;
                            VAR(*OUT*) x  : M.M);

CONST Indeps = ARRAY Type OF IndepsFunc { SurfaceRep.ComputeIndepsC, 
                                          SurfaceRep.ComputeIndepsL,
                                          SurfaceRep.ComputeIndepsQ };

      
END ResponseModel.
    
