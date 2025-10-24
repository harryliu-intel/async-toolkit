INTERFACE Variation;
IMPORT Pathname, Perturbation;
IMPORT Wr, Rd, OSError;
IMPORT TextLongRealTbl AS TextLRTbl;

TYPE
  VarParser = OBJECT METHODS
    line(str : TEXT);
  END;

  T <: Public;
  
  Public = OBJECT METHODS
    init(fromHsp : Pathname.T; varParser : VarParser := NIL) : T
      RAISES { Rd.Failure, OSError.E };

    setGlobalParam(nm : TEXT; to : LONGREAL);

    makeParamDict(fromLib, baseTranTypeName : TEXT) : TextLRTbl.T;
    (* unperturbed model *)
    
    wrModel(wr : Wr.T;
            fromLib, baseTranTypeName, newTranTypeName : TEXT;
            READONLY params : ARRAY OF Param
    ) RAISES { Wr.Failure };
    (* write custom model *)
    
    havePerturbation(baseTranTypeName, named : TEXT;
                     VAR p : Perturbation.T) : BOOLEAN;
    (* do we have given perturbation -- subtypes must override *)
  END;

  Default <: PublicDef;

  PublicDef = T OBJECT METHODS
    setPerturbation(p : Perturbation.Default);
  END;

  Param = RECORD nm : TEXT; v : LONGREAL END;

PROCEDURE SetP(VAR a : ARRAY OF Param;
               READONLY nm : ARRAY OF TEXT;
               READONLY to : ARRAY OF LONGREAL);
  
CONST Brand = "Variation";

END Variation.
