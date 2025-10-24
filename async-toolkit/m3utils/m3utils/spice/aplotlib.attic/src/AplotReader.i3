INTERFACE AplotReader;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    readStep(stepIdx : CARDINAL) : ByStep;

    readNode(nodeIdx : CARDINAL) : ByNode;
  END;

  ByStep = OBJECT METHODS
    readStep(VAR data : ARRAY OF REAL);

    readStepLR(VAR data : ARRAY OF LONGREAL);
  END;

  ByNode = OBJECT METHODS
    readNode(VAR data : ARRAY OF REAL);

    readNodeLR(VAR data : ARRAY OF LONGREAL);

    setNextTime(to : LONGREAL);  (* error conditions? *)
    
    next(VAR time, val : LONGREAL) : BOOLEAN;
    (* sequential read*)
  END;

CONST Brand = "AplotReader";

END AplotReader.
  
