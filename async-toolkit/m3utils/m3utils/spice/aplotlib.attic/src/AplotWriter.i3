INTERFACE AplotWriter;

TYPE
  T <: Public;

  Public = OBJECT METHODS
  END;
    
  ByStep = T OBJECT METHODS
    writeStep(idx : CARDINAL; READONLY data : ARRAY OF REAL);

    writeStepLR(idx : CARDINAL; READONLY data : ARRAY OF LONGREAL);
  END;

  ByNode = T OBJECT METHODS
    nodeWriter(nodeId : CARDINAL) : NodeWriter;
  END;

  NodeWriter = OBJECT METHODS
    writeArray(READONLY data : ARRAY OF REAL);

    writeArrayLR(READONLY data : ARRAY OF LONGREAL);

    newData(time : LONGREAL; val : LONGREAL);
  END;

CONST Brand = "AplotWriter";

END AplotWriter.
