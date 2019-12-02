INTERFACE CompMemoryListener;
IMPORT CsrOp;
IMPORT Word;

TYPE
  T =  OBJECT METHODS
    callback(writeOp : CsrOp.T);
    hash() : Word.T;
    equal(t : T) : BOOLEAN;
  END;

CONST Brand = "CompMemoryListener";

PROCEDURE Hash(a : T) : Word.T;      (* calls a.hash() *)
PROCEDURE Equal(a, b : T) : BOOLEAN; (* calls a.equal(b) *)
  
END CompMemoryListener.
