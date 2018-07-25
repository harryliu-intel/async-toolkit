UNSAFE INTERFACE UnsafeUpdater;
IMPORT Word;
IMPORT CompPath;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(base : REFANY; fieldAddr : ADDRESS; width : CARDINAL; nm : CompPath.T) : T;
    update(to : Word.T);
  END;

CONST Brand = "UnsafeUpdater";

VAR doDebug : BOOLEAN;

END UnsafeUpdater.
     
  
