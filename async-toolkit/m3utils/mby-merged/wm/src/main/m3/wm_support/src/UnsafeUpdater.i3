UNSAFE INTERFACE UnsafeUpdater;
IMPORT Word;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(base : REFANY; fieldAddr : ADDRESS; width : CARDINAL) : T;
    update(to : Word.T);
  END;

CONST Brand = "UnsafeUpdater";

VAR doDebug : BOOLEAN;

END UnsafeUpdater.
     
  
