UNSAFE INTERFACE UnsafeUpdater;
IMPORT CompPath;
IMPORT Updater;

TYPE
  T <: Public;

  Public = Updater.T OBJECT METHODS
    init(base : REFANY; fieldAddr : ADDRESS; width : CARDINAL; nm : CompPath.T) : T;
    getWidth() : CARDINAL;
    getNm() : CompPath.T;
  END;

CONST Brand = "UnsafeUpdater";

VAR doDebug : BOOLEAN;

END UnsafeUpdater.
     
  
