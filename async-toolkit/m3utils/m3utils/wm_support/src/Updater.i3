INTERFACE Updater;
IMPORT Word;

TYPE
  T = OBJECT
    doSync := FALSE; (* do we call sync() on every CSR write from SBIOSF *)
  METHODS
    update(to : Word.T);
    value() : Word.T;
    sync(); (* call to synchronize alternate representations *)
  END;

CONST Brand = "Updater";

END Updater.
