INTERFACE Updater;
IMPORT Word;

TYPE
  T = OBJECT METHODS
    update(to : Word.T);
    value() : Word.T;
  END;

CONST Brand = "Updater";

END Updater.
