INTERFACE FileNamer;
IMPORT Pathname;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(wd : Pathname.T; nFiles : CARDINAL; nNames : CARDINAL) : T;
    name(idx : CARDINAL) : Pathname.T;
  END;

CONST Brand = "FileNamer";

END FileNamer.
