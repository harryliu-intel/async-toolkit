INTERFACE FileNamer;
IMPORT Pathname;

(* compute name of temp files *)

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(wd : Pathname.T; nFiles : CARDINAL; nNames : CARDINAL) : T;
    name(idx : CARDINAL) : Pathname.T;
    getWd() : Pathname.T;
  END;

CONST Brand = "FileNamer";

END FileNamer.
