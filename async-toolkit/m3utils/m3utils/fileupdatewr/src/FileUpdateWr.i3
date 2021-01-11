INTERFACE FileUpdateWr;
IMPORT Wr;
IMPORT OSError;
IMPORT Pathname;

TYPE T <: Wr.T;
     
PROCEDURE Open(p : Pathname.T; suffix := ".temp") : T RAISES { OSError.E };

CONST Brand = "FileUpdateWr";

END FileUpdateWr.

  
  
