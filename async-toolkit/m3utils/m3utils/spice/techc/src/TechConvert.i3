INTERFACE TechConvert;
IMPORT Pathname;
IMPORT Watchdog;
IMPORT TextWr;
IMPORT TechConfig;

PROCEDURE DoConvert(READONLY c : TechConfig.T;
                    traceRoot : Pathname.T; exitOnError : BOOLEAN) : BOOLEAN;

TYPE
  MyCb <: PublicCb;

  PublicCb = Watchdog.Callback OBJECT
    cmd : TEXT;
    wr  : TextWr.T;
  END;
  
END TechConvert.
