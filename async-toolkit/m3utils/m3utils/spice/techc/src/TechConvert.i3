INTERFACE TechConvert;
IMPORT Pathname;
IMPORT Watchdog;
IMPORT TextWr;
IMPORT TechConfig;
IMPORT OSError;

CONST
  CtPath = "/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/ct/AMD64_LINUX/ct";

  NanosimrdPath = "/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/fsdb/src/nanosimrd";

  SpicestreamPath = "/nfs/site/disks/zsc3_fon_fe_0001/mnystroe/m3utils/spice/spicecompress/spicestream/AMD64_LINUX/spicestream";

PROCEDURE DoConvert(READONLY c : TechConfig.T;
                    traceRoot  : Pathname.T; exitOnError : BOOLEAN) : BOOLEAN;

TYPE
  MyCb <: PublicCb;

  PublicCb = Watchdog.Callback OBJECT
    cmd : TEXT;
    wr  : TextWr.T;
  END;

PROCEDURE FindFsdbInDir(dir : Pathname.T) : Pathname.T RAISES { OSError.E };
 
END TechConvert.
