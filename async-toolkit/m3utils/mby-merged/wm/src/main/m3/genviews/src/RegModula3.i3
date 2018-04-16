INTERFACE RegModula3;
IMPORT Wr, Thread, RegAddrmap, OSError, Pathname;

TYPE
  T <: Public;

  Public = OBJECT METHODS
    init(map : RegAddrmap.T) : T;
    write(dirPath : Pathname.T; rw : RW)
      RAISES { OSError.E, Wr.Failure, Thread.Alerted };
  END;

TYPE
  TypeHier = { Read, Addr, Update };

CONST
  MainTypeName = ARRAY TypeHier OF TEXT { "T", "A", "Update" };

  CompTypeSuffix = ARRAY TypeHier OF TEXT { "", "__addr", "__update" };

  TypePhase = ARRAY TypeHier OF Phase { RW.R, RW.W, RW.W };
  (* which output file does it go to? *)

  InitProcName = ARRAY TypeHier OF TEXT { "Init", "Init", "UpdateInit" };

TYPE
  RW = { R, W };

  Phase = RW;  (* controls top-level code generation, one pass per phase *)

CONST
  PhaseNames = ARRAY RW OF TEXT { "Read", "Write" };
  
CONST RWsuffixes = ARRAY RW OF TEXT { "", "_addr" };

CONST Brand = "RegModula3";

END RegModula3.
