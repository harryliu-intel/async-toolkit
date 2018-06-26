INTERFACE RegModula3;
IMPORT RegCompiler;
IMPORT Wx, Pathname;

TYPE Public = RegCompiler.T;
     T      <: Public;

TYPE
  TypeHier = { Read, Addr, Unsafe, Update };

CONST
  MainTypeName = ARRAY TypeHier OF TEXT { "T", "A", "X", "Update" };

  CompTypeSuffix = ARRAY TypeHier OF TEXT { "", "__addr", "__unsafe", "__update" };

  TypePhase = ARRAY TypeHier OF Phase { RW.R, RW.W, RW.W, RW.W };
  (* which output file does it go to? *)

  InitProcName = ARRAY TypeHier OF TEXT { "Init", "Init", "InitX", "UpdateInit" };

TYPE
  RW = { R, W };

  Phase = RW;  (* controls top-level code generation, one pass per phase *)

CONST
  PhaseNames = ARRAY RW OF TEXT { "Read", "Write" };
  
CONST RWsuffixes = ARRAY RW OF TEXT { "", "_addr" };

CONST Brand = "RegModula3";

END RegModula3.
