INTERFACE TechProcess;
FROM TechConfig IMPORT Tran, Corn, Gate;

TYPE
  TranSufxs     = ARRAY Tran OF TEXT;

TYPE
  T = RECORD
    tranSufxs          : ARRAY Tran OF TEXT;
    tranSize           : TEXT;
    hspiceModel        : TEXT;
    hspiceModelRoot    : TEXT;
    cornNames          : ARRAY Corn OF TEXT;
    cellPaths          : ARRAY Gate OF ARRAY Tran OF TEXT;
    cellNames          : ARRAY Gate OF ARRAY Tran OF TEXT;
    plugText           : TEXT;
  END;

CONST
  Brand = "TechProcess";

  (* a process for which we don't have standard cells can use the below *)
CONST    
  UnknownCellNames = ARRAY Tran OF TEXT { NIL, .. };
  TechUnknownCellNames =
    ARRAY Gate OF ARRAY Tran OF TEXT { UnknownCellNames, .. };
  
  UnknownCellPaths = ARRAY Tran OF TEXT { NIL, .. };
  TechUnknownCellPaths =
    ARRAY Gate OF ARRAY Tran OF TEXT { UnknownCellPaths, .. };

END TechProcess.

