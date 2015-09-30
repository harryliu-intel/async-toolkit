INTERFACE Directives;
IMPORT Rd;
IMPORT NodePair, NodePairRefTbl;
IMPORT Name, NameSet;

TYPE 
  Table <: NodePairRefTbl.Default OBJECT METHODS
    allNodes() : NameSet.T;
  END;
  (* Table is a NodePairRefTbl that tracks all the names used in the
     indexing nodepairs *)

PROCEDURE Parse(rd          : Rd.T; 
                globalNames : NameSet.T;
                prefix      : Name.T;
                into        : Table);

TYPE Data = { SlewPoints, MaxDelay, MaxSlew, MinDelay, MinSlew };

TYPE 
  Characterization = ARRAY OF ARRAY Data OF LONGREAL;

  DelayBlock = OBJECT
    nodes : NodePair.T;
    data : REF Characterization;
  END;

TYPE Interpolator <: ROOT;

PROCEDURE MakeInterpolator(READONLY data : Characterization; 
                           minMult : LONGREAL) : Interpolator;
  (* minMult multiplies all the mins, must satisfy 0 < minMult <= 1 *)

PROCEDURE Interpolate(i : Interpolator;
                      at            : LONGREAL) : ARRAY Data OF LONGREAL;

PROCEDURE InterpolateDeriv(i : Interpolator;
                           at            : LONGREAL) : ARRAY Data OF LONGREAL;

END Directives.
