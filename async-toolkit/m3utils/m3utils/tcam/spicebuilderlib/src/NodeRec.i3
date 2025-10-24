INTERFACE NodeRec;
IMPORT Nodes, Dims;

TYPE
  T = OBJECT
    nm  : TEXT;           (* full name, including indexing *)
    sNm : TEXT;           (* name w/o DUT prefix *)
    nds : Nodes.T;
    idx : REF Dims.T;
  END;

CONST Brand = "NodeRec";

END NodeRec.
