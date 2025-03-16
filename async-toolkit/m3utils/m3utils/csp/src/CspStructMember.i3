INTERFACE CspStructMember;
IMPORT CspType;

TYPE
  T = RECORD
    name : TEXT; (* should this be Atom.T ? *)
    type : CspType.T;
  END;

CONST Brand = "CspStructMember";

END CspStructMember.
    
