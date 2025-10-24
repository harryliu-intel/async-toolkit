INTERFACE CspNode;
IMPORT CspPortObject;

TYPE
  T = CspPortObject.T BRANDED Brand OBJECT
    (* wait queue goes here *)
  END;

CONST Brand = "CspNode";

END CspNode.
