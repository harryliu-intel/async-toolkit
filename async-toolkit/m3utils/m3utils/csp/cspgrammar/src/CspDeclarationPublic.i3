INTERFACE CspDeclarationPublic;
IMPORT CspDeclaration;
IMPORT CspDeclaratorSeq;
IMPORT CspStructDeclaratorSeq;
IMPORT Atom;
IMPORT CspType;

REVEAL
  CspDeclaration.Function =  CspDeclaration.T BRANDED CspDeclaration.Brand & " Function" OBJECT
    funcName   : Atom.T;
    formals    : CspDeclaratorSeq.T;
    returnType : CspType.T;
  END;

  CspDeclaration.Structure = CspDeclaration.T BRANDED CspDeclaration.Brand & " Structure" OBJECT
    name  : Atom.T;
    decls : CspStructDeclaratorSeq.T;
  END;

END CspDeclarationPublic.

