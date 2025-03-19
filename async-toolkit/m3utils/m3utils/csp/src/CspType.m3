MODULE CspType;
IMPORT CspSyntax;
IMPORT SchemeSymbol;
IMPORT SchemeObject;

FROM SchemeUtils IMPORT List2, List3, List5;

IMPORT SchemeBoolean;
IMPORT CspRange;
IMPORT CspDirection;
IMPORT BigInt;

CONST Sym = SchemeSymbol.FromText;


REVEAL
  T = Public BRANDED Brand OBJECT
  END;

  Array = PubArray BRANDED Brand & " Array" OBJECT
  OVERRIDES
    lisp := ArrayLisp;
  END;
  
  Channel = PubChannel BRANDED Brand & " Channel" OBJECT
  OVERRIDES
    lisp := ChannelLisp;
  END;
  
  Integer = PubInteger BRANDED Brand & " Integer" OBJECT
  OVERRIDES
    lisp := IntegerLisp;
  END;
  
  Node = PubNode BRANDED Brand & " Node" OBJECT
  OVERRIDES
    lisp := NodeLisp;
  END;
  
  Structure = PubStructure BRANDED Brand & " Structure" OBJECT
  OVERRIDES
    lisp := StructureLisp;
  END;
  
  Boolean = T BRANDED Brand & " Boolean" OBJECT
  OVERRIDES
    lisp := BooleanLisp;
  END;

  String = T BRANDED Brand & " String" OBJECT
  OVERRIDES
    lisp := StringLisp;
  END;

PROCEDURE ArrayLisp(self : Array) : SchemeObject.T =
  BEGIN
    RETURN List3(Sym("array"),
                 CspRange.Lisp(self.range),
                 self.elemntType.lisp())
  END ArrayLisp;
  
PROCEDURE ChannelLisp(self : Channel) : SchemeObject.T =
  BEGIN
    RETURN List3(Sym("channeltype"),
                 self.numValues,
                 CspDirection.Names[self.dir]);
  END ChannelLisp;
  
PROCEDURE IntegerLisp(self : Integer) : SchemeObject.T =
  VAR
    lispDw : BigInt.T;
    lispInterval : SchemeObject.T;
  BEGIN
    IF self.hasDw THEN
      lispDw := BigInt.New(self.dw)
    ELSE
      lispDw := NIL
    END;
    IF self.hasInterval THEN
      lispInterval := List2(self.interval.left, self.interval.right)
    ELSE
      lispInterval := NIL
    END;
    RETURN List5(Sym("integer"),
                 SchemeBoolean.Truth(self.isConst),
                 SchemeBoolean.Truth(self.isSigned),
                 lispDw,
                 lispInterval)
  END IntegerLisp;
  
PROCEDURE NodeLisp(self : Node) : SchemeObject.T =
  BEGIN
    IF self.arrayed THEN
      RETURN List3(Sym("node-array"),
                   Sym(CspDirection.Names[self.direction]),
                   BigInt.New(self.width))
    ELSE
      RETURN List2(Sym("node"),
                   Sym(CspDirection.Names[self.direction]))
    END
  END NodeLisp;
  
PROCEDURE StructureLisp(self : Structure) : SchemeObject.T =
  BEGIN
    RETURN List3(Sym("structure"),
                 SchemeBoolean.Truth(self.isConst),
                 Sym(self.name))
  END StructureLisp;
  
PROCEDURE BooleanLisp(self : Boolean) : SchemeObject.T =
  BEGIN
    RETURN Sym("boolean")
  END BooleanLisp;
  
PROCEDURE StringLisp(self : String) : SchemeObject.T =
  BEGIN
    RETURN Sym("string")
  END StringLisp;

BEGIN END CspType.
