MODULE CspExpression;
IMPORT CspExpressionPublic AS P;
FROM SchemeUtils IMPORT List2, List3, List4, List5, Cons;
IMPORT SchemeSymbol;
IMPORT SchemeObject;
IMPORT SchemeUtils;
IMPORT CspSyntax;
FROM CspSyntax IMPORT Lisp;
IMPORT SchemeBoolean;
IMPORT SchemeString;
IMPORT SchemePair;
IMPORT CspRange;

CONST Sym = SchemeSymbol.FromText;
      
REVEAL
  Boolean = PublicBoolean BRANDED Brand & " Boolean" OBJECT
  OVERRIDES
    lisp := BooleanLisp;
  END;

  Else = T BRANDED Brand & " Else" OBJECT
  OVERRIDES
    lisp := ElseLisp;
  END;
  
  Integer = PublicInteger BRANDED Brand & " Integer" OBJECT
  OVERRIDES
    lisp := IntegerLisp;
  END;
  
  String = PublicString BRANDED Brand & " String" OBJECT
  OVERRIDES
    lisp := StringLisp;
  END;
  
  (**********************************************************************)

  Identifier = PublicIdentifier BRANDED Brand & " Identifier" OBJECT
  OVERRIDES
    lisp := IdentifierLisp;
  END;

  (**********************************************************************)
  
  Binary = PublicBinary BRANDED Brand & " Binary" OBJECT
  OVERRIDES
    lisp := BinaryLisp;
  END;
  
  Unary = PublicUnary BRANDED Brand & " Unary" OBJECT
  OVERRIDES
    lisp := UnaryLisp;
  END;

  ArrayAccess = PublicArrayAccess BRANDED Brand & " ArrayAccess" OBJECT
  OVERRIDES
    lisp := ArrayAccessLisp;
  END;
  
  MemberAccess = PublicAccess BRANDED Brand & " MemberAccess" OBJECT
  OVERRIDES
    lisp := MemberAccessLisp;
  END;
  
  StructureAccess = PublicAccess BRANDED Brand & " StructureAccess" OBJECT
  OVERRIDES
    lisp := StructureAccessLisp;
  END;

  BitRange = PublicBitRange BRANDED Brand & " BitRange" OBJECT
  OVERRIDES
    lisp := BitRangeLisp;
  END;
  
  Probe = ChanExpr BRANDED Brand & " Probe" OBJECT
  OVERRIDES
    lisp := ProbeLisp;
  END;
  
  Receive = ChanExpr BRANDED Brand & " Receive" OBJECT
  OVERRIDES
    lisp := ReceiveLisp;
  END;

  Peek = ChanExpr BRANDED Brand & " Peek" OBJECT
  OVERRIDES
    lisp := PeekLisp;
  END;

  FunctionCall = P.PublicFunctionCall BRANDED Brand & " FunctionCall" OBJECT
  OVERRIDES
    lisp := FunctionCallLisp;
  END;

  Loop = P.PublicLoop BRANDED Brand & " Loop" OBJECT
  OVERRIDES
    lisp := LoopLisp;
  END;

PROCEDURE BooleanLisp(self : Boolean) : SchemeObject.T =
  BEGIN
    RETURN SchemeBoolean.Truth(self.val)
  END BooleanLisp;

PROCEDURE ElseLisp(<*UNUSED*>self : Else) : SchemeObject.T =
  BEGIN
    RETURN Sym("else")
  END ElseLisp;
  
PROCEDURE IntegerLisp(self : Integer) : SchemeObject.T =
  BEGIN
    RETURN self.val
  END IntegerLisp;
  
PROCEDURE StringLisp(self : String) : SchemeObject.T =
  BEGIN
    RETURN SchemeString.FromText(self.val)
  END StringLisp;
  
PROCEDURE IdentifierLisp(self : Identifier) : SchemeObject.T =
  BEGIN
    RETURN SchemeUtils.List2(Sym("id"), self.id)
  END IdentifierLisp;

PROCEDURE BinaryLisp(self : Binary) : SchemeObject.T =
  BEGIN
    RETURN SchemeUtils.List3(Sym(BinMap[self.op]),
                             Lisp(self.l),
                             Lisp(self.r))
  END BinaryLisp;
  
PROCEDURE UnaryLisp(self : Unary) : SchemeObject.T =
  BEGIN
    RETURN SchemeUtils.List2(Sym(UnaMap[self.op]),
                             Lisp(self.x))
  END UnaryLisp;
  
PROCEDURE ArrayAccessLisp(self : ArrayAccess) : SchemeObject.T =
  BEGIN
    RETURN List3(Sym("array-access"), Lisp(self.arr), Lisp(self.idx))
    END ArrayAccessLisp;
  
PROCEDURE MemberAccessLisp(self : MemberAccess) : SchemeObject.T =
  BEGIN
    RETURN List3(Sym("member-access"), Lisp(self.struct), self.member)
  END MemberAccessLisp;
  
PROCEDURE StructureAccessLisp(self : StructureAccess) : SchemeObject.T =
  BEGIN
    RETURN List3(Sym("struct-access"), Lisp(self.struct), self.member)
  END StructureAccessLisp;
  
PROCEDURE BitRangeLisp(self : BitRange) : SchemeObject.T =
  BEGIN
    RETURN List4(Sym("bits"), Lisp(self.bits), Lisp(self.minx), Lisp(self.maxx))
  END BitRangeLisp;
  
PROCEDURE ProbeLisp(self : Probe) : SchemeObject.T =
  BEGIN
    RETURN List2(Sym("probe"), Lisp(self.chan))
  END ProbeLisp;
  
PROCEDURE ReceiveLisp(self : Receive) : SchemeObject.T =
  BEGIN
    RETURN List2(Sym("recv-expression"), Lisp(self.chan))
  END ReceiveLisp;
  
PROCEDURE PeekLisp(self : Peek) : SchemeObject.T =
  BEGIN
    RETURN List2(Sym("peek"), Lisp(self.chan))
  END PeekLisp;
  
PROCEDURE FunctionCallLisp(self : FunctionCall) : SchemeObject.T =
  VAR
    p : SchemePair.T := NIL;
  BEGIN
    FOR i := self.args.size() - 1 TO 0 BY -1 DO
      p := Cons(Lisp(self.args.get(i)), p)
    END;
    p := Cons(Lisp(self.f), p);
    p := Cons(Sym("apply"), p);
    RETURN p
  END FunctionCallLisp;

PROCEDURE LoopLisp(self : Loop) : SchemeObject.T =
  BEGIN
    RETURN List5(Sym("loop-expression"),
                 self.dummy,
                 CspRange.Lisp(self.range),
                 Sym(BinMap[self.op]),
                 self.x.lisp())
  END LoopLisp;

BEGIN END CspExpression.
