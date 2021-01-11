INTERFACE BnfRule;
IMPORT BnfType AS Bnf;
IMPORT SyntaxType;

TYPE
  T = RECORD
    t    : TEXT;
    b    : Bnf.T;
    type : SyntaxType.T;
  END;

CONST Brand = "TextBnf";

END BnfRule.
