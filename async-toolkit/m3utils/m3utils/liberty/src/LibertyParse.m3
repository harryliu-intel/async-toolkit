MODULE LibertyParse;
IMPORT Rd;
IMPORT LibertyComponent;
IMPORT libertyLexStd;
IMPORT libertyParseStd;

PROCEDURE Parse(rd : Rd.T) : LibertyComponent.T  RAISES { Rd.Failure } =
  VAR
    lexer    := NEW(libertyLexStd.T);
    parser   := NEW(libertyParseStd.T);
  BEGIN
    EVAL lexer.setRd(rd);
    EVAL parser.setLex(lexer);
    
    EVAL parser.parse();
    RETURN parser.val
  END Parse;


BEGIN END LibertyParse.
