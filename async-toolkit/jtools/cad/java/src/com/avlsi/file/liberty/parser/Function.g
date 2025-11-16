header {
    package com.avlsi.file.liberty.parser;
}

class FunctionLexer extends Lexer;
options {
    charVocabulary = '\0'..'\377';
    k = 1;
    caseSensitive = true;
}

TOKEN
    : ( ( WS )? BINARY_OPS ( WS )? ) => ( ( WS )? )!
                                        op:BINARY_OPS { $setType(op.getType()); }
                                        ( ( WS )? )!
    | ( ( WS )? ( QUOTE | RPAREN ) ) => ( ( WS )? )!
                                        ( QUOTE { $setType(QUOTE); }
                                        | RPAREN { $setType(RPAREN); } )
    | ( ( BANG | LPAREN ) ( WS )? ) => ( BANG { $setType(BANG); }
                                       | LPAREN { $setType(LPAREN); })
                                       ( ( WS )? )!
    | ( WS ) => WS { $setType(WS); }
    | "\\\"" NODE "\\\"" { $setType(NODE); }
    | NODE { $setType(NODE); }
    ;

protected
NODE: ( '0'..'9' | 'A'..'Z' | '[' | ']' | '_' | 'a'..'z' )+;

protected
WS: ( ' ' | '\t' )+;

protected
BINARY_OPS
    : '^' { $setType(HAT); }
    | '*' { $setType(STAR); }
    | '&' { $setType(AMP); }
    | '+' { $setType(PLUS); }
    | '|' { $setType(BAR); }
    ;

protected
QUOTE: "'";

protected
BANG: '!';

protected
LPAREN: '(';

protected
RPAREN: ')';

{
    import java.io.BufferedReader;
    import java.io.InputStreamReader;
    import java.io.StringReader;

    import antlr.RecognitionException;
    import antlr.TokenStreamException;

    import com.avlsi.util.bool.BooleanExpressionInterface;
    import com.avlsi.util.bool.BooleanUtils;
}

class FunctionParser extends Parser;
options {
    k = 1;
    buildAST = false;
}

{
    final BooleanUtils boolUtils = new BooleanUtils();

    public static BooleanExpressionInterface parse(String s)
    throws RecognitionException, TokenStreamException {
        final StringReader reader = new StringReader(s.trim());
        FunctionLexer lexer = new FunctionLexer(reader);
        FunctionParser parser = new FunctionParser(lexer);
        return parser.goal();
    }

    public static void main(String[] args) throws Exception {
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        br.lines()
          .forEach(l -> {
              try {
                  System.out.println(l + " -> " + parse(l).toUserVisibleString());
              } catch (Exception e) {
                  System.err.println("Failed: " + l);
              }
          });
    }
}

primary returns [BooleanExpressionInterface expr = null]
    : n:NODE { String text = n.getText();
               if (text.equals("0")) expr = boolUtils.f();
               else if (text.equals("1")) expr = boolUtils.t();
               else expr = boolUtils.literal(text);
             }
    | LPAREN expr = expr RPAREN
    ;
notExpr returns [BooleanExpressionInterface expr = null]
    : BANG expr = notExpr { expr = boolUtils.not(expr); }
    | ( primary QUOTE ) => expr = primary QUOTE { expr = boolUtils.not(expr); }
    | expr = primary
    ;
xorExpr returns [BooleanExpressionInterface expr = null]
    { BooleanExpressionInterface e; }
    : expr = notExpr
      ( HAT e = notExpr { expr = boolUtils.xor(expr, e); } )*
    ;
andExpr returns [BooleanExpressionInterface expr = null]
    { BooleanExpressionInterface e; }
    : expr = xorExpr
      ( ( WS | STAR | AMP ) e = xorExpr { expr = boolUtils.and(expr, e); } )*
    ;
orExpr returns [BooleanExpressionInterface expr = null]
    { BooleanExpressionInterface e; }
    : expr = andExpr
      ( ( PLUS | BAR ) e = andExpr { expr = boolUtils.or(expr, e); } )*
    ;
expr returns [BooleanExpressionInterface expr = null]
    : expr = orExpr;

goal returns [BooleanExpressionInterface expr = null]
    : expr = expr EOF
    ;
