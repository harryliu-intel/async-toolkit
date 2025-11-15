header {
    package com.avlsi.fast.shapes.stringexpression.impl.parser;
}

{
    
    import com.avlsi.util.debug.Debug;

    import com.avlsi.fast.shapes.stringexpression.StringExpression;
    import com.avlsi.fast.shapes.stringexpression.StringExpressionFactory;
    import com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollection;
}


class StringExpressionParser extends Parser ;
options {
  k = 2;
  buildAST = false;
}

{
    protected StringExpressionFactory m_Factory;
    //protected WriteableVariableDictionary m_VarDict;
    public StringExpressionParser( TokenStream lexer,
        StringExpressionFactory stringExpressionFactory
        /*WriteableVariableDictionary varDict */ ) {
        
        this( lexer );

        m_Factory = stringExpressionFactory;
    }
}

goal returns [ StringExpression exp ]
    {
        exp = null;
    }
    :
        EOF
    |   exp=stringExpression 
    ;

stringExpression returns [ StringExpression exp ]
    {
        StringExpression seed_term;
        StringExpression curr_term;
        WriteableStringExpressionCollection terms = null;
        exp = null;
    }
    :
        seed_term=stringExpressionAtom
        ( 
            PLUS curr_term=stringExpressionAtom {
                if ( terms == null ) {
                    terms = m_Factory.makeExpressionCollection();
                    terms.addExpression( seed_term );
                }
                terms.addExpression( curr_term );
            }
        )* 
        
        {    
            if ( terms == null ) {
                exp = seed_term;
            }
            else {
                exp = m_Factory.makeConcatOperator( terms );
            }
        }
    ;


protected
stringExpressionAtom returns [ StringExpression exp ]
    {
        exp = null;
    }
    :   myIdent:IDENT {
            exp = m_Factory.makeVariableReference( myIdent.getText() );
        }
    |   myString:STRING_LITERAL {
            String str = myString.getText();
            exp = m_Factory.makeConstant(str.substring(1, str.length() - 1));
        }
    |   OPENPAREN exp=stringExpression CLOSEPAREN
    ;

class StringExpressionLexer extends Lexer;

options {
    charVocabulary = '\0'..'\377';
	k = 3;
   	caseSensitive = true ;
 	}

{
    private boolean singleQuote = false;

    public StringExpressionLexer(final Reader in, boolean singleQuote) {
        this(in);
        this.singleQuote = singleQuote;
    }

    public StringExpressionLexer(final InputStream in, boolean singleQuote) {
        this(in);
        this.singleQuote = singleQuote;
    }
}

PLUS : '+' ;

/* Following 3 rules copied from http://www.antlr.org/grammars/java/java.g,
   with semantic actions added and other small modifications */
STRING_LITERAL
    :   { singleQuote }?  '\'' (ESC|~('\''|'\\'))* '\''
    |   { !singleQuote }? '"' (ESC|~('"'|'\\'))* '"'
    ;

protected
ESC
    :   '\\'
        (   'n' { $setText("\n"); }
        |   'r' { $setText("\r"); }
        |   't' { $setText("\t"); }
        |   'b' { $setText("\b"); }
        |   'f' { $setText("\f"); }
        |   '"' { $setText("\""); }
        |   '\'' { $setText("'"); }
        |   '\\' { $setText("\\"); }
        |   'x' HEX_DIGIT HEX_DIGIT {
                String esc = $getText;
                char c[] = { (char) Integer.parseInt(esc.substring(2), 16) };
                String s = new String(c);
                $setText(s);
            }
        |   ('0'..'3')
            (
                options {
                    warnWhenFollowAmbig = false;
                }
            :   ('0'..'7')
                (   
                    options {
                        warnWhenFollowAmbig = false;
                    }
                :   '0'..'7'
                )?
            )? {
                String esc = $getText;
                char c[] = { (char) Integer.parseInt(esc.substring(1), 8) };
                String s = new String(c);
                $setText(s);
            }
        )
    ;

protected
HEX_DIGIT
    :   ('0'..'9'|'A'..'F'|'a'..'f')
    ;

WS
    : ( ' ' | '\t' | '\f' | ( '\n' { newline(); } ) )
    { $setType(Token.SKIP); }
    ;

CMT : "/*" (options {greedy=false;} : ( '\n' {newline();} | ~('\n') ) )* "*/" 

        {$setType(Token.SKIP);}
    ;

IDENT
    options {testLiterals=true;}
    : ( LETTER | '_' ) ( LETTER | '_' | DIGIT )*
    ;

protected
LETTER
    : 'a'..'z' | 'A'..'Z'
    ;

protected
DIGIT
    : '0'..'9'
    ;
