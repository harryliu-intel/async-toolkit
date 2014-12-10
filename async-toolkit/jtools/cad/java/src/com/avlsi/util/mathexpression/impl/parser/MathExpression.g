header {
    package com.avlsi.util.mathexpression.impl.parser;
}

{
    
    import java.util.HashMap;
    import java.util.Map;
    import java.util.ArrayList;

    import com.avlsi.util.debug.Debug;

    import com.avlsi.util.mathexpression.MathExpressionFactory;
    import com.avlsi.util.mathexpression.MathExpression;
    import com.avlsi.util.mathexpression.WriteableExpressionCollection;
    import com.avlsi.util.mathexpression.variable.WriteableVariableDictionary;
}


class MathExpressionParser extends Parser ;
options {
  k = 2;
  buildAST = false;
}

{
    protected MathExpressionFactory m_Factory;
    //protected WriteableVariableDictionary m_VarDict;
    protected HashMap funmap;

    protected abstract class Function {
        private int args;

        Function(int args) {
            this.args = args;
        }

        public void check(MathExpression[] arg) throws RuntimeException {
            if (arg.length != args) {
                throw new RuntimeException();
            }
        }

        public abstract MathExpression make(MathExpression[] arg);
    }

    private void fillFunmap(Map map) {
        map.put("exp", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeExpFunction(arg[0]);
            }});
        map.put("log", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeLogFunction(arg[0]);
            }});
        map.put("sin", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeSinFunction(arg[0]);
            }});
        map.put("cos", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeCosFunction(arg[0]);
            }});
        map.put("tan", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeTanFunction(arg[0]);
            }});
        map.put("asin", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeArcsinFunction(arg[0]);
            }});
        map.put("acos", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeArccosFunction(arg[0]);
            }});
        map.put("atan", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeArctanFunction(arg[0]);
            }});
        map.put("ceil", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeCeilFunction(arg[0]);
            }});
        map.put("round", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeRoundFunction(arg[0]);
            }});
        map.put("floor", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeFloorFunction(arg[0]);
            }});
        map.put("less", new Function(4) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return m_Factory.makeLessThanOperator(arg[0],
                                                      arg[1],
                                                      arg[2],
                                                      arg[3]);
            }});
        map.put("sum", new Function(4) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                final String idx = arg[0].getVariableNames()[0];
                return m_Factory.makeSumOperator(idx,
                                                 arg[1],
                                                 arg[2],
                                                 arg[3]);
            }});
    }
    public MathExpressionParser( TokenStream lexer,
        MathExpressionFactory mathExpressionFactory
        /*WriteableVariableDictionary varDict */ ) {
        
        this( lexer );

        m_Factory = mathExpressionFactory;
        //m_VarDict = varDict;

        funmap = new HashMap();
        fillFunmap(funmap);
    }

    protected MathExpression makeFunction(final String name,
                                          final MathExpression[] arg) {
        Function fun = (Function) funmap.get(name);
        Debug.assertTrue(fun != null,
                     "Function unknown, even though isFunction returned true.");
        return fun.make(arg);
    }

    protected boolean isFunction(final String name) {
        return funmap.containsKey(name);
    }
}

goal returns [ MathExpression exp ]
    {
        exp = null;
    }
    :
        EOF
    |   exp=mathExpression 
    ;

mathExpression returns [ MathExpression exp ]
    {
        MathExpression seed_term;
        MathExpression curr_term;
        boolean negate_next_term;
        WriteableExpressionCollection terms=null;
        exp = null;
    }
    :
        seed_term=mathExpressionProduct 
        ( 
            (  
                PLUS {
                    negate_next_term = false;
                }
                | MINUS {
                    negate_next_term = true;
                } 
            ) 
            curr_term=mathExpressionProduct {
                if ( terms == null ) {
                    terms = m_Factory.makeExpressionCollection();
                    terms.addExpression( seed_term );
                }
                
                if ( negate_next_term ) {
                    curr_term = 
                        m_Factory.makeNegationOperator( curr_term );
                }
                terms.addExpression( curr_term );
            }
        )* 
        
        {    
            if ( terms == null ) {
                exp = seed_term;
            }
            else {
                exp = m_Factory.makePlusOperator( terms );
            }
        }
    ;

protected
mathArgumentList returns [ MathExpression[] exp ]
    {
        ArrayList args = new ArrayList();
        MathExpression term;
        exp = null;
    }
    :
        term=mathExpression {
            args.add(term);
        }
        (
            COMMA term=mathExpression {
                args.add(term);
            }
        )*
        
        {
            exp = (MathExpression[]) args.toArray(new MathExpression[0]);
        }
    ;


protected
mathExpressionProduct returns [ MathExpression exp ]
    {
        MathExpression seed_member;
        MathExpression curr_member;
        boolean invert_next_member;
        WriteableExpressionCollection members = null;
        exp = null;
    }
    : 
        seed_member=mathExpressionAtom 
        ( 
            ( 
                DIV {
                    invert_next_member = true;
                }
                | TIMES {
                    invert_next_member = false;
                }
            ) 
            curr_member=mathExpressionAtom {
                if ( members == null ) {
                    members = m_Factory.makeExpressionCollection();
                    members.addExpression( seed_member );
                }
                if ( invert_next_member ) {
                    curr_member = 
                        m_Factory.makeDivideOperator( m_Factory.makeConstant(1),
                                                      curr_member );
                }
                members.addExpression( curr_member );
            }
        )*
        {
            if ( members == null ) {
                exp = seed_member;
            }
            else {
                exp = m_Factory.makeTimesOperator( members );
            }
        }
    ;


protected
mathExpressionAtom returns [ MathExpression exp ]
    {
        exp = null;
        MathExpression[] args;
    }
    :   {isFunction(LT(1).getText())}?
        myFunc:IDENT OPENPAREN args=mathArgumentList CLOSEPAREN {
            exp=makeFunction(myFunc.getText(), args);
        }
    |   myIdent:IDENT {
            exp = m_Factory.makeVariableReference( myIdent.getText() );
        }
    |   myNumber:NUMBER {
            exp = m_Factory.makeConstant( ((MathExpressionLexer.NumberToken) myNumber).getValue());
        }
    |   OPENPAREN exp=mathExpression CLOSEPAREN
    ;

class MathExpressionLexer extends Lexer;

options {
    charVocabulary = '\0'..'\377';
	k = 3;
   	caseSensitive = true ;
 	}

{
    class NumberToken extends CommonToken {
        private final double val;
        public NumberToken(final Token token, final double val) {
            super(NUMBER, token.getText());
            this.val = val;
        }
        public double getValue() {
            return val;
        }
    }
}

PLUS : '+' ;
MINUS : '-' ;
TIMES : '*' ;
DIV : '/' ;
OPENPAREN : '(' ;
CLOSEPAREN : ')' ;
COMMA : ',';

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

NUMBER
    {
        StringBuffer value = new StringBuffer();
        double scale = 1;
        double exponent = 1;
        double number = 0;
    }
    :   ( DIGITS ( '.' | 'e' )? )=>
        digit:DIGITS {
            value.append(digit.getText());
        }
        (
            '.' {
                value.append('.');
            }
            (decimal:DIGIT {
                value.append(decimal.getText());
            })*
            (exponent = EXPONENT)?
        |   exponent = EXPONENT
        )?
        (scale = SUFFIX CHARS)? {
            try {
                number = Double.parseDouble(value.toString());
            } catch (NumberFormatException e) {
                System.err.println("Invalid number " + value + "!");
            }
            number *= scale * exponent;
            final NumberToken t1 = new NumberToken(digit, number);
            $setToken(t1);
        }
    |   '.' integer:DIGITS (exponent = EXPONENT)? (scale = SUFFIX CHARS)? {
            try {
                number = Integer.parseInt(integer.getText());
            } catch (NumberFormatException e) {
                System.err.println("Invalid number " + number + "!");
            }
            number *= scale * exponent;
            final NumberToken t2 = new NumberToken(integer, number);
            $setToken(t2);
        }
    ;

protected
EXPONENT returns [ double exponent ]
    {
        boolean neg = false;
        exponent = 1;
    }
    :   ( 'e' ) ( '+' | '-' { neg = true; } )?
        exp:DIGITS {
            try {
                exponent = Long.parseLong(exp.getText(), 10);
            } catch (NumberFormatException e) {
                System.err.println("Exponent " + exp.getText() + " too large!");
            }
            if (neg) exponent = -exponent;
            exponent = Math.pow(10, exponent);
        }
    ;

protected
SUFFIX returns [ double scale ]
    {
        scale = 1.0;
    }
    :   'm' { scale = 1e-3; }
    |   'u' { scale = 1e-6; }
    |   'n' { scale = 1e-9; }
    |   'p' { scale = 1e-12; }
    |   'f' { scale = 1e-15; }
    ;

protected
LETTER
    : 'a'..'z' | 'A'..'Z'
    ;

protected
DIGIT
    : '0'..'9'
    ;

protected
DIGITS
    : (DIGIT)+
    ;

protected
CHARS
    : ( '!'..'<' | '>'..'~' )*
    ;
