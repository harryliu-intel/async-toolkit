/* Grammar:

goal ::= "shape" LCURLY shapeBlock* RCURLY
shapeBlock ::= shapeFuncDecl | shapeFuncCall
shapeFuncDecl ::= "shapefun" ident
                  "(" shapeParamList ")"
                  ("{" shapeParamList "}")?
                  stringExpression ("/" stringExpression)?
                  "repeat"? mathExpression
                  "repeat"? mathExpression
                  ("bound" stringExpression)?
                  "{" (shapeFuncStatement ";")* "}"
shapeFuncStatement ::= pointList | shapeFuncCall
shapeParamList ::= (ident ("," ident)*)?
shapeFuncCall ::= ident "(" mathArgList ")" ("{" stringArgList "}")?
mathArgList ::= (mathExpression ("," mathExpression)*)?
stringArgList ::= (stringExpression ("," stringExpression)*)?
pointList ::= "[" mathExpression+ "]"

*/

header {
    package com.avlsi.fast.shapes.impl.parser;
}

class ShapeLexer extends Lexer;

options {
    charVocabulary = '\0'..'\377';
    filter = false;
    testLiterals = true;
//    importVocab = Common;
//    exportVocab = ShapeLexer;
    k = 2;
}

tokens {
    SHAPEFUN = "shapefun";
    SHAPE = "shape";
    REPEAT = "repeat";
    BOUND = "bound";
}

LBRACKET : '[';
RBRACKET : ']';
LCURLY : '{';
RCURLY : '}';
LPAREN : '(';
RPAREN : ')';
COMMA : ',';
SEMI : ';';
PLUS : '+' ;
MINUS : '-' ;
TIMES : '*' ;
SLASH : '/';
QUOTE : '"';
BACKSLASH : '\\';
WS : ( ' ' | '\t' | '\f' | ( '\n' { newline(); } ) )
     { $setType(Token.SKIP); }
   ;


IDENT
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

protected
INTEGER
    : ( DIGIT )+
    ;
protected
FLOAT
    : INTEGER '.' INTEGER
    ;

INT_OR_FLOAT
    : ( INTEGER '.' INTEGER ) => FLOAT { $setType(FLOAT); }
    | INTEGER { $setType(INTEGER); }
    ;

/* Following 3 rules copied from http://www.antlr.org/grammars/java/java.g,
   with semantic actions added and other small modifications */
STRING_LITERAL
    :   QUOTE (ESC|~('"'|'\\'))* QUOTE
    ;

protected
ESC
    :   BACKSLASH
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
                char c[] = { (char) Integer.parseInt(esc.substring(2), 8) };
                String s = new String(c);
                $setText(s);
            }
        )
    ;

protected
HEX_DIGIT
    :   ('0'..'9'|'A'..'F'|'a'..'f')
    ;

CMT : "/*" (options {greedy=false;} : ( '\n' {newline();} | ~('\n') ) )* "*/" 

        {$setType(Token.SKIP);}
    ;

{
    import java.util.ArrayList;
    import java.util.HashMap;
    import java.util.List;
    import java.util.Map;

    import com.avlsi.util.debug.Debug;

    import com.avlsi.fast.shapes.ShapeFactory;
    import com.avlsi.fast.shapes.ShapeFunction;
    import com.avlsi.fast.shapes.ShapeFunctionRedefinedException;
    import com.avlsi.fast.shapes.Point;
    import com.avlsi.fast.shapes.Shape;
    import com.avlsi.fast.shapes.WriteableShapeFunctionTable;

    import com.avlsi.util.debug.Debug;

    import com.avlsi.util.mathexpression.ExpressionCollection;
    import com.avlsi.util.mathexpression.MathExpression;
    import com.avlsi.util.mathexpression.MathExpressionFactory;
    import com.avlsi.util.mathexpression.WriteableExpressionCollection;

    import com.avlsi.fast.shapes.stringexpression.StringExpression;
    import com.avlsi.fast.shapes.stringexpression.StringExpressionFactory;
    import com.avlsi.fast.shapes.stringexpression.StringExpressionCollection;
    import com.avlsi.fast.shapes.stringexpression.WriteableStringExpressionCollection;

    import antlr.CharScanner;
    import antlr.TokenStreamSelector;
}

class ShapeParser extends Parser;
options {
    k = 1;
    buildAST = false;
//    importVocab = ShapeLexer;
}

{
    protected WriteableShapeFunctionTable table;

    protected ShapeFactory shapeFactory;
    protected MathExpressionFactory mathFactory;
    protected StringExpressionFactory stringFactory;
    protected TokenStreamSelector selector;
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
                return mathFactory.makeExpFunction(arg[0]);
            }});
        map.put("log", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeLogFunction(arg[0]);
            }});
        map.put("sin", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeSinFunction(arg[0]);
            }});
        map.put("cos", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeCosFunction(arg[0]);
            }});
        map.put("tan", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeTanFunction(arg[0]);
            }});
        map.put("asin", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeArcsinFunction(arg[0]);
            }});
        map.put("acos", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeArccosFunction(arg[0]);
            }});
        map.put("atan", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeArctanFunction(arg[0]);
            }});
        map.put("ceil", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeCeilFunction(arg[0]);
            }});
        map.put("round", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeRoundFunction(arg[0]);
            }});
        map.put("floor", new Function(1) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeFloorFunction(arg[0]);
            }});
        map.put("less", new Function(4) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                return mathFactory.makeLessThanOperator(arg[0],
                                                      arg[1],
                                                      arg[2],
                                                      arg[3]);
            }});
        map.put("sum", new Function(4) {
            public MathExpression make(MathExpression[] arg) {
                check(arg);
                final String idx = arg[0].getVariableNames()[0];
                return mathFactory.makeSumOperator(idx,
                                                 arg[1],
                                                 arg[2],
                                                 arg[3]);
            }});
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

    public ShapeParser(TokenStreamSelector selector,
                       WriteableShapeFunctionTable table,
                       ShapeFactory shapeFactory,
                       MathExpressionFactory mathFactory,
                       StringExpressionFactory stringFactory) {
        this(selector);
        this.shapeFactory = shapeFactory;
        this.mathFactory = mathFactory;
        this.stringFactory = stringFactory;
        this.selector = selector;
        this.table = table;
        funmap = new HashMap();
        fillFunmap(funmap);
    }

    public static ShapeParser makeShapeParser(ShapeLexer lexer,
                                       WriteableShapeFunctionTable table,
                                       ShapeFactory shapeFactory,
                                       MathExpressionFactory mathFactory,
                                       StringExpressionFactory stringFactory) {
        TokenStreamSelector selector = new TokenStreamSelector();
        //MathExpressionLexer mathLexer = new MathExpressionLexer(lexer.getInputState());
        selector.addInputStream(lexer, "shapeLexer");
        //selector.addInputStream(mathLexer, "mathLexer");
        selector.select("shapeLexer");
        return new ShapeParser(selector, table, shapeFactory, mathFactory, stringFactory);
    }
}

goal returns [ ShapeFunction anon ]
    {
        anon = null;
    }
    :   EOF
    |   SHAPE LCURLY {
            anon = shapeFactory.makeShapeFunction(
                "anonymous",
                stringFactory.makeConstant("noLayer"),
                stringFactory.makeConstant("noNet"),
                false, false,
                stringFactory.makeConstant("noBoundaryLayer"),
                shapeFactory.makePoint(mathFactory.makeConstant(0),
                                       mathFactory.makeConstant(0)),
                new String[0],
                new String[0]);
        } (shapeBlock[anon])* RCURLY
    ;

shapeBlock [ ShapeFunction anon ]
    {
        ShapeFunction func;
    }
    :   SHAPEFUN func = shapeFuncDecl {
            try {
                table.define(func.getName(), func);
            } catch (ShapeFunctionRedefinedException e) {
                System.err.println(e.getMessage());
            }
        }
    |   shapeFuncCall[anon]
    ;

shapeFuncDecl returns [ ShapeFunction func ]
    {
        boolean repeatx = false, repeaty = false;
        StringExpression net = null, layer;
        StringExpression bound = stringFactory.makeConstant("prBound");
        List mathParams, stringParams = null;
        MathExpression xorig, yorig;
        func = null;
    }
    :   funName:IDENT
        LPAREN mathParams = shapeParamList RPAREN
        (LCURLY stringParams = shapeParamList RCURLY)?
        layer = stringExpression
        (SLASH net = stringExpression)?
        (REPEAT {
            repeatx = true;
        })?
        xorig = mathExpression
        (REPEAT {
            repeaty = true;
        })?
        yorig = mathExpression
        (BOUND bound = stringExpression)?
        LCURLY {
            func = shapeFactory.makeShapeFunction(
                funName.getText(),
                layer,
                net,
                repeatx,
                repeaty,
                bound,
                shapeFactory.makePoint(xorig, yorig),
                (String[]) mathParams.toArray(new String[0]),
                stringParams == null ?
                    new String[0] :
                    (String[]) stringParams.toArray(new String[0])
            );
        }
        (shapeFuncStatement[func])*
        RCURLY
    ;

shapeFuncStatement [ ShapeFunction func ]
    {
    }
    :   pointList[func]
    |   shapeFuncCall[func]
    ;

shapeParamList returns [ ArrayList params ]
    {
        params = new ArrayList();
    }
    :   (
            param1:IDENT {
                params.add(param1.getText());
            }
            (
                COMMA paramk:IDENT {
                    params.add(paramk.getText());
                }
            )*
        )?
    ;

shapeFuncCall [ ShapeFunction func ]
    {
        ExpressionCollection mathArgs;
        StringExpressionCollection stringArgs =
            stringFactory.makeExpressionCollection();
    }
    :   funName:IDENT
        mathArgs = mathArgList
        (stringArgs = stringArgList)? {
            func.addFunctionCall(funName.getText(), mathArgs, stringArgs);
        }
        SEMI
    ;

mathArgList returns [ WriteableExpressionCollection args ]
    {
        args = mathFactory.makeExpressionCollection();
        MathExpression expr;
    }
    :   { LT(2).getType() != RPAREN }? LPAREN expr = mathExpression {
            args.addExpression(expr);
        }
        (
            COMMA expr = mathExpression {
                args.addExpression(expr);
            }
        )*
        RPAREN
    |   LPAREN RPAREN
    ;

stringArgList returns [ WriteableStringExpressionCollection args ]
    {
        args = stringFactory.makeExpressionCollection();
        StringExpression expr;
    }
    :   { LT(2).getType() != RCURLY }? LCURLY {
            expr = stringExpression();
            args.addExpression(expr);
        }
        (
            COMMA expr = stringExpression {
                args.addExpression(expr);
            }
        )*
        RCURLY
    |   LCURLY RCURLY
    ;

protected
pointList [ ShapeFunction func ]
    {
        MathExpression x, y, first;
        ArrayList points = new ArrayList();
        int count = 0;
    }
    :   LBRACKET x = mathExpression {
            first = x;
        }
        (
            y = mathExpression {
                count++;
                if (count % 2 == 0) {
                    points.add(shapeFactory.makePoint(y, x));
                } else {
                    points.add(shapeFactory.makePoint(x, y));
                }
                x = y;
            }
        )+
        RBRACKET {
            if (count < 4) {
                System.err.println("Warning: Only " + count +
                                   " points specified.");
            }
            if (!first.equals(x)) {
                System.err.println("Warning: Point specification may not be" +
                                   "rectilinear");
            }
            func.addPolygon(shapeFactory.makePolygon(points));
        }
        SEMI
    ;

/*
mathExpression returns [ MathExpression exp ]
    {
        exp = null;
    }
    :
    {
        selector.push("mathLexer");
        MathExpressionParser mathParser = new MathExpressionParser(getInputState(), mathFactory);
        exp = mathParser.goal();
        selector.pop();
    }
    ;
*/

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
                    terms = mathFactory.makeExpressionCollection();
                    terms.addExpression( seed_term );
                }
                
                if ( negate_next_term ) {
                    curr_term = 
                        mathFactory.makeNegationOperator( curr_term );
                }
                terms.addExpression( curr_term );
            }
        )* 
        
        {    
            if ( terms == null ) {
                exp = seed_term;
            }
            else {
                exp = mathFactory.makePlusOperator( terms );
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
                SLASH {
                    invert_next_member = true;
                }
                | TIMES {
                    invert_next_member = false;
                }
            ) 
            curr_member=mathExpressionAtom {
                if ( members == null ) {
                    members = mathFactory.makeExpressionCollection();
                    members.addExpression( seed_member );
                }
                if ( invert_next_member ) {
                    curr_member = 
                        mathFactory.makeDivideOperator( mathFactory.makeConstant(1),
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
                exp = mathFactory.makeTimesOperator( members );
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
        myFunc:IDENT LPAREN args=mathArgumentList RPAREN {
            exp=makeFunction(myFunc.getText(), args);
        }
    |   myIdent:IDENT {
            exp = mathFactory.makeVariableReference( myIdent.getText() );
        }
    |   myFloat:FLOAT {
            exp = mathFactory.makeConstant( Double.parseDouble( myFloat.getText() ) );
        }
    |   myInt:INTEGER {
            exp = mathFactory.makeConstant( Double.parseDouble( myInt.getText() ) );
        }
    |   LPAREN exp=mathExpression RPAREN
    ;

/*
stringExpression returns [ StringExpression exp ]
    {
        exp = null;
    }
    :
    {
        selector.push("stringLexer");
        StringExpressionParser stringParser = new StringExpressionParser(getInputState());
        stringParser.setFactory(stringFactory);
        exp = stringParser.goal();
        selector.pop();
    }
    ;
*/

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
                    terms = stringFactory.makeExpressionCollection();
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
                exp = stringFactory.makeConcatOperator( terms );
            }
        }
    ;


protected
stringExpressionAtom returns [ StringExpression exp ]
    {
        exp = null;
    }
    :   myIdent:IDENT {
            exp = stringFactory.makeVariableReference( myIdent.getText() );
        }
    |   myString:STRING_LITERAL {
            String str = myString.getText();
            exp = stringFactory.makeConstant(str.substring(1, str.length() - 1));
        }
    |   LPAREN exp=stringExpression RPAREN
    ;
