/* Requires antlr 2.7.1 or newer, because lexer uses column information. */
/* Does not handle forward references */

header {
    package com.avlsi.file.cdl.parser;
}

{
    import java.util.ArrayList;
    import java.util.Iterator;
    import java.util.List;

    import antlr.RecognitionException;
    import antlr.TokenStreamSelector;
    import antlr.TokenStreamException;
    import antlr.collections.AST;

    import com.avlsi.cast.impl.ASTWithInfo;
    import com.avlsi.cast.impl.AmbiguousLookupException;
    import com.avlsi.cast.impl.BoolValue;
    import com.avlsi.cast.impl.Environment;
    import com.avlsi.cast.impl.FloatValue;
    import com.avlsi.cast.impl.InvalidOperationException;
    import com.avlsi.cast.impl.Symbol;
    import com.avlsi.cast.impl.TokenWithInfo;
    import com.avlsi.cast.impl.TupleValue;
    import com.avlsi.cast.impl.Range;
    import com.avlsi.cast.impl.UserDefinedValue;
    import com.avlsi.cast.impl.Value;
    import com.avlsi.cast2.impl.CastTwoLexer;
    import com.avlsi.cast2.impl.CastTwoParser;
    import com.avlsi.cast2.impl.CastTwoTreeParser;
    import com.avlsi.util.container.Pair;
    import com.avlsi.util.container.Triplet;
}

class CDLLexer extends Lexer;
options {
    charVocabulary = '\0'..'\377';
    k = 2;
    caseSensitive = false;
}

{
    private boolean extension = false;
    private boolean nl = false;

    public CDLLexer(Reader in, boolean extension) {
        this(in);
        this.extension = extension;
        setTokenObjectClass(TokenWithInfo.class.getName());
    }

    public void consume() throws CharStreamException {
        char c = LA(1);
        if (c == '\n') nl = true;
        else if (!Character.isWhitespace(c)) nl = false;
        super.consume();
    }

    public static abstract class InfoToken extends TokenWithInfo {
        protected InfoToken(int t, String s) {
            super(t, s);
        }
        protected InfoToken(int t, Token o) {
            this(t, o.getText());
            setFilename(o.getFilename());
            setColumn(o.getColumn());
            setLine(o.getLine());
        }
        public abstract String getText(Environment env);
        public abstract Double getValue(Environment env);
        public String getSpiceString(Environment env) {
            return getText(env);
        }
        public String getFqcn(Environment env) {
            return getText(env);
        }
    }

    public static class SimpleToken extends InfoToken {
        private final Double val;
        private final String text;
        public SimpleToken(final Double val) {
            this(val, val.toString());
        }
        public SimpleToken(final Double val, final String text) {
            super(0, text);
            this.val = val;
            this.text = text;
        }
        public String getText(Environment env) {
            return text;
        }
        public Double getValue(Environment env) {
            return val;
        }
    }

    private interface ParserAction {
        void execute(CastTwoParser parser) throws RecognitionException,
                                                  TokenStreamException;
    }

    static AST evaluate(String expr, ParserAction act) {
        final CastTwoParser castParser =
            CastTwoParser.getParser(expr, 0, 0, "<netlist block>");
        try {
            act.execute(castParser);
        } catch (RecognitionException e) {
System.err.println("evaluate: " + e);
            return null;
        } catch (TokenStreamException e) {
System.err.println("evaluate: " + e);
            return null;
        }
        return castParser.getAST();
    }

    /** Use the expression rule in the CAST parser to parse the expression into
     * an AST.
     **/
    static AST evaluate(String expr) {
        return evaluate(expr,
                        new ParserAction() {
                            public void execute(CastTwoParser parser)
                            throws RecognitionException, TokenStreamException {
                                parser.startExpression();
                            }
                        });
    }

    // Parse an expression list using the CAST parser and return the AST.
    static AST evaluateList(String expr) {
        return evaluate(expr,
                        new ParserAction() {
                            public void execute(CastTwoParser parser)
                            throws RecognitionException, TokenStreamException {
                                parser.startExpressionList();
                            }
                        });
    }

    private interface TreeParserAction {
        Value execute(CastTwoTreeParser parser) throws RecognitionException;
    }

    static Value evaluate(AST ast, TreeParserAction act) {
        if (ast == null) return null;
        final CastTwoTreeParser treeParser = new CastTwoTreeParser();
        try {
            return act.execute(treeParser);
        } catch (RecognitionException e) {
//System.err.println("evaluate: " + e);
            return null;
        }
    }

    // Parse an expression list using the CAST parser and return the AST.
    static Value evaluateList(final AST ast, final Environment env) {
        return evaluate(ast,
                        new TreeParserAction() {
                            public Value execute(CastTwoTreeParser parser)
                            throws RecognitionException {
                                return parser.expressionList(ast, env, false);
                            }
                        });
    }

    /** Use the expression rule in the CAST tree parser to evaluate an
     * expression given an AST of the expression and an environment.
     **/
    static Value evaluate(final AST ast, final Environment env) {
        return evaluate(ast,
                        new TreeParserAction() {
                            public Value execute(CastTwoTreeParser parser)
                            throws RecognitionException {
                                return parser.expression(ast, env, false);
                            }
                        });
    }

    /**
     * Evaluate a string represented as an expression in a given environment.
     **/
    static Value evaluate(String expr, Environment env) {
        return evaluate(evaluate(expr), env);
    }

    static Boolean evaluateBoolean(String expr, Environment env) {
        Value v = evaluate(expr, env);
        if (v == null) return null;
        else {
            try {
                return new Boolean(BoolValue.valueOf(v).getValue());
            } catch (InvalidOperationException e) {
                return null;
            }
        }
    }

    static Double evaluateDouble(AST ast, Environment env) {
        Value v = evaluate(ast, env);
        if (v == null) return null;
        else {
            try {
                return new Double(FloatValue.valueOf(v).getValue());
            } catch (InvalidOperationException e) {
                return null;
            }
        }
    }

    static Double evaluateDouble(String expr, Environment env) {
        return evaluateDouble(evaluate(expr), env);
    }

    static Double toDouble(final Value v) {
        if (v == null) return null;
        else {
            try {
                return new Double(FloatValue.valueOf(v).getValue());
            } catch (InvalidOperationException e) {
                return null;
            }
        }
    }

    static Range evaluateRange(String expr, Environment env) {
        // XXX: Keep track of line and column numbers, and filename
        final CastTwoParser castParser =
            CastTwoParser.getParser(expr, 0, 0, "<netlist block>");
        
        try {
            castParser.startRange();
        } catch (RecognitionException e) {
            return null;
        } catch (TokenStreamException e) {
            return null;
        }

        final AST node = castParser.getAST();

        final CastTwoTreeParser treeParser = new CastTwoTreeParser();
        try {
            return treeParser.range(node, env, true);
        } catch (RecognitionException e) {
            return null;
        }
    }

    /**
     * Class representing a token that "looks" like an double.  The problem is
     * the lexer cannot tell whether this token should be interpreted as a
     * double, or as a node name, since SPICE allows almost anything to be a
     * node name.  This class keeps track of the string and the double value,
     * so the parser can decide later.  Array indexing might require parameter
     * substitution.
     **/
    class NameToken extends InfoToken {
        private List name;
        private Double val;

        public NameToken(final List name) {
            super(NODE, (Token) name.get(0));
            this.name = name;
            this.val = null;
        }

        public NameToken(final Token token, Double val) {
            super(NODE, token);
            name = new ArrayList();
            name.add(token);
            this.val = val;
        }

        private String getIndexText(final Environment env, final List indices) {
            StringBuffer buf = new StringBuffer();
            for (Iterator i = indices.iterator(); i.hasNext(); ) {
                final List l = (List) i.next();
                buf.append(getText(env, l));
                if (i.hasNext()) buf.append(",");
            }
            return buf.toString();
        }
        
        private String getText(final Environment env, final List name) {
            StringBuffer buf = new StringBuffer();
            for (Iterator i = name.iterator(); i.hasNext(); ) {
                final Object o = i.next();
                if (o instanceof Triplet) {
                    final Triplet p = (Triplet) o;
                    List l = (List) p.getSecond();
                    buf.append((String) p.getFirst());
                    String indexText = getIndexText(env, l);
                    if (l.size() > 1 || !extension) {
                        buf.append(indexText);
                    } else {
                        Double element = evaluateDouble(indexText, env);
                        if (element == null) buf.append(indexText);
                        else buf.append(element.intValue());
                    }
                    buf.append((String) p.getThird());
                } else {
                    Token t = (Token) o;
                    buf.append(t.getText());
                }
            }
            return buf.toString();
        }

        public String getText(Environment env) {
            return getText(env, name);
        }

        public Double getValue(Environment env) {
            if (val != null) return val;
            if (name.size() == 1) {
                final Token t = (Token) name.get(0);
                final String key = t.getText();
                try {
                    final Value v = env.lookup(Symbol.create(key));
                    if (v == null)
                        return null;
                    return new Double(FloatValue.valueOf(v).getValue());
                } catch (AmbiguousLookupException e) {
                    return null;
                } catch (InvalidOperationException e) {
                    return null;
                }
            }
            return null;
        }

        public String getFqcn(final Environment env) {
            StringBuffer buf = new StringBuffer();
            for (Iterator i = name.iterator(); i.hasNext(); ) {
                final Object o = i.next();
                if (o instanceof Triplet) {
                    final Triplet p = (Triplet) o;
                    List l = (List) p.getSecond();
                    final String lbracket = (String) p.getFirst();
                    String indexText = getIndexText(env, l);

                    final TupleValue v;
                    if (extension && lbracket.equals("(")) {
                        v = (TupleValue) evaluateList(evaluateList(indexText),
                                                      env);
                    } else {
                        v = null;
                    }

                    if (v == null) {
                        buf.append(lbracket);
                        buf.append(indexText);
                        buf.append((String) p.getThird());
                    } else {
                        buf.append(UserDefinedValue.getTypeName("", v));
                    }
                } else {
                    Token t = (Token) o;
                    buf.append(t.getText());
                }
            }
            return buf.toString();
        }
    }

    class MathExprToken extends InfoToken {
        private AST parsedAST;
        public MathExprToken(final Token token) {
            super(NODE, token);
            parsedAST = evaluate(super.getText());
        }

        public String getText(Environment env) {
            return super.getText();
        }

        public Double getValue(Environment env) {
            return evaluateDouble(parsedAST, env);
        }

        public String getSpiceString(Environment env) {
            return "'" + getText(env) + "'";
        }
    }

    class LoopToken extends Token {
        private Token ident, range;

        public LoopToken(Token ident, Token range) {
            super(LOOP_BEGIN, ident.getText());
            this.ident = ident;
            this.range = range;
        }

        public String getIdent() {
            return ident.getText();
        }

        public Range getRange(Environment env) {
            return evaluateRange(range.getText(), env);
        }
    }

    public Token makeToken(final int t) {
        final Token tok = super.makeToken(t);
        tok.setFilename(getFilename());
        return tok;
    }
}

EQUAL : '=';

WS  :   ( ' ' 
        | '\t' 
        | '\f'
        | '\r'
        | ( ( '\n' { newline(); } ) ( '+' )? )
        )
        { $setType(Token.SKIP); }
    ;

TOKEN
    :   { getColumn() == 1 || (nl && extension) }?
        (
            ".subckt" {
                $setType(SUBCKT);
            }
        |   ".ends" {
                $setType(ENDS);
            }
        |   ".param" {
                $setType(PARAM);
            }
        |   device:DEVICE {
                $setToken(device);
                $setType(device.getType());
            }
        |   CMT {
                $setType(Token.SKIP);
            }
        |   { extension }? '<' var:IDENT ':' range:RANGE_CHARS ':' {
                LoopToken t = new LoopToken(var, range);
                $setToken(t);
            }
        |   { extension }? '>' {
                $setType(LOOP_END);
            }
        |   { extension }? '['! ( options { greedy = false; }: ~('\n') )* "->"! {
                $setType(IF_BEGIN);
            }
        |   { extension }? ']' {
                $setType(IF_END);
            }
        )
    |   node:NODE {
            $setToken(node);
            $setType(NODE);
        }
    ;

protected
RANGE_CHARS
    :   ( ~( '\n' | ':' ) )+
    ;

protected
CMT
    : '*' ( ~('\n') )* { $setType(Token.SKIP); }
    ;

protected
NODE
    {
        double val = 0;
        List names;
    }
    :   ( SIGNED_NUMBER ( ' ' | '\t' | '\n' ) )=>
        val = number:SIGNED_NUMBER {
            NameToken t = new NameToken(number, new Double(val));
            $setToken(t);
        }
    |   ( MATH_EXPR )=>
        expr:MATH_EXPR {
            MathExprToken t = new MathExprToken(expr);
            $setToken(t);
        }
    |   names = NODE_NAME {
            NameToken t = new NameToken(names);
            $setToken(t);
        }
    ;

protected
DEVICE
    {
        List names;
        Token t;
        int type = 0;
    }
    :   (
            'r'! { type = RESISTOR; }
        |   'c'! { type = CAPACITOR; }
        |   'm'! { type = TRANSISTOR; }
        |   'x'! { type = SUBCELL; }
        |   'd'! { type = DIODE; }
        |   'l'! { type = INDUCTOR; }
        |   'q'! { type = BIPOLAR; }
        ) 
        names = NODE_NAME {
            t = new NameToken(names);
            t.setType(type);
            $setToken(t);
        }
    ;

protected
SIGNED_NUMBER returns [ double number ]
    {
        boolean neg = false;
    }
    :   ( '-' { neg = true; } )? number = NUMBER {
            if (neg) number = -number;
        }
    ;

protected
NUMBER returns [ double number ]
    {
        StringBuffer value = new StringBuffer();
        double scale = 1;
        double exponent = 1;
        number = 0;
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
        }
    |   '.' integer:DIGITS (exponent = EXPONENT)? (scale = SUFFIX CHARS)? {
            try {
                number = Integer.parseInt(integer.getText());
            } catch (NumberFormatException e) {
                System.err.println("Invalid number " + number + "!");
            }
            number *= scale * exponent;
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
    |   'k' { scale = 1e3; }
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

protected
MATH_EXPR
    : '\''! ( ~( '\'' | '\n' ) )* '\''!
    ;

protected
LETTER
    : 'a'..'z'
    ;

protected
IDENT
    : ( LETTER | '_' ) ( LETTER | DIGIT | '_' )*
    ;

/* Printable characters excluding ' ', ',', '=', '(', ')', '[', ']', '{', '}' */
protected
NODE_CHARS
    : ( '!' | '#'..'\'' | '*'..'+' | '-'..'<' | '>'..'@' | '\\' | '^'..'z' | '~' | '|' )+
    ;

protected
NODE_NAME_BASE [ List name ]
    :   base:NODE_CHARS {
            name.add(base);
        }
    ;

protected
NODE_NAME_LIST [ List name ]
    {
        List indices;
        Triplet triple;
    }
    :   triple = BRACKET_BLOCK {
            name.add(triple);
        }
        ( selector:NODE_CHARS {
             name.add(selector); 
        } )?
    ;
            
protected
NODE_NAME returns [ List name ]
    {
        name = new ArrayList();
    }
    :   NODE_NAME_BASE[ name ]
        (
            NODE_NAME_LIST[ name ]
        )*
    ;

protected
NODE_NAME_OPT returns [ List name ]
    {
        name = new ArrayList();
    }
    :   ( NODE_NAME_BASE[ name ] )?
        (
            NODE_NAME_LIST[ name ]
        )*
    ;

protected
INDEX returns [ List indices ]
    {
        indices = new ArrayList();
        List index;
    }
    :   index = NODE_NAME_OPT { indices.add(index); }
        ( ',' index = NODE_NAME_OPT { indices.add(index); } )*
    ;

protected
BRACKET_BLOCK returns [ Triplet bracket ]
    {
        bracket = null;
        List indices;
    }
    :   '[' indices = INDEX ']' { bracket = new Triplet("[", indices, "]"); }
    |   '{' indices = INDEX '}' { bracket = new Triplet("{", indices, "}"); }
    |   '(' indices = INDEX ')' { bracket = new Triplet("(", indices, ")"); }
    ;

{
    import antlr.CommonAST;
}

class CDLParser extends Parser;
options {
    k = 2;
    buildAST = true;
    ASTLabelType = "ASTWithToken";
    defaultErrorHandler = false;
}

tokens {
    LOOP_BEGIN;
    LOOP_END;
    IF_BEGIN;
    IF_END;
}

{
    public static class ASTWithToken extends CommonAST {
        private Token token = null;

        public void initialize(Token tok) {
            super.initialize(tok);
            this.token = tok;
        }

        public void initialize(final AST ast) {
            super.initialize(ast);
            setToken(((ASTWithToken) ast).getToken());
        }

        public void setToken(Token tok) {
            token = tok;
        }

        public Token getToken() {
            return token;
        }
    }
}

goal
    :   deviceList EOF!
    ;

param
    :   PARAM^ parameterList
    ;

subcircuit
    :   subcircuitStart deviceList subcircuitEnd
    ;

parameterList
    :   ( parameter )+
        { #parameterList = #( [PARAMETERS], #parameterList ); }
    ;

parameter
    :   NODE { #parameter = #( [NOEQUAL], #parameter ); }
    |   NODE EQUAL^ NODE
    ;


device
    :   RESISTOR^ NODE NODE parameterList
    |   CAPACITOR^ NODE NODE parameterList
    |   TRANSISTOR^ NODE NODE NODE NODE NODE parameterList
    |   DIODE^ NODE NODE NODE parameterList
    |   INDUCTOR^ NODE NODE parameterList
    |   BIPOLAR^ NODE NODE NODE NODE parameterList
    |   SUBCELL^ parameterList
    |   LOOP_BEGIN^ deviceList LOOP_END!
    |   IF_BEGIN^ deviceList IF_END!
    |   subcircuit
    |   param
    ;

startDeviceList
    :   deviceList EOF!
    ;

deviceList
    :   ( device )*
        { #deviceList = #( [DEVICES], #deviceList ); }
    ;

expr
    :   RESISTOR^ NODE NODE parameterList
    |   CAPACITOR^ NODE NODE parameterList
    |   TRANSISTOR^ NODE NODE NODE NODE NODE parameterList
    |   DIODE^ NODE NODE NODE parameterList
    |   INDUCTOR^ NODE NODE parameterList
    |   BIPOLAR^ NODE NODE NODE NODE parameterList
    |   SUBCELL^ parameterList
    |   subcircuitStart
    |   subcircuitEnd
    |   EOF!
    ;

subcircuitStart
    :   SUBCKT^ parameterList
    ;

subcircuitEnd
    :   ENDS^ ( NODE! )*
    ;

{
    import java.util.ArrayList;
    import java.util.LinkedHashMap;
    import java.util.Iterator;
    import java.util.List;
    import java.util.Map;
    import java.util.Stack;

    import com.avlsi.file.cdl.parser.CDLFactoryInterface;
    import com.avlsi.file.cdl.parser.CDLLexer.InfoToken;
    import com.avlsi.file.cdl.parser.CDLLexer.NameToken;
    import com.avlsi.file.common.HierName;
    import com.avlsi.file.common.InvalidHierNameException;
    import com.avlsi.cast.impl.ArrayValue;
    import com.avlsi.cast.impl.BlockEnvironment;
    import com.avlsi.cast.impl.BoolValue;
    import com.avlsi.cast.impl.Environment;
    import com.avlsi.cast.impl.EnvironmentEntry;
    import com.avlsi.cast.impl.EnvironmentEntryIterator;
    import com.avlsi.cast.impl.FloatValue;
    import com.avlsi.cast.impl.IntValue;
    import com.avlsi.cast.impl.LocalEnvironment;
    import com.avlsi.cast.impl.LoopEnvironment;
    import com.avlsi.cast.impl.Range;
    import com.avlsi.cast.impl.Symbol;
    import com.avlsi.cast.impl.SymbolRedeclaredException;
    import com.avlsi.cast.impl.UserDefinedValue;
    import com.avlsi.cast.impl.Value;
    import com.avlsi.util.debug.Debug;
}

class CDLWalker extends TreeParser;

options {
    ASTLabelType = "CDLParser.ASTWithToken";
}

{
    private Stack subNameStack = new Stack();

    private HierName makeHierName(final String s, char sep) {
        HierName name = null;
        try {
            name = HierName.makeHierName(s, sep);
        } catch (InvalidHierNameException e) {
            Debug.assertTrue(false, "Cannot makeHierName");
        }
        return name;
    }

    private String toStr(final Token t, Environment env) {
        return ((InfoToken) t).getText(env);
    }

    private HierName toHier(final Token t, Environment env) {
        return makeHierName(toStr(t, env), '.');
    }

    private Double toDouble(final Environment env, final InfoToken token)
    throws RecognitionException {
        final Double ret = token.getValue(env);
        if (ret == null) {
            throw new RecognitionException("Expecting an numerical value, found " + token + "!");
        }
        return ret;
    }

    private int findList(final List l, final String text) {
        int index = 0;
        for (Iterator i = l.iterator(); i.hasNext(); index++) {
            Token t = (Token) i.next();
            if (t.getText().equals(text)) return index;
        }
        return -1;
    }

    private void bindParams(final Map<String,InfoToken> binding,
                            final Environment env) throws RecognitionException {
        for (Map.Entry<String,InfoToken> entry : binding.entrySet()) {
            final Symbol key = Symbol.create(entry.getKey());
            final Double val = toDouble(env, entry.getValue());
            if (val == null) {
                throw new RecognitionException("Invalid value specified for parameter " + key.getString() + "!");
            }
            try {
                env.bind(key, new FloatValue(val.doubleValue()));
            } catch (SymbolRedeclaredException e) {
                throw (RecognitionException) new RecognitionException("Cannot assign to parameter " + key.getString() + " more than once!").initCause(e);
            }
        }
    }

    private static boolean isSimpleValue(final Value v) {
        if (v instanceof ArrayValue) {
            // an array is simple if its elements are simple
            final Value ev = ((ArrayValue) v).getIterator().next();
            return isSimpleValue(ev);
        } else {
            return v instanceof BoolValue ||
                   v instanceof FloatValue ||
                   v instanceof IntValue;
        }
    }

    /**
     * Copy simple values (floats, integers, booleans, arrays) from the given
     * environment and return it as a new environment.  The values are shallow
     * copied (i.e., not <code>duplicate</code>d).  The purpose is to remove
     * pointers to <code>UserDefinedValue</code>s, which hold cell information,
     * in the environment, so that they may be garbage collected.  This is
     * necessary because devices in <code>Template</code> keeps a copy of the
     * environment.  That requirement should be re-thought.<p>
     **/
    public static Environment sanitizeEnvironment(final Environment env) {
        final Environment clean = new LocalEnvironment();
        for (EnvironmentEntryIterator i = env.entryIterator(); i.hasNext(); ) {
            final EnvironmentEntry entry = i.next();
            final Value v = entry.getValue();
            if (isSimpleValue(v)) {
                final Symbol name = entry.getName();
                if (!clean.contains(name)) {
                    try {
                        clean.bind(entry.getName(), v);
                    } catch (SymbolRedeclaredException e) {
                        Debug.assertTrue(false, "Cannot happen!");
                    }
                }
            }
        }
        return clean;
    }

    /**
     * Parse [mname] ( val | key=val ).  Added $.MODEL=mname to
     * <code>binding</code> if model name exists; returns val.
     **/
    private InfoToken parseModelValue(final Environment env,
                                      final List<InfoToken> param,
                                      final Map<String,InfoToken> binding,
                                      final String key) {
        InfoToken val = null;
        int currParam = 0;
        if (binding.containsKey(key)) {
            val = binding.remove(key);
        } else if (binding.containsKey(key.toUpperCase())) {
            val = binding.remove(key.toUpperCase());
        }
        if (param.size() > currParam) {
            InfoToken token = param.get(currParam++);
            if (val != null ||
                param.size() > currParam ||
                token instanceof NameToken && token.getValue(env) == null) {
                binding.put("$.MODEL", token);
            } else {
                val = token;
            }
        }
        if (val == null && param.size() > currParam) val = param.get(currParam++);
        return val;
    }
}


expr[ Environment env, CDLFactoryInterface factory ]
    :   resistor[env,factory]
    |   capacitor[env,factory]
    |   transistor[env,factory]
    |   inductor[env,factory]
    |   diode[env,factory]
    |   bipolar[env,factory]
    |   subcell[env,factory]
    |   param[ env ]
    |   subcircuitStart[ env, factory ]
    |   subcircuitEnd[ env, factory ]
    ;

goal [ Environment env, CDLFactoryInterface factory ]
    :   deviceList[ env, factory ]
    ;

param [ Environment env ]
    {
        List<InfoToken> param = new ArrayList<>();
        Map<String,InfoToken> binding = new LinkedHashMap<>();
    }
    :   #( PARAM parameterList[ env, param, binding ] {
            bindParams(binding, env);
        })
    ;

subcircuit [ Environment env, CDLFactoryInterface factory ]
    {
        Environment localEnv = new BlockEnvironment(env);
    }
    :   subcircuitStart[ localEnv, factory ]
        deviceList[ localEnv, factory ]
        subcircuitEnd[ env, factory ]
    ;

subcircuitStart[ Environment env, CDLFactoryInterface factory ]
    {
        List<InfoToken> param = new ArrayList<>();
        Map<String,InfoToken> binding = new LinkedHashMap<>();
    }
    :   #( SUBCKT parameterList[ env, param, binding ] )  {
            List<String> input = new ArrayList<>(), output = new ArrayList<>();
            String subName = null;
            if (param.size() >= 1) {
                Iterator<InfoToken> i = param.iterator();
                subName = i.next().getText(env);
                List<String> current = input;
                while (i.hasNext()) {
                    InfoToken t = i.next();
                    String ttext = t.getText(env);
                    if (ttext.equals("/")) {
                        if (current == input) {
                            current = output;
                            continue;
                        } else {
                            throw new RecognitionException("Too many slashes in prototype of subcircuit " + subName + "!");
                        }   
                    }
                    current.add(ttext);
                }
            } else {
                throw new RecognitionException("Subcircuit definition missing name!");
            }
            bindParams(binding, env);
            subNameStack.push(subName);
            factory.beginSubcircuit(subName,
                                    input.toArray(new String[0]),
                                    output.toArray(new String[0]),
                                    binding,
                                    env);
        }
    ;

subcircuitEnd[ Environment env, CDLFactoryInterface factory ]
    :   ENDS
        { 
            String subName = (String)subNameStack.pop();
            factory.endSubcircuit(subName, env);
        }
    ;


parameterList [ Environment env, List<InfoToken> param, Map<String,InfoToken> binding ]
    :   #( PARAMETERS ( parameter[ env, param, binding ] )+ )
    ;

deviceList [ Environment env, CDLFactoryInterface factory ]
    :   #( DEVICES ( device[ env, factory ] )* )
    ;

parameter[ Environment env, List<InfoToken> param, Map<String,InfoToken> binding ]
    :   #( NOEQUAL node:NODE ) {
            param.add((InfoToken) node.getToken());
        }
    |   #( EQUAL key:NODE val:NODE ) {
            binding.put(key.getToken().getText(), (InfoToken) val.getToken());
        }
    ;
      
device[ Environment env, CDLFactoryInterface factory ]
    :   resistor[env,factory]
    |   capacitor[env,factory]
    |   transistor[env,factory]
    |   inductor[env,factory]
    |   diode[env,factory]
    |   bipolar[env,factory]
    |   subcell[env,factory]
    |   #( loop:LOOP_BEGIN loopdev:DEVICES ) {
            // XXX: Should refactor with loopStatement in CastTwoTree.g
            final CDLLexer.LoopToken token = (CDLLexer.LoopToken) loop.getToken();
            final Symbol var = Symbol.create(token.getIdent());
            final Range range = token.getRange(env);
            if (range == null) {
                System.err.println("Loop range uninitialized.");
            } else {
                for (Range.Iterator ri = range.iterator(); ri.hasNext(); ) {
                    final int i = ri.next();
                    final Environment loopEnv =
                        new LoopEnvironment(env, var, IntValue.valueOf(i));
                    deviceList(loopdev, loopEnv, factory);
                }
            }
        }
    |   #( cond:IF_BEGIN ifdev:DEVICES ) {
            // XXX: Should refactor with ifStatement in CastTwoTree.g
            final Boolean guard = CDLLexer.evaluateBoolean(cond.getToken().getText(), env);
            if (guard == null) {
                System.err.println("If guard uninitialized.");
            } else {
                if (guard.booleanValue()) {
                    deviceList(ifdev, env, factory);
                }
            }
        }
    |   subcircuit[ env, factory ]
    |   param[ env ]
    ;

resistor[Environment env,  CDLFactoryInterface factory]
    {
        List<InfoToken> param = new ArrayList<>();
        Map<String,InfoToken> binding = new LinkedHashMap<>();
    }
    :   #( r:RESISTOR n1:NODE n2:NODE parameterList[ env, param, binding ] ) {
            // Rxxxxxxx n1 n2 [mname] ( resistance | R=resistance )
            InfoToken res = parseModelValue(env, param, binding, "r");
            if (res == null) {
                throw new RecognitionException("No resistance specified for resistor " + r.getText() + "!");
            } else {
                factory.makeResistor(toHier(#r.getToken(), env),
                                     toHier(#n1.getToken(), env),
                                     toHier(#n2.getToken(), env),
                                     res,
                                     binding, env);
            }
        }
    ;

capacitor[Environment env,  CDLFactoryInterface factory]
    {
        List<InfoToken> param = new ArrayList<>();
        Map<String,InfoToken> binding = new LinkedHashMap<>();
    }
    :   #( c:CAPACITOR npos:NODE nneg:NODE
           parameterList[ env, param, binding ] ) {
            // Cxxxxxx n+ n- [mname] ( capacitance | C=capacitance )
            InfoToken cap = parseModelValue(env, param, binding, "c");
            if (cap == null) {
                throw new RecognitionException("No capacitance specified for capacitor " + c.getText() + "!");
            } else {
                factory.makeCapacitor(toHier(#c.getToken(), env),
                                      toHier(#npos.getToken(), env),
                                      toHier(#nneg.getToken(), env),
                                      cap, binding,
                                      env);
            }
        }   
    ;

transistor[Environment env,  CDLFactoryInterface factory]
    {
        List<InfoToken> param = new ArrayList<>();
        Map<String,InfoToken> binding = new LinkedHashMap<>();
    }
    :   #( t:TRANSISTOR nd:NODE ng:NODE ns:NODE nb:NODE mname:NODE  
            parameterList[ env, param, binding ] ) {
            InfoToken w = null, l = null;

            if (binding.containsKey("W")) w = binding.remove("W");
            else if (binding.containsKey("w")) w = binding.remove("w");

            if (binding.containsKey("L")) l = binding.remove("L");
            else if (binding.containsKey("l")) l = binding.remove("l");

            if (w == null || l == null) {
                throw new RecognitionException("No length or width specified for transistor " + t.getText() + "!");
            } else {
                factory.makeTransistor(toHier(#t.getToken(), env),
                                       toStr(#mname.getToken(), env),
                                       toHier(#ns.getToken(), env),
                                       toHier(#nd.getToken(), env),
                                       toHier(#ng.getToken(), env),
                                       toHier(#nb.getToken(), env),
                                       w,
                                       l,
                                       binding, env);
            }
        }
    ;

inductor[Environment env,  CDLFactoryInterface factory]
    {
        List<InfoToken> param = new ArrayList<>();
        Map<String,InfoToken> binding = new LinkedHashMap<>();
    }
    :   #( l0:INDUCTOR lpos:NODE lneg:NODE
           parameterList[ env, param, binding ] ) {
            // Lxxxxxx n+ n- inductance
            InfoToken ind = null;
            if (param.size() >= 1) {
                ind = param.get(0);
            }
            if (ind == null) {
                throw new RecognitionException("No inductance specified for inductor " + l0.getText() + "!");
            } else {
                factory.makeInductor(toHier(#l0.getToken(), env),
                                     toHier(#lpos.getToken(), env),
                                     toHier(#lneg.getToken(), env),
                                     ind,
                                     binding, env);
            }
        }
    ;

diode[Environment env,  CDLFactoryInterface factory]
    {
        List<InfoToken> param = new ArrayList<>();
        Map<String,InfoToken> binding = new LinkedHashMap<>();
    }
    :   #( d:DIODE dpos:NODE dneg:NODE dtype:NODE
           parameterList[ env, param, binding ] ) {
            // Dxxxxxxx n1 n2 type ( area | area=area )
            InfoToken area = null;
            if (binding.containsKey("area")) area = binding.remove("area");
            else if (binding.containsKey("AREA")) area = binding.remove("AREA");
            else if (param.size() >= 1) area = param.get(0);
            if (area == null) {
                throw new RecognitionException("No area specified for diode " + d.getText() + "!");
            } else {
                factory.makeDiode(toHier(#d.getToken(), env),
                                  dtype.getText(),
                                  toHier(#dpos.getToken(), env),
                                  toHier(#dneg.getToken(), env),
                                  area,
                                  binding, env);
            }
        }
    ;

bipolar[Environment env,  CDLFactoryInterface factory]
    {
        List<InfoToken> param = new ArrayList<>();
        Map<String,InfoToken> binding = new LinkedHashMap<>();
    }
    :   #( q:BIPOLAR nc:NODE nb:NODE ne:NODE mname:NODE
           parameterList[ env, param, binding ] ) {
            // Qxxxxxxx nc nb ne type ( area | area=area )
            InfoToken area = null;
            if (binding.containsKey("area")) area = binding.remove("area");
            else if (binding.containsKey("AREA")) area = binding.remove("AREA");
            else if (param.size() >= 1) area = param.get(0);
            if (area == null) {
                throw new RecognitionException("No area specified for BJT " + q.getText() + "!");
            } else {
                factory.makeBipolar(toHier(#q.getToken(), env),
                                    mname.getText(),
                                    toHier(#nc.getToken(), env),
                                    toHier(#nb.getToken(), env),
                                    toHier(#ne.getToken(), env),
                                    area,
                                    binding, env);
            }
        }
    ;

subcell[Environment env,  CDLFactoryInterface factory]
    {
        List<InfoToken> param = new ArrayList<>();
        Map<String,InfoToken> binding = new LinkedHashMap<>();
    }
    :   #( call:SUBCELL parameterList[ env, param, binding ] ) {
            final int slash = findList(param, "/");
            
            final int numSubCircuitArgs;
            //findList should always return >= -1
            assert ( slash >= -1 );
            
            final InfoToken subToken;
            //If there is a slash, it must only be followed by exactly one token.
            if ( slash > -1 ) {
                if ( ( slash + 2 ) != param.size() ) {    
                    throw new RecognitionException("Improper subcell specification in call " + call);
                }
                else {
                    subToken = param.get(slash + 1);
                    numSubCircuitArgs = slash;
                }
            }
            else {
                final int indexOfLast = param.size() - 1;
                //There was no slash so subname is last element of param list.
                subToken = param.get(indexOfLast);
                numSubCircuitArgs = indexOfLast;
            }

            final String subName = subToken.getFqcn(env);

            final HierName args[] = new HierName[numSubCircuitArgs];
            for (int i = 0; i < numSubCircuitArgs; i++) {
                args[i] = toHier((Token) param.get(i), env);
            }

            factory.makeCall(toHier(#call.getToken(), env), subName, args,
                             binding, env);
        }
    ;
