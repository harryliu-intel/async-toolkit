header {
    ///////////////////////////////////////////////////////////////////////
    //
    // Copyright 2001 Fulcrum Microsystems.  All rights reserved.
    //
    // Warning:  This file was AUTOMATICALLY GENERATED!!!
    // 
    // DO NOT check in.
    // DO NOT modify.
    //
    // You want to modify CastQuery.g instead
    //
    ///////////////////////////////////////////////////////////////////////

    package com.avlsi.tools.jauto;
}

{
    import java.util.ArrayList;
    import java.util.List;
    import java.util.Iterator;
    import com.avlsi.util.functions.UnaryPredicate;
    import com.avlsi.util.functions.BinaryFunction;
}

class CastQueryParser extends Parser;
options {
    k = 1;
}
{
    public interface QueryConstructor {
        UnaryPredicate getClause(String test, String op, String arg);
    }

    /**
     * Returns true if all sub-predicates returns true.  Evaluation is short
     * circuited if a sub-predicate returns false.
     **/
    private static class AndUnaryPredicates implements UnaryPredicate {
        private List predicates;
        public AndUnaryPredicates() {
            this.predicates = new ArrayList();
        }
        public void addPredicate(final UnaryPredicate p) {
            predicates.add(p);
        }
        public boolean evaluate(final Object o) {
            for (Iterator i = predicates.iterator(); i.hasNext(); ) {
                final UnaryPredicate p = (UnaryPredicate) i.next();
                if (!p.evaluate(o)) return false;
            }
            return true;
        }
    }

    /**
     * Returns true if any sub-predicates returns true.  Evaluation is short
     * circuited if a sub-predicate returns true.
     **/
    private static class OrUnaryPredicates implements UnaryPredicate {
        private List predicates;
        public OrUnaryPredicates() {
            this.predicates = new ArrayList();
        }
        public void addPredicate(final UnaryPredicate p) {
            predicates.add(p);
        }
        public boolean evaluate(final Object o) {
            for (Iterator i = predicates.iterator(); i.hasNext(); ) {
                final UnaryPredicate p = (UnaryPredicate) i.next();
                if (p.evaluate(o)) return true;
            }
            return false;
        }
    }

    /**
     * Returns the logical opposite of the sub-predicate.
     **/
    private static class NotUnaryPredicate implements UnaryPredicate {
        private UnaryPredicate predicate;
        public NotUnaryPredicate(final UnaryPredicate predicate) {
            this.predicate = predicate;
        }
        public boolean evaluate(final Object o) {
            return !predicate.evaluate(o);
        }
    }
}

goal [ QueryConstructor qc ] returns [ UnaryPredicate p ]
    {
        p = null;
    }
    :   p = orExpr[qc] EOF
    ;

orExpr [ QueryConstructor qc ] returns [ OrUnaryPredicates or ]
    {
        UnaryPredicate p;
        or = new OrUnaryPredicates();
    }
    :   p = andExpr[qc] {
            or.addPredicate(p);
        }
        ( OR p = andExpr[qc] {
            or.addPredicate(p);
        })*
    ;

andExpr [ QueryConstructor qc ] returns [ AndUnaryPredicates and ]
    {
        UnaryPredicate p;
        and = new AndUnaryPredicates();
    }
    :   p = unaryExpr[qc] {
            and.addPredicate(p);
        } ( AND p = unaryExpr[qc] {
            and.addPredicate(p);
        })*
    ;

unaryExpr [ QueryConstructor qc ] returns [ UnaryPredicate unary ]
    {
        UnaryPredicate p;
        unary = null;
    }
    :   NOT p = unaryExpr[qc] {
            unary = new NotUnaryPredicate(p);
        }
    |   unary = primaryExpr[qc]
    ;

primaryExpr [ QueryConstructor qc ] returns [ UnaryPredicate primary ]
    {
        primary = null;
        String op = null;
        String val = null;
    }
    :   LPAREN primary = orExpr[qc] RPAREN
    |   test:TEXT ( oper:OPER arg:TEXT {
            op = oper.getText();
            val = arg.getText();
        } )? {
            primary = qc.getClause(test.getText(), op, val);
        }
    ;

class CastQueryLexer extends Lexer;
options {
    k = 2;
}

AND     : "&";
OR      : "|";
LPAREN  : "(";
RPAREN  : ")";
NOT     : "!";
protected
LETTER  : 'a'..'z' | 'A'..'Z'
        ;
protected
DIGIT   : '0'..'9'
        ;
protected
CHAR    : LETTER | DIGIT | '_' | '+' | '-' | '*' | '/' | '.' | '{' | '}' | ',' | ':' ;
TEXT    : CHAR (CHAR | '(' | ')')+;
WS      :   (   ' '
            |   '\t'
            |   '\r' '\n' { newline(); }
            |   '\n'      { newline(); }
            )
            { $setType(Token.SKIP); }
        ;
OPER    :   "<" | "<=" | "==" | ">=" | ">" | "="
        ;
