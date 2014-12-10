/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.ast;

import antlr.Token;
import com.avlsi.util.debug.Debug;
import com.avlsi.csp.grammar.ParsePosition;
import com.avlsi.csp.grammar.ParseRange;

/**
 * This class is the abstract superclass of all objects representing
 * AST nodes. The only functionality it currently provides are methods
 * to retrieve and manipulate parse range data for the node. 
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public abstract class AbstractASTNode 
    implements AbstractASTNodeInterface {
    private ParseRange parseRange=ParseRange.EMPTY;
    private Exception whereInstantiated=null;

    /* IMPORTANT: Change to false before submitting: */
    static final boolean debugParseRange=false;

    public AbstractASTNode() {
        if(debugParseRange) {
            whereInstantiated = new Exception();
        }
    }

    // all the following methods are documented in
    // AbstractASTNodeInterface
    public void checkParseRange() {
        if(debugParseRange) {
            if(parseRange==ParseRange.EMPTY) {
                System.err.println("In AST node "+this.getClass()+
                                   ": found null parseRange");
                System.err.println("Instantiated at: ");
                whereInstantiated.printStackTrace();
                throw new Error("null parseRange");
            }
        }
    }
    
    public ParseRange getParseRange() {
        checkParseRange();
        return parseRange;
    }

    public AbstractASTNode epr(ParseRange p) {
        if(parseRange == ParseRange.EMPTY) {
            if(debugParseRange)
                Debug.assertTrue(p!=null);
            parseRange = p;
        } else {
            parseRange = new ParseRange(parseRange, p);
        }
        return this;
    }

    public AbstractASTNode epr(Token t) {
        if(t!=null)
            epr(new ParseRange(t));
        return this;
    }

    public AbstractASTNode epr(AbstractASTNodeInterface x) {
        if(x!=null) {
            ((AbstractASTNode)x).checkParseRange();
            epr(((AbstractASTNode)x).getParseRange());
        }
        return this;
    }

    public AbstractASTNode epr(AbstractASTNodeInterface x, 
                               AbstractASTNodeInterface y) {
        epr(x);
        return epr(y);
    }

    public AbstractASTNode epr(Token t, Token s) {
        epr(t);
        return epr(s);
    }

    public AbstractASTNode epr(Token t, AbstractASTNodeInterface x) {
        epr(t); 
        return epr(x);
    }

    public AbstractASTNode epr(AbstractASTNodeInterface x, Token t) {
        epr(x);
        return epr(t);
    }

// never used:
//     /* expressions of a[] should be in forward order */
//     public AbstractASTNode epr(AbstractASTNodeInterface a[]) {
//         Debug.assertTrue(a.length>0, 
//                      "epr(AbstractASTNodeInterface[]) called with zero-length array");
//         /* simple sanity check for a[] ordering */
//         Debug.assertTrue(((AbstractASTNode)a[0]).parseRange.end.
//                      compareTo(((AbstractASTNode)a[a.length-1]).parseRange.start)<=0);
//         epr(new ParseRange(((AbstractASTNode)a[0]).parseRange,
//                            ((AbstractASTNode)a[a.length-1]).parseRange));
//         return this;
//     }
}
