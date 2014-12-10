/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.ast;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * Class for variable declarations.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class Declaration 
    extends AbstractASTNode {
    private final DeclaratorList declaratorList;

    public Declaration (final IdentifierList identifierList, 
            final Type type) {
        this.declaratorList = new DeclaratorList();
        for (Iterator i = identifierList.getIdentifiers(); i.hasNext(); ) {
            IdentifierExpression ident = (IdentifierExpression) i.next();
            declaratorList.addDeclarator(new Declarator(ident, type, null));
        }
    }

    public Declaration (final DeclaratorList declaratorList,
            final Type type) {
        this.declaratorList = declaratorList;
        for (Iterator i = declaratorList.getDeclarators(); i.hasNext(); ) {
            Declarator d = (Declarator) i.next();
            ArrayType at = (ArrayType) d.getTypeFragment();

            /* Complete the type fragment */

            // VariableAnalyzer requires each Type instance corresponds to only
            // one variable, so we clone the type to make each declarator have
            // an unique instance
            final Type cloned;
            try {
                cloned = (Type) type.clone();
            } catch (CloneNotSupportedException e) {
                throw new AssertionError("Cannot clone: " + type);
            }
            if (at == null) {
                d.setTypeFragment(cloned);
            } else {
                while (at.getElementType() != null) {
                    at = (ArrayType) at.getElementType();
                }
                at.setElementType(cloned);
            }
        }
    }

    public Declaration (final DeclaratorList declaratorList) {
        this.declaratorList = declaratorList;
    }

    public DeclaratorList getDeclaratorList () {
        return declaratorList;
    }

    /** 
     * Convenience routine to return the set of declared identifiers.  Each
     * element is an object of type java.lang.String.
     **/
    public Set getIdentifierSet() {
        Set result = new HashSet();
        for (Iterator i = declaratorList.getDeclarators(); i.hasNext(); ) {
            Declarator declarator = (Declarator) i.next();
            result.add (declarator.getIdentifier().getIdentifier());
        }

        return result;
    }

    /** 
     * Convenience routine to return a mapping of identifiers to types.  Each
     * identifier is an object of type java.lang.String.  Types are
     * subclasses of ast.Type.
     **/
    public Map<String,Type> getMap() {
        Map<String,Type> result = new HashMap<String,Type>();
        for (Iterator i = declaratorList.getDeclarators(); i.hasNext(); ) {
            Declarator declarator = (Declarator) i.next();
            result.put (declarator.getIdentifier().getIdentifier(),
                        declarator.getTypeFragment());
        }

        return result;
    }
}
