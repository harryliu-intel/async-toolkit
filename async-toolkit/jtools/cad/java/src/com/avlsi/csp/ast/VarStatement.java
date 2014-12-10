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

/**
 * Class for variable declaration block statements.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class VarStatement
    extends AbstractASTNode
    implements StatementInterface {
    private final DeclarationList declarationList;
    private final StatementInterface statement;

    public VarStatement (final DeclarationList declarationList,
            StatementInterface statement) {
        this.declarationList = declarationList;
        this.statement = statement;
    }

    /**
     * Support for new-style variable declarations, which occur as part of
     * a sequential statement.
     **/
    public VarStatement (final Declaration declaration) {
        this.declarationList = new DeclarationList();
        this.declarationList.addDeclaration (declaration);
        this.statement = null;
    }

    public DeclarationList getDeclarationList () {
        return declarationList;
    }

    public StatementInterface getStatement () {
        return statement;
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitVarStatement(this);
    }
}
