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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.HashSet;

/**
 * Class representing a csp program body and function definitions.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public class CSPProgram 
    extends AbstractASTNode {

    /**
     * List of {@link FunctionDeclaration}s, not null.
     **/
    private final List functionDeclList;

    /**
     * Map from structure name to {@link StructureDeclaration}s, not null.
     **/
    private final Map structureDeclMap;

    /**
     * List of {@link CSPProgram}s, not null.
     **/
    private final List refinementParents;

    /**
     * Set of {@link CSPProgram}s that CSP statements (as opposed to
     * declarations) should not be inherited from
     **/
    private final Set declarationParents;

    /**
     * Initializer statement.  May be null.
     **/
    private SequentialStatement initStmt;

    /**
     * Body statement.  May be null.
     **/
    private StatementInterface stmt;

    /**
     * Class constructor.  Creates a CSP program with an empty list of
     * function declarations, and no body statement.
     *
     **/
    public CSPProgram() {
        this.functionDeclList = new ArrayList();
        this.structureDeclMap = new LinkedHashMap();
        this.refinementParents = new ArrayList();
        this.declarationParents = new HashSet();
        this.initStmt = null;
        this.stmt = null;
    }

    /**
     * Returns the initializer statement.
     *
     * @return initializer statement, may be null
     **/
    public SequentialStatement getInitializerStatement() {
        return initStmt;
    }

    /**
     * Sets the initializer statement.
     *
     * @param stmt  initializer statement, not null
     **/
    public void setInitializerStatement(final SequentialStatement initStmt) {
        this.initStmt = initStmt;
    }

    /**
     * Returns the body statement.
     *
     * @return body statement, may be null
     **/
    public StatementInterface getStatement() {
        return stmt;
    }

    /**
     * Sets the body statement.
     *
     * @param stmt  body statement, not null
     **/
    public void setStatement(final StatementInterface stmt) {
        this.stmt = stmt;
    }

    /**
     * Append a function declaration to the list of function declarations.
     *
     * @param funcDecl  function declaration to append, not null
     **/
    public void addFunctionDeclaration(final FunctionDeclaration funcDecl) {
        functionDeclList.add(funcDecl);
    }

    /**
     * Returns an iterator of the declared functions
     *
     * @return an Iterator of {@link FunctionDeclaration}s over the
     *     function declarations that have been added.  Will not return
     *     null.
     **/
    public Iterator getFunctionDeclarations() {
        return functionDeclList.iterator();
    }

    /**
     * Append a structure declaration to the list of structure declarations.
     *
     * @param structDecl  structure declaration to append, not null
     **/
    public void addStructureDeclaration(final StructureDeclaration structDecl) {
        structureDeclMap.put(structDecl.getName(), structDecl);
    }

    /**
     * Returns an iterator of the declared structures.
     *
     * @return an Iterator of {@link StructureDeclaration}s over the
     *     structure declarations that have been added.  Will not return
     *     null.
     **/
    public Iterator getStructureIterator() {
        return structureDeclMap.values().iterator();
    }

    /**
     * Returns a map of structure names to structure declarations.
     *
     * @return a map from <code>String</code> to {@link StructureDeclaration}s
     **/
    public Map getStructureDeclarations() {
        return Collections.unmodifiableMap(structureDeclMap);
    }

    /**
     * Return the structure declaration corresponding to a name.
     *
     * @return a {@link StructureDeclaration} associated with the name, or
     * <code>null</code> if there is no such declaration.
     **/
    public StructureDeclaration getStructureDeclaration(final String name) {
        return (StructureDeclaration) structureDeclMap.get(name);
    }

    /**
     * Accepts a visitor, calling the appropriate visit method on it.
     **/
    public void accept(VisitorInterface v) throws VisitorException {
        v.visitCSPProgram(this);
    }

    /**
     * Record the ancestry for possible declaration and statement inheritence
     * later.
     **/
    public void refineFrom(final CSPProgram parent) {
        if (parent == null) return;
        refinementParents.add(parent);
    }

    /**
     * Like <code>refineFrom</code>, except only function and structure
     * declarations are inherited from the parent, not the statements.
     **/
    public void refineDeclarationsFrom(final CSPProgram parent) {
        if (parent == null) return;
        refineFrom(parent);
        declarationParents.add(parent);
    }

    /**
     * Should only the declaration but not the statement body of the specified
     * program be inherited?
     **/
    public boolean inheritDeclarationOnly(final CSPProgram p) {
        return declarationParents.contains(p);
    }

    /**
     * Return a list of {@link CSPProgram}s that are ancestors.
     **/
    public List getRefinementParents() {
        return Collections.unmodifiableList(refinementParents);
    }

    /** For debugging purposes. **/
    public String toString() {
        return "functions: " + functionDeclList + " structures: " +
               structureDeclMap + " statement: " + stmt;
    }
}
