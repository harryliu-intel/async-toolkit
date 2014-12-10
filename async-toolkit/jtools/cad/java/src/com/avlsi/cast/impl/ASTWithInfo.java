/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast.impl;

import antlr.CommonAST;
import antlr.Token;
import antlr.collections.AST;

/**
 * AST with filename, line number, and column number.
 * Used to give decent error messages in the tree parser.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class ASTWithInfo extends CommonAST {
    private int line = 0;
    private int column = 0;
    private String filename = null;

    public void initialize(Token tok) {
        super.initialize(tok);
        setLine(tok.getLine());
        setColumn(tok.getColumn());
        setFilename(((TokenWithInfo) tok).getFilename());
    }

    public void initialize(final AST ast) {
        super.initialize(ast);
        copyInfo((ASTWithInfo) ast);
    }

    public int getLine(){
        return line;
    }

    public void setLine(final int line) {
        this.line = line;
    }

    public String getFilename() {
        return filename;
    }

    public void setFilename(final String filename) {
        this.filename = filename;
    }
    
    public int getColumn() {
        return column;
    }

    public void setColumn(final int column) {
        this.column = column;
    }

    /**
     * Copies file/line/column info from ast into this one,
     * overwriting whatever was set in this one.
     **/
    public void copyInfo(final ASTWithInfo ast) {
        setLine(ast.getLine());
        setColumn(ast.getColumn());
        setFilename(ast.getFilename());
    }

    public String toString() {
        return "ASTWithInfo(line=" + getLine()
            + ", column=" + getColumn()
            + ", file=" + getFilename()
            + ", type=" + getType()
            + ", super=" + super.toString() + ")";
    }

    /**
     * Returns a copy of this tree with all the info, but with no
     * children or siblings.
     **/
    public ASTWithInfo getRoot() {
        ASTWithInfo result = new ASTWithInfo();
        result.initialize(this);
        result.removeChildren();
        result.setNextSibling(null);
        return result;
    }
}
