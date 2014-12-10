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

package com.avlsi.fast;

import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.impl.CastParserEnvironment;
import com.avlsi.cast.impl.ChainEnvironment;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.EnvironmentEntry;
import com.avlsi.cast.impl.EnvironmentEntryIterator;
import com.avlsi.cast.impl.NullEnvironment;
import com.avlsi.cast.impl.SemanticWrapperException;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.UserDefinedValue;
import com.avlsi.cast.impl.Value;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.util.debug.Debug;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.List;

/**
 * Interface to an env block.  Currently only useful for getting
 * the named environments from the env block.
 **/

public class EnvBlock extends BlockCommon {
    /**
     * An environment containing definitions of environment cells as well as
     * any bindings before that point.  It is set when the environment block of
     * a cell is parsed.
     *
     * @see com.avlsi.cast.impl.UserDefinedValue
     **/
    private Environment env;

    /**
     * An environment containing definitions inside the environment block. It
     * is set when the environment block of a cell is parsed.
     *
     * @see com.avlsi.cast.impl.UserDefinedValue
     **/
    private Environment blockEnv;

    /**
     * The current parser environment associated with this environment block.
     * This is necessary so we can use the correct CastParsingOptions when we
     * actually tree parse the AST associated with an environment block.
     * Keeping a reference to it shouldn't create any problems with GC.
     **/
    private CastParserEnvironment parserEnv;

    /**
     * The cell containing this EnvBlock.
     **/
    private final CellInterface parent;

    public EnvBlock(final CellInterface parent) {
        env = NullEnvironment.getInstance();
        blockEnv = NullEnvironment.getInstance();
        this.parent = parent;
    }

    public void setEnvironment(final Environment env,
                               final Environment blockEnv,
                               final CastParserEnvironment parserEnv) {
        this.env = env;
        this.blockEnv = blockEnv;
        this.parserEnv = parserEnv;
    }

    public Environment getEnvironment() {
        return env;
    }

    /**
     * Returns an iterator over the names of all environments that do not have
     * metaparameters.  Automated testing programs use this function to iterate
     * over environments to test.  Environments with metaparameters are not
     * tested, because metaparameters cannot be choosen automatically.
     **/
    public Iterator/*<String>*/ getNames() {
        final List/*<String>*/ result = new ArrayList();
        for (EnvironmentEntryIterator i = blockEnv.entryIterator();
             i.hasNext(); ) {
            final EnvironmentEntry entry = i.next();
            final Value val = entry.getValue();
            if (val instanceof UserDefinedValue &&
                !((UserDefinedValue) val).hasMetaParams()) {
                result.add(entry.getName().getString());
            }
        }
        return result.iterator();
    }

    /**
     * Returns an environment by that name, or null if it cannot be
     * instantiated.  <code>name</code> may contain metaparameters.
     **/
    public CellInterface getNamedEnvironment(final String name)
    throws CastSemanticException {
        CellInterface result = null;

        // check to make sure name is an environment defined locally
        if (parserEnv != null &&
            blockEnv.contains(Symbol.create(CellUtils.getBaseType(name)))) {
            try {
                result = parserEnv.getCell(env, name, null, null, parent);
            } catch (SemanticWrapperException e) {
                throw new CastSemanticException(e.getCause(), e.getFilename(),
                                                e.getLine(), e.getColumn());
            }
        } else {
            // automatically generated environments
            if (name.equals("leakage")) {
                result = CellUtils.getLeakageEnv(parent);
            }
        }
        return result;
    }

    /**
     * Environments add, except that in name clashes the refined one
     * is used.
     **/
    public void refineFrom(final BlockInterface o) {
        super.refineFrom(o);
        final EnvBlock parent = (EnvBlock) o;
        this.env = new ChainEnvironment(parent.env, this.env);
        this.blockEnv = new ChainEnvironment(parent.blockEnv, this.blockEnv);
        if (this.parserEnv == null) this.parserEnv = parent.parserEnv;
    }
    
    public String getType() {
        return BlockInterface.ENV;
    }

    public BlockInterface merge(BlockInterface o) {
        Debug.assertTrue(false, "EnvBlock doesn't support all BlockInterface functionality");
        return null;
    }
    public BlockInterface replace(BlockInterface o) {
        Debug.assertTrue(false, "EnvBlock doesn't support all BlockInterface functionality");
        return null;
    }

    public String toString() {
        return "EnvBlock " + env;
    }
}
