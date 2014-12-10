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

package com.avlsi.fast;

import com.avlsi.fast.BlockIterator;

/**
 * Interface representing f/cast bodies.
 * Example of f/cast bodies are: prs block, decomposition
 * block, csp block, etc.
 *
 * These classes should be immutable, unless you really, *really* know
 * what you are doing.  refineFrom() modifies them, but should only be
 * called during creation of a CellImpl (specifically, during the
 * function setRefinementParent()).  Specifically, refineFrom() should
 * only be called before the cell is used for anything, even as a
 * refinement parent of another cell.
 *
 * @author Aaron Denney
 * @version $Date$
 **/

public interface BlockInterface {
    String ENV = "env";
    String DIRECTIVE = "directive";
    String CELL = "cell";
    String PRS = "prs";
    String NETLIST = "netlist";
    String ASSERT = "assert";
    String SUBCELL = "subcell";
    String VERILOG = "verilog";
    String CSP = "csp";
    String JAVA = "java";

    /**
     * Returns the type of this block.
     **/
    String getType();

    /**
     * Returns a new block that is the merging of this block and the specified
     * block.  The semantics of a merge operation is block specific.
     **/
    BlockInterface merge(BlockInterface o);

    /**
     * Returns a new block that is the replacement of the specified block over
     * this block.  The semantics of a replace operation is block specific.
     **/
    BlockInterface replace(BlockInterface o);

    /**
     * Takes the given block as the refinement parent of this block,
     * and modifies this block accordingly.  <em>Changes this
     * block.</em> See class notes for restrictions on use.
     **/
    void refineFrom(BlockInterface parent);

    /**
     * Returns a BlockIterator of all attached blocks of a specific type.
     **/
    BlockIterator iterator(String type);

    /**
     * Returns a clone of this block.
     **/
    Object clone() throws CloneNotSupportedException;
}
