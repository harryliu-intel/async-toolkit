// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.output;

/*
NodeExpression
|-- AliasNodeExpression
|-- ArbitraryNodeExpression
`-- ShorthandNodeExpression
    |-- And2C2Expression
    |-- CElementExpression
    |-- NandExpression
    |-- NorExpression
    `-- TwoAnd2C2Expression
*/

public interface NodeExpression {
    LineAndSection[] evaluate(NodeName dest, Direction preferredDirection,
                              Section upSection, Section downSection);
    NodeExpression unparameterize(int index);
    NodeExpression canonicalize(NodeCanonicalizer c);
    Simplification getSimplification();
    NodeExpression applySimplification(Simplification simp);
    /* note: classes that implement this interface also need to
     * implement the equals() and hashCode() methods correctly. */
}
