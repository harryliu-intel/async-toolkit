/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.cast2.directive.impl;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.DirectiveInterfaceFactory;
import com.avlsi.cast2.directive.impl.DirectiveImpl;
import com.avlsi.cast2.util.DirectiveActionInterface;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.util.container.Pair;

// Take advantage of the inability to remove directives.
public class DirectiveDifference implements DirectiveActionInterface,
                                            DirectiveInterfaceFactory {
    private final String block;
    private final Map paramDefinition, nonparamDefinition;
    private boolean add = true;

    public DirectiveDifference(final String block) {
        this.block = block;
        this.paramDefinition = new LinkedHashMap();
        this.nonparamDefinition = new LinkedHashMap();
    }

    public void setMode(final boolean add) {
        this.add = add;
    }

    public void doUnParameterizedDirective(BlockInterface block,
                                           DirectiveBlock db,
                                           String directive,
                                           Object value,
                                           String valueType)
    throws IOException {
        if (add) {
            nonparamDefinition.put(directive, value);
        } else {
            final Object old = nonparamDefinition.get(directive);
            if (old.equals(value)) {
                nonparamDefinition.remove(directive);
            }
        }
    }

    public void doParameterizedDirectiveValue(BlockInterface block,
                                              DirectiveBlock db,
                                              String directive,
                                              Object parameter,
                                              Object value,
                                              String parameterType,
                                              String valueType)
    throws IOException {
        final Pair key = new Pair(directive, parameterType);
        Map m = (Map) paramDefinition.get(key);
        if (add) {
            if (m == null) {
                m = new LinkedHashMap();
                paramDefinition.put(key, m);
            }
            m.put(parameter, value);
        } else {
            final Object old = m.get(parameter);
            if (old.equals(value)) {
                m.remove(parameter);
            }
        }
    }

    public void doParameterizedDirectiveType(BlockInterface block,
                                             DirectiveBlock db,
                                             String directive,
                                             String parameterType,
                                             String valueType)
    throws IOException { }

    public void doBlockInterface(BlockInterface block)
    throws IOException { }

    public DirectiveInterface getDirectiveInterface() {
        return new DirectiveImpl.Impl(block, paramDefinition,
                                      nonparamDefinition);
    }
}
