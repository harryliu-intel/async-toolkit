/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.DirectiveInterface;
import com.avlsi.cast2.directive.DirectiveInterfaceFactory;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.cast2.directive.impl.DirectiveVisitor;
import com.avlsi.cast2.directive.impl.DirectiveTable;
import com.avlsi.cast2.directive.impl.TokenInfo;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.Triplet;

public class DirectiveSource implements DirectiveInterfaceFactory {
    private String block;
    private Map paramDefinition, noparamDefinition;

    public DirectiveSource(String block) {
        this.block = block;
        paramDefinition = new HashMap();
        noparamDefinition = new HashMap();
    }

    public void definition(String key, Object val) {
        noparamDefinition.put(key, val);
    }

    public void definition(String key, String memberType, Object param,
                           Object value) {
        Map m = (Map) paramDefinition.get(new Pair(key, memberType));
        if (m == null) {
            m = new HashMap();
            paramDefinition.put(new Pair(key, memberType), m);
        }
        m.put(param, value);
    }

    public DirectiveInterface getDirectiveInterface() {
        return new DirectiveImpl.Impl(block, paramDefinition, noparamDefinition);
    }

    public int hashCode() {
        return paramDefinition.hashCode() + noparamDefinition.hashCode();
    }

    public boolean equal(final Object o) {
        if (o instanceof DirectiveSource) {
            final DirectiveSource s = (DirectiveSource) o;
            return block.equals(s.block) &&
                   paramDefinition.equals(s.paramDefinition) &&
                   noparamDefinition.equals(s.noparamDefinition);
        } else {
            return false;
        }
    }

    public String toString() {
        return "Non-parameterized: " + noparamDefinition.toString() + "\n" + "Parameterized: " + paramDefinition.toString();
    }
}
