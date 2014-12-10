/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cast2.directive.impl;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.file.common.HierName;
import com.avlsi.util.container.Pair;

/**
 * API used to convert a value type to a string representation.
 **/
public class CastEmitter implements DirectiveEmitter {
    private static CastEmitter singleton = null;

    private CastEmitter() {
    }

    public String emit(final String block, final String type,
                       final Object value) {
        if (type.equals(DirectiveConstants.INT_TYPE) ||
            type.equals(DirectiveConstants.FLOAT_TYPE) ||
            type.equals(DirectiveConstants.DOUBLE_TYPE) ||
            type.equals(DirectiveConstants.BOOLEAN_TYPE) ||
            type.equals(DirectiveConstants.WIDE_CHANNEL_TYPE) ||
            type.equals(DirectiveConstants.POSSIBLY_WIDE_CHANNEL_TYPE)) {
            return value.toString();
        } else if (type.equals(DirectiveConstants.STRING_TYPE)) {
            // XXX: escape single quotes inside the string?
            return "'" + (String) value + "'";
        } else if (type.equals(DirectiveConstants.NODE_TYPE) ||
                   type.equals(DirectiveConstants.CHANNEL_TYPE) ||
                   type.equals(DirectiveConstants.INSTANCE_TYPE)) {
            return ((HierName) value).getCadenceString();
        } else if (type.equals(DirectiveConstants.HALFOP_TYPE)) {
            final Pair p = (Pair) value;
            final String node = ((HierName) p.getFirst()).getCadenceString();
            final Boolean up = (Boolean) p.getSecond();
            if (up == null) {
                return node;
            } else {
                return node + (up.booleanValue() ? "+" : "-");
            }
        } else if (type.equals(DirectiveConstants.LAYER_TYPE)) {
            final Pair p = (Pair) value;
            return "( '" + p.getFirst() + "' '" + p.getSecond() + "' )";
        } else {
            throw new RuntimeException("Do not know how to handle type: " + type);
        }
    }

    public static CastEmitter getInstance() {
        if (singleton == null) singleton = new CastEmitter();
        return singleton;
    }
}
