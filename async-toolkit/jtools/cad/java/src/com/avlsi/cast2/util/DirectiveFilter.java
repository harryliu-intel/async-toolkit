/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
package com.avlsi.cast2.util;

import java.io.IOException;
import java.util.Set;

import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.DirectiveBlock;

/**
 * A utility class containing useful <code>DirectiveActionFilter</code>
 * implementations that filters directives.
 *
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/

public final class DirectiveFilter {
    public static final DirectiveActionFilter NULL = new DirectiveActionFilter()
    {
        public DirectiveActionInterface filter(final DirectiveActionInterface action)
        {
            return new DirectiveActionInterface() {
                public void doUnParameterizedDirective(BlockInterface block,
                                                       DirectiveBlock db,
                                                       String directive,
                                                       Object value,
                                                       String valueType)
                    throws IOException {
                    action.doUnParameterizedDirective(block, db, directive,
                                                      value, valueType);
                }
                public void doParameterizedDirectiveValue(BlockInterface block,
                                                          DirectiveBlock db,
                                                          String directive,
                                                          Object parameter,
                                                          Object value,
                                                          String parameterType,
                                                          String valueType)
                    throws IOException {
                    action.doParameterizedDirectiveValue(block, db, directive,
                                                         parameter, value,
                                                         parameterType,
                                                         valueType);
                }
                public void doParameterizedDirectiveType(BlockInterface block,
                                                         DirectiveBlock db,
                                                         String directive,
                                                         String parameterType,
                                                         String valueType)
                    throws IOException {
                    action.doParameterizedDirectiveType(block, db, directive,
                                                        parameterType,
                                                        valueType);
                }
                public void doBlockInterface(BlockInterface block)
                    throws IOException {
                    action.doBlockInterface(block);
                }
            };
        }
    };

    /**
     * Filtering based on the names of directives.
     **/
    public static class ByDirective implements DirectiveActionFilter {
        private final Set set;
        private final boolean mode;

        /**
         * Constructs a new filter.  The filter has two modes:
         * <ul>
         * <li> include: any directive not in the set is filtered out.
         * <li> exclude: any directive in the set is filtered out.
         * </ul>
         *
         * @param mode true if include mode is desired, false if exclude mode
         * is desired.
         * @param set a set of directives.
         **/
        public ByDirective(final boolean mode, final Set set) {
            this.set = set;
            this.mode = mode;
        }

        public ByDirective getReverse() {
            return new ByDirective(!mode, set);
        }

        public DirectiveActionInterface filter(final DirectiveActionInterface action)
        {
            return new DirectiveActionInterface() {
                public void doUnParameterizedDirective(BlockInterface block,
                                                       DirectiveBlock db,
                                                       String directive,
                                                       Object value,
                                                       String valueType)
                    throws IOException {
                    if (set.contains(directive) == mode) {
                        action.doUnParameterizedDirective(block, db, directive,
                                                          value, valueType);
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
                    if (set.contains(directive) == mode) {
                        action.doParameterizedDirectiveValue(block, db,
                                                             directive,
                                                             parameter, value,
                                                             parameterType,
                                                             valueType);
                    }
                }
                public void doParameterizedDirectiveType(BlockInterface block,
                                                         DirectiveBlock db,
                                                         String directive,
                                                         String parameterType,
                                                         String valueType)
                    throws IOException {
                    if (set.contains(directive) == mode) {
                        action.doParameterizedDirectiveType(block, db,
                                                            directive,
                                                            parameterType,
                                                            valueType);
                    }
                }
                public void doBlockInterface(BlockInterface block)
                    throws IOException {
                    action.doBlockInterface(block);
                }
            };
        }
    }
}
