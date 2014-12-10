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

package com.avlsi.csp.csp2java.runtime;

import java.lang.reflect.Constructor;

/**
 * A class for runtime logged array support of any number of dimensions.  The
 * object being stored inside the array should be another logged data type.
 * 
 * @author Harry Liu
 * @version $Revision$ $Date$
 **/
public class LoggedCspArray extends CspArray {
    public interface ElementConstructor {
        CspValue create(CspRuntimeAbstractDevice parent, String varName);
    }

    private static class ReflectionConstructor implements ElementConstructor {
        private final Class elem;
        public ReflectionConstructor(final Class elem) {
            this.elem = elem;
        }
        public CspValue create(CspRuntimeAbstractDevice parent,
                               String varName) {
            try {
                final Constructor ctor =
                    elem.getConstructor(
                        new Class[] { parent.getClass(),
                                      String.class } );
                return (CspValue) ctor.newInstance(
                    new Object[] { parent, varName });
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Class constructor.
     *
     * @param varName name of the array
     * @param min lower bound of the array
     * @param max upper bound of the array
     * @param ec an ElementConstructor used to construct elements stored inside
     * the array
     **/
    public LoggedCspArray (final CspRuntimeAbstractDevice parent,
                           final String varName, final CspInteger min,
                           final CspInteger max, final ElementConstructor ec) {
        super(min.intValue(), max.intValue(),
              getArray(parent, varName, min.intValue(), max.intValue(), ec));
    }

    private static CspValue[] getArray(final CspRuntimeAbstractDevice parent,
                                       final String varName,
                                       final int min,
                                       final int max,
                                       final ElementConstructor ec) {
        final CspValue[] ca = new CspValue[max - min + 1];
        for (int i = 0; i < ca.length; ++i)
            ca[i] = ec.create(parent, varName + "[" + (i + min) + "]" );
        return ca;
    }

    private static CspArray makeArray(final CspRuntimeAbstractDevice parent,
                                      final String varName,
                                      final CspInteger[] limits,
                                      final int start,
                                      final ElementConstructor ec) {
        if (limits.length - start == 2) {
            return new LoggedCspArray(parent, varName, limits[start],
                                      limits[start + 1], ec);
        } else {
            final int min = limits[start].intValue();
            final int max = limits[start + 1].intValue();
            final int len = max - min + 1;
            final CspValue[] content = new CspValue[len];
            for (int i = 0; i < len; ++i) {
                content[i] = makeArray(parent, varName + "[" + (i + min) + "]",
                                       limits, start + 2, ec);
            }
            return new CspArray(min, max, content);
        }
    }

    /**
     * Construct a possibly multi-dimensional array.  If the array is
     * multidimensional, the result is a <code>CspArray</code> that contains
     * <code>CspArray</code>s of one less dimension.
     *
     * @param parent the csp device that contains this array
     *
     * @param varName name of the array
     *
     * @param limits bounds of the dimensions of the array;
     * <code>limits[0]</code> and <code>limits[1]</code> are the lower and
     * upper bound of the first dimension respectively, <code>limits[2]</code>
     * and <code>limits[3]</code> are the lower and upper bound of the second
     * dimension, and so on.  Therefore the length of <var>limits</var> must be
     * even.
     * @param ec an ElementConstructor used to construct elements stored inside
     * the array
     *
     * @return a CspArray with the specified dimensions, with all elements
     * initialized using the default constructor of <var>elem</var>
     **/
    public static CspArray makeArray(final CspRuntimeAbstractDevice parent,
                                     final String varName,
                                     final CspInteger[] limits,
                                     final ElementConstructor ec) {
        // assert limits.length % 2 == 0;
        return makeArray(parent, varName, limits, 0, ec);
    }

    public static CspArray makeArray(final CspRuntimeAbstractDevice parent,
                                     final String varName,
                                     final CspInteger[] limits,
                                     final Class elem) {
        return makeArray(parent, varName, limits,
                         new ReflectionConstructor(elem));
    }

    public static CspArray makeArray(final CspRuntimeAbstractDevice parent,
                                     final String varName,
                                     final String parsePos,
                                     final CspInteger[] limits,
                                     final Class elem) {
        return makeArray(parent, varName, parsePos, limits,
                         new ReflectionConstructor(elem));
    }

    public static CspArray makeArray(final CspRuntimeAbstractDevice parent,
                                     final String varName,
                                     final String parsePos,
                                     final CspInteger[] limits,
                                     final ElementConstructor ec) {
        final CspArray result = makeArray(parent, varName, limits, ec);
        parent.topFrame().addVariable(varName, parsePos, result);
        return result;
    }
}
