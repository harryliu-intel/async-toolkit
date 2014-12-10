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

package com.avlsi.tools.lvs;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.avlsi.util.text.StringUtil;

/**
 * This is a description of the class
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
final class SubtypeInfo {
    public static final class SubtypeInfoFileFormatException
        extends Exception {
        SubtypeInfoFileFormatException(final String message) {
            super(message);
        }
    }

    private final Map subtypeMap;

    public SubtypeInfo() {
        subtypeMap = new HashMap();
    }

    public void parseSubtypes(final String fileName)
        throws IOException, SubtypeInfoFileFormatException {
        final BufferedReader reader =
            new BufferedReader(new InputStreamReader(
                        new FileInputStream(fileName)));
        String line;

        while ((line = reader.readLine()) != null) {
            final String[] s = StringUtil.split(line, ':');
            
            if (s.length != 2)
                throw new SubtypeInfoFileFormatException("Expected 1 :, " +
                        "got" + (s.length - 1) + " in " + line);

            final String baseType = s[0];
            final String subType  = s[1];

            final String baseBase = (String) subtypeMap.get(baseType);

            if (baseBase != null)
                throw new SubtypeInfoFileFormatException("Basetype " +
                        baseType + " is a subtype of " + baseBase +
                        "; not allowed.");

            final String subBase = (String) subtypeMap.get(subType);

            if (subBase != null)
                throw new SubtypeInfoFileFormatException("Subtype " +
                        subType + " is a already a subtype of " + subBase);

            subtypeMap.put(subType, baseType);
        }
    }

    public String getBasetype(final String subType) {
        String nextType;
        String baseType = subType;
        while ((nextType = (String) subtypeMap.get(baseType)) != null)
            baseType = nextType;
        return baseType;
    }
}
