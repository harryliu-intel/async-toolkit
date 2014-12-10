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
 
package com.avlsi.tools.cdl2aspice;

import java.io.FileOutputStream;
import java.util.Map;
import java.util.TreeMap;

import com.avlsi.file.aspice.AspiceCell;
import com.avlsi.file.spice.SpiceParser;

/**
 * Converts a .cdl file to a .aspice file.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class CDL2Aspice {
    private static void usage(final int exitStatus) {
        System.err.println("Usage: cdl2aspice " + 
                "( --res res_type rho )*" + 
                "file.cdl cell_name out_file");
        System.exit(exitStatus);
    }

    public static void main(final String[] args) {
        if (args.length < 3)
            usage(1);

        final Map resMap = new TreeMap();
	
        int i = 0;
        for (i = 0; i + 3 < args.length && args[i].equals("--res"); i += 3) {
            final String resType = args[i + 1];
            final String rho = args[i + 2];

            try {
                final Double d =
                    (Double) resMap.put(resType, new Double(rho));
                if (d != null) {
                    System.err.println("Duplicate rho for " + resType +
                            ".  Old value: " + d + ", new value: " + rho);
                    System.exit(2);
                }
            } catch (NumberFormatException e) {
                System.err.println("Bad double format for rho of " +
                        resType + ": " + rho);
                System.exit(3);
            }
        }

        final String cdlFileName = args[i];
        final String cdlCellName = args[i + 1];
        final String outFile = args[i + 2];
        AspiceCell.Repository repos = new AspiceCell.Repository();
	
        try {
            SpiceParser.ParsingCallback callback = null;
            final SpiceParser spiceParser = 
                new SpiceParser('.', resMap, callback);
            spiceParser.parseFile(cdlFileName, repos);
            final AspiceCell aspiceCell = 
                (AspiceCell) repos.getCell(cdlCellName);
	    
            if (aspiceCell == null) {
                System.err.println("Couldn't find cell " + cdlCellName);
                System.exit(2);
            }
	    
            aspiceCell.printToStream(new FileOutputStream(outFile), repos);
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println(e + ": " + e.getMessage());
            System.exit(1);
        }
    }
}
