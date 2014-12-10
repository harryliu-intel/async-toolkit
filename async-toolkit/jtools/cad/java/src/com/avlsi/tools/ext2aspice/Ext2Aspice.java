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

package com.avlsi.tools.ext2aspice;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.util.Iterator;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;


import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.aspice.Diode;
import com.avlsi.file.aspice.Transistor;
import com.avlsi.file.common.Capacitor;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.ext.ExtCell;
import com.avlsi.file.ext.FET;
import com.avlsi.file.ext.Node;
import com.avlsi.file.ext.Terminal;
import com.avlsi.file.ext.parse.ExtParser;
import com.avlsi.io.FileSearchPath;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.text.StringUtil;


import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

/**
 * This class converts produces a .aspice file from a .ext file
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class Ext2Aspice {
    /**
     * ExtCell being converted.
     **/
    private final ExtCell ext;

    /**
     * AspiceFile being constructed from ExtCell.
     **/
    private final AspiceFile aspiceFile;

    /**
     * The global repository for subcell definitions.
     **/
    private final Map subcellNameToDefnMap;

    /**
     * Static method to convert an ExtCell into an AspiceFile
     * @design jmr XXX deprecate this?
     **/
    public static AspiceFile ext2aspice(final ExtCell ext) {
        return new Ext2Aspice(ext).convert();
    }

    /**
     * Static method to convert an ExtCell into an AspiceFile
     **/
    public static AspiceFile ext2aspice(final ExtCell ext,
                                        final Map subcellNameToDefnMap)
    {
        return new Ext2Aspice(ext, subcellNameToDefnMap).convert();
    }

    /**
     * Class constructor.  Initializes a blank AspiceFile with the 
     * correct name.
     * @design jmr XXX deprecate this?
     **/
    private Ext2Aspice(final ExtCell ext) {
        this(ext, new TreeMap());
    }

    /**
     * Class constructor.  Initializes a blank AspiceFile with the 
     * correct name.
     **/
    private Ext2Aspice(final ExtCell ext, final Map subcellNameToDefnMap) {
        this.ext = ext;
        aspiceFile = new AspiceFile(ext.getName(), subcellNameToDefnMap);
        this.subcellNameToDefnMap = subcellNameToDefnMap;
    }

    /**
     * If nodeName is already in diodeNodeToWidthMap, adds width
     * the the value in the map; if it is not there, put it in.
     *
     * @param diodeNodeToWidthMap  map of String to double[], mapping
     *     node names of diodes to their width.  The given width will
     *     be added to nodeName's entry in this map.  This use of
     *     double[] is because of Java's lack of ref cells.
     * @param nodeName  the name of the diode node
     * @param width  the width of nodeName
     *
     * @see #convert
     **/
    private void addDiodeWidth(final Map diodeNodeToWidthMap,
                               final HierName nodeName,
                               final double width)
    {
        final double[] d = (double[]) diodeNodeToWidthMap.get(nodeName);

        if (d == null)
            diodeNodeToWidthMap.put(nodeName, new double[]{width});
        else
            d[0] += width;
    }

    /**
     * If nodeName is in diodeNodeToWidthMap, returns it width, otherwise
     * returns 0.0.
     * 
     * @see #convert
     **/
    private double getDiodeWidth(final Map diodeNodeToWidthMap,
                                 final HierName nodeName) 
    {
        final double[] d = (double[]) diodeNodeToWidthMap.get(nodeName);
        return d == null ? 0.0 : d[0];
    }

    /**
     * Converts /'s in name to .'s, leaving .'s alone.  This is
     * a no-op because HierName can do everything in its
     * getAsString(separator) method.
     * @design jmr change to handle global!ext? instead of in 
     * parse stage?
     **/
    private HierName convertName(final HierName name) {
        return name;
    }

    /**
     * Convert the stored aspice file, and return it.  Calling
     * this more than once will be very, very bad.
     **/
    private AspiceFile convert() {
        // algorithm to convert ext -> aspice
        // for each fet(area,perim,{source,drain,gate}.length):
        //   make a transistor
        //   record length of source/drain for use by diodes
        //   aspice transistors have (width,length)
        // for each node({pdiode,ndiode}.{area,perim}):
        //   make a pdiode w/ area/perim given by resist class iDiodeType
        //   make an ndiode w/ area/perim given by resist class iDiodeType
        //   aspice diodes have (width,length,area,perim)
        //   the width / length are obtained as follows:
        //     width: sum of widths of adjacent transistors,
        //       (this is actually calculated in the fet loop, for
        //        efficiency)
        //     length: 0.0, as it doesn't matter
        // for each cap(cap): 
        //   make a cap
        //   aspice caps have (cap)

        // we will always use the canonical name for a node when
        // converting to AspiceFile
        // XXX what about connections to subcells?

        // convert subcells
        // for all subcells of this cell, 
        //   convert that subcell if its type was not already converted
        //   add the converted subcell
        for (Iterator iSubcellName = ext.getSubcellNames();
             iSubcellName.hasNext(); )
        {
            // no conversion of type names in needed for ext vs aspice

            final String name = (String) iSubcellName.next();
            final String type = ext.getSubcellTypeForName(name);

            AspiceFile sub = (AspiceFile) subcellNameToDefnMap.get(type);

            if (sub == null) {
                // the type has not been converted, so convert it
                sub = ext2aspice(ext.getSubcellDefForName(name),
                        subcellNameToDefnMap);
                // conversion will add the file to subcellNameToDefnMap
            }

            // add the converted cell to the list of subcells
            aspiceFile.addSubcell(name, sub);
        }

        // array indexed by fet type of maps from diode name to width
        // along the fet
        // @bug jmr XXX does this deal with different names for
        // the same node correctly?
        final Map[] diodeNodeToWidthMapArr
            = new Map[]{new HashMap(), new HashMap()};

        // fets
        for (final Iterator ifets = ext.getFETs(); ifets.hasNext(); ) {
            final FET fet = (FET) ifets.next();

            final int type = fet.getType();
            final HierName source = convertName(ext.getCanonicalName(
                        fet.getSource().getConnectingNode()));
            final HierName drain = convertName(ext.getCanonicalName(
                        fet.getDrain().getConnectingNode()));
            final HierName gate = convertName(ext.getCanonicalName(
                        fet.getGate().getConnectingNode()));
            final HierName bulk = convertName(ext.getCanonicalName(
                        fet.getSubstrateNode()));

            // width / length computation
            // ext files provide:
            //    a = gate area
            //    p = gate perimeter
            //   pg = gate portion of perimeter
            //   ps = source portion of perimeter
            //   pd = drain portion of perimeter
            //   where p = pg + ps + pd
            // aspice files use:
            //   width & length, where these are not clearly specified

            // we will use
            //   w = (ps + pd) / 2
            //   l = a / w
            // this matches the definition in Andrew's old ext2aspice

            final double a = fet.getArea();
            final double ps = fet.getSource().getLength();
            final double pd = fet.getDrain().getLength();

            final double w = (ps + pd) / 2.0;
            final double l = a / w;

            aspiceFile.addTransistor(new Transistor(type, source, drain,
                        gate, bulk, w, l));

            // accumulate length along source/drain for use in creating
            // diodes
            addDiodeWidth(diodeNodeToWidthMapArr[type], source, ps);
            addDiodeWidth(diodeNodeToWidthMapArr[type], drain, pd);
        }

        // Jikes is broken.  But it compiles fast.  This lets
        // us compile with jikes, which we really want to do.
        // Hopefully we can take this out in a later version.
        HierName jikesGND, jikesVdd;
        try {
            jikesGND = HierName.makeHierName("GND!", '.');
            jikesVdd = HierName.makeHierName("Vdd!", '.');
        } catch (InvalidHierNameException e) {
            throw new AssertionFailure(e);
        }

        // Names of nodes for diode drains
        final HierName GND = jikesGND, Vdd = jikesVdd;

        final HierName[] diodeDrains = new HierName[]{GND, Vdd};

        // nodes
        for (final Iterator iNode = ext.getNodes(); iNode.hasNext(); ) {
            final Node node = (Node) iNode.next();
            final HierName extSource = ext.getCanonicalName(node.getName());
            final HierName source = convertName(extSource);

            // create diode if non-zero area/perim
            for (int iDiodeType = 0; iDiodeType < 2; ++iDiodeType) {
                final double area = node.getAreaForResistClass(iDiodeType);
                final double perim = node.getPerimForResistClass(iDiodeType);

                if (area > 0.0 || perim > 0.0) {
                    // length is 0.0 to indicate it is meaningless,
                    // aspice will make one up

                    // width is sum of widths of connecting transistors

                    aspiceFile.addDiode(new Diode(iDiodeType, source,
                                diodeDrains[iDiodeType], 
                                getDiodeWidth(
                                    diodeNodeToWidthMapArr[iDiodeType],
                                    source),
                                0.0, area, perim));
                }
            }

            // create cap to ground
            final double capToGnd = node.getCapacitanceToGround();

            // add cap unless the nodes are equivalent or there is none
            if (capToGnd != 0.0 && !ext.areEquivalent(source, GND))
                aspiceFile.addCapacitor(
                        new Capacitor(source, GND, capToGnd));

            // namespace: aliases within the subcell
            //        and connections between cell/sub & sub/sub
            // both are handled, as we don't treat subcell names specially
            aspiceFile.addName(source);
            for (final Iterator iEquivNames
                    = ext.getEquivalentNames(extSource);
                    iEquivNames.hasNext(); ) {
                final HierName name2 = (HierName) iEquivNames.next();

                aspiceFile.connectNames(source, convertName(name2));
            }
        }

        // add node to node caps, caps to ground have already been added
        // @todo like caps should be merged
        for (final Iterator iCap = ext.getCapacitors(); iCap.hasNext(); ) {
            // change names for aspice
            final Capacitor cap = (Capacitor) iCap.next();
            final HierName s
                = convertName(ext.getCanonicalName(cap.getSource()));
            final HierName d
                = convertName(ext.getCanonicalName(cap.getDrain()));

            // add cap unless the nodes are equivalent
            if (!ext.areEquivalent(s, d))
                aspiceFile.addCapacitor(new Capacitor(s, d, cap.getCap()));
        }

        // resistors

        // now that the file has been converted, add it to the global 
        // repository
        subcellNameToDefnMap.put(aspiceFile.getName(), aspiceFile);

        return aspiceFile;
    }

    /**
     * Prints usage message, then exits with status
     * <code>exitStatus</code>.
     **/
    private static void usage(int exitStatus) {
	final String usageStr =  "Usage:"
	    + " [--ext-path=extractpath]"
            + " cellName";
                               
        System.err.println(usageStr);
        System.exit(exitStatus);
    }

    /**
     * Convert a .ext file into a .aspice file.
     * @param args  an array of one string: the cellname to use
     *   the .ext file will be loaded from cellname.ext
     **/
    public static void main(String[] args)
        //throws Exception
    {
        if (args.length != 1)
            usage(1);


	CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
	CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles( parsedArgs ); 

	CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs( argsWithConfigs );

	CommandLineArgs theArgs = cachedArgs;

        String cellName = null;

	{
	    StringContainerIterator strIter = theArgs.nonParsedArgumentsIterator();
	    if ( strIter.hasNext() ) {
		cellName = strIter.next();
	    }
	    else {
		usage(1);
	    }
	}
        final String aspiceFileName = cellName + ".aspice";

        try {
            final FileSearchPath fsp = new FileSearchPath( theArgs.getArgValue("ext-path","."),
							   System.getProperty( "user.home" ) );
            System.out.println("EXT_PATH is " + fsp.getSearchPathString());

            final OutputStream os
                = new FileOutputStream(aspiceFileName);

            System.out.println("Parsing .ext files ...");
            final ExtCell ext = ExtParser.parse(cellName, fsp, true);

            System.out.println("Building aspice structure ...");
            final AspiceFile af = ext2aspice(ext);

            System.out.println("Writing " + aspiceFileName + " ...");
            af.printToStream(os);

            System.out.println("Done.");
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println(e + ": " + e.getMessage());
            System.exit(1);
        }

    }
}
