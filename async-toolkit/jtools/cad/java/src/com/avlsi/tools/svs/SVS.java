/*
 * Copyright 2001-2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.svs;

import java.io.FileReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;
import java.util.NoSuchElementException;

import com.avlsi.cast.CastFileParser;

import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.aspice.Transistor;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.cdl.parser.AspiceCellAdapter;
import com.avlsi.file.cdl.parser.Inline;
import com.avlsi.file.ext.ExtCell;
import com.avlsi.file.ext.parse.ExtParser;
import com.avlsi.io.FileSearchPath;
import com.avlsi.prs.ProductionRule;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.ext2aspice.Ext2Aspice;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.Namespace;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.functions.UnaryAction;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.text.StringUtil;


import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

import com.avlsi.cell.RefinementException;

/**
 * 
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
public final class SVS {

    private static CellInterface parseCell(
            final String cellName,
            final FileSearchPath castPath,
            final boolean verbose) throws Exception {
        if (cellName.lastIndexOf('.') == -1) {
            System.err.println
                ("You must specify a fully qualified cell name.");
            usage(1);
        }
        
        return new CastFileParser(castPath, "2", verbose)
                    .getFullyQualifiedCell(cellName);

    }

    private boolean assertSame(boolean design, 
                               CellInterface designCell,
                               boolean layout, 
                               CellInterface layoutCell,
                               String what) 
        throws RefinementException {
        return this.assertSame(design,designCell,
                               layout,layoutCell,
                               what,"design cell", "layout cell");
    }

    private boolean assertSame(boolean design, 
                               CellInterface designCell,
                               boolean layout, 
                               CellInterface layoutCell,
                               String what,
                               String dName, String lName) 
        throws RefinementException {
        if(design) {
            if(layout) 
                return true;
            else
                raise(layoutCell,designCell,
                      dName+" is "+what+" but "+lName+" isn't");
        } else if(layout) {
            raise(layoutCell,designCell,
                  lName+" is "+what+" but "+dName+" isn't");
        } 
        return false;
    }

    private boolean typeEquals(final CellInterface designCell,
                               final CellInterface layoutCell) {
        return layoutCell.getFullyQualifiedType().equals
            (designCell.getFullyQualifiedType());
    }

    private void raise(final CellInterface layoutCell,
                               final CellInterface designCell,
                               String message)
        throws RefinementException {
        throw new RefinementException
                (message,
                 layoutCell.getFullyQualifiedType(),
                 designCell.getFullyQualifiedType());
        
    }

    Set checkRefinementPassed=new HashSet();
    Set checkRefinementFailed=new HashSet();
    private boolean checkRefinement(final String prefix,
                                 final CellInterface designCell,
                                 final CellInterface layoutCell) {

        System.err.println("checking design cell: "+
                           designCell.getFullyQualifiedType());
        System.err.println("vs. layout cell:      "+
                           layoutCell.getFullyQualifiedType());

        // see if we already checked these types
        if(checkRefinementPassed.contains
           (new Pair(designCell,
                     layoutCell))) {
            System.err.println("already checked");
            return true;
        }
        if(checkRefinementFailed.contains
           (new Pair(designCell,
                     layoutCell))) {
            System.err.println("already checked, failed");
            return false;
        }

        boolean passed=true;
        try {
            // if they're both nodes, that's the end
            if(assertSame(designCell.isNode(),designCell,
                          layoutCell.isNode(),layoutCell,
                          "node")) {
                return true;
            }

            // are they channels?
            boolean isChannel = 
                assertSame(designCell.isChannel(),designCell,
                           layoutCell.isChannel(),layoutCell,
                           "channel");

            // check that layout cell and design cell are not same type
            // unless they are channels
            if(!isChannel && typeEquals(layoutCell,designCell)
               && !CellUtils.isWiring(designCell)) {
                raise(layoutCell,designCell,
                      "layout cell is the same as the design cell "+
                      "and neither is a wiring cell");
            }

            // check that layoutCell refines designCell (we can't use
            // CellInterface.eventuallyRefinesFrom() because it compares
            // objects and not fully qualified types)
            CellInterface temp = layoutCell;
            while(temp!=null) {
                if(typeEquals(temp,designCell))
                    break;
                temp = temp.getDirectRefinementParent();
            }
            if(temp==null)
                raise(layoutCell,designCell,
                      "layout cell doesn't refine design cell");

            String dn = designCell.getFilename();
            String tn = temp.getFilename();
            if(!(dn==null?"":dn).equals((tn==null?"":tn))) {
                raise(temp,designCell,
                      "File names differ: design cell file name = "+
                      (dn==null?"<null file name>":dn)
                      +", layout ancestor cell file name = "+
                      (tn==null?"<null file name>":tn));
            } else {
                if(dn==null)
                    System.err.println("Warning: no filename for "+
                                       designCell.getFullyQualifiedType()+", "+
                                       layoutCell.getFullyQualifiedType());
            }

            // check that they have the same subcell names
            Set designNames=(Set)CollectionUtils.
                addAll(new TreeSet(),designCell.getSubcellNames());
            Set layoutNames=(Set)CollectionUtils.
                addAll(new TreeSet(),layoutCell.getSubcellNames());
            if (!layoutNames.equals(designNames)) {
                String errString="subcell sets not the same: ";
                Set diff0=new TreeSet(), diff1=new TreeSet();
                diff0.addAll(designNames);
                diff0.removeAll(layoutNames);
                diff1.addAll(layoutNames);
                diff1.removeAll(designNames);
                Iterator i0=diff0.iterator();
                while(i0.hasNext()) {
                    errString+="\n   "+i0.next()+" in design but not layout";
                }
                Iterator i1=diff1.iterator();
                while(i1.hasNext()) {
                    errString+="\n   "+i1.next()+" in layout but not design";
                }
                raise(layoutCell,designCell,errString);
            }

            // only want non-inlined subcell for this part
            Iterator si=layoutCell.getSubcellPairs();
            while(si.hasNext()) {
                Pair next=(Pair)si.next();
                HierName name=(HierName)next.getFirst();
                System.err.println("examining subcell: "+
                                   designCell.getFullyQualifiedType()+
                                   "."+name);
                passed = passed && checkRefinement(designCell.getSubcell(name),
                                                   layoutCell.getSubcell(name));
            }
        } catch(RefinementException e) {
            System.err.println(e.getMessage());
            passed = false;
        }
        if(passed)
            checkRefinementPassed.add(new Pair(designCell,layoutCell));
        else
            checkRefinementFailed.add(new Pair(designCell,layoutCell));
        return passed;
    }

    public boolean checkRefinement(
            final CellInterface designCell,
            final CellInterface specCell) {
        return checkRefinement(null, designCell, specCell);
    }

    private static void usage(final int exitStatus) {
        System.err.println("Usage: java " + SVS.class.getName() + "\n" +
                           " --design-cell=cast_cell\n" +
                           " --design-cast-path=castpath\n" +
                           " --layout-cell=cast_cell\n" +
                           " --layout-cast-path=castpath\n" +
                           " [--verbose]");
        System.exit(exitStatus);
    }

    public static void main(String[] args) throws Exception {
	final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl(args);
	final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles(parsedArgs); 

	final CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs(argsWithConfigs);

	final CommandLineArgs theArgs = cachedArgs;
        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(SVS.class));
            System.exit(1);
        }
        
	// check number of arguments
        if (args.length != 4 && args.length != 5)
          usage(1);

        final boolean verbose = theArgs.argExists("verbose");

	final String designCellName =
            theArgs.getArgValue("design-cell", null);
        if(designCellName==null) usage(1);
        final String designCastPathString = theArgs.getArgValue("design-cast-path", ".");
        if(designCastPathString==null) usage(1);
	final FileSearchPath designCastPath =
            new FileSearchPath(designCastPathString);

	final String layoutCellName =
            theArgs.getArgValue("layout-cell", null);
        if(layoutCellName==null) usage(1);
        final String layoutCastPathString = theArgs.getArgValue("layout-cast-path", ".");
        if(layoutCastPathString==null) usage(1);
	final FileSearchPath layoutCastPath =
            new FileSearchPath(layoutCastPathString);

        System.err.println("Parsing design cell...");
        final CellInterface designCell =
            parseCell(designCellName, designCastPath, verbose);

        System.err.println("Parsing layout cell...");
        final CellInterface layoutCell =
            parseCell(layoutCellName, layoutCastPath, verbose);

        try {
            System.err.println("design name: "+
                               designCell.getFullyQualifiedType());
            System.err.println("layout name: "+
                               layoutCell.getFullyQualifiedType());
            boolean passed = new SVS().checkRefinement(designCell, layoutCell);
            if(passed)
                System.out.println("OK");
            else
                System.out.println("Failed");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
