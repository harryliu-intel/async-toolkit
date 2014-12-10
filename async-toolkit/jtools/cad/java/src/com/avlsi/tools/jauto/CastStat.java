/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.io.FileSearchPath;
import com.avlsi.prs.UnimplementableProductionRuleException;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.MappingIterator;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryFunction;

/**
 * A class that brings together miscellanous statistics tasks
 * @author Harry Liu
 * @version $Revision$ $DateTime$
 **/
public class CastStat {
    /**
     * This class should not be instantiated.
     **/
    private CastStat() { }

    private static final HierName Vdd = HierName.makeHierName("Vdd");
    private static final HierName GND = HierName.makeHierName("GND");
    public static void usage() {
        System.out.println("java com.avlsi.tools.jauto.CastStat \n" +
                           "    --cast-path=<path> (defaults to .)\n" +
                           "    --cast-version=[1 | 2] (defaults to 2)\n" +
                           "    --cell=<mod.ule.cell.subtype> (fully qualified cell name)\n");
        System.exit(1);
    }

    public static void getDynamicNodes(final CellInterface cell,
                                       final CastFileParser cfp,
                                       final Cadencize cad,
                                       final Set result) {
        final CadenceInfo cinfo = cad.convert(cell);
        final ExclusiveNodeSets exclusives = new ExclusiveNodeSets();
        exclusives.merge(cinfo.getPortExclusiveNodeSets());
        exclusives.merge(cinfo.getLocalExclusiveNodeSets());
        final NetGraph graph = new NetGraph(cinfo.getLocalNodes(),
                                            exclusives,
                                            new ArrayList(),
                                            Vdd,
                                            GND,
                                            Collections.EMPTY_SET);
        try {
            graph.addCellInterface(cell, new NetGraph[0], cfp, cad);
        } catch (UnimplementableProductionRuleException e) {
            System.err.println("Unimplementable production rules found in " + cell.getFullyQualifiedType());
        }
        graph.prepareForLvs();
        for (Iterator i = graph.getNodes().iterator(); i.hasNext(); ) {
            final NetGraph.NetNode node = (NetGraph.NetNode) i.next();
            if (node.isNamed() && node.isOutput() && !node.isCombinational()) {
                result.add(node.name);
            }
        }
    }
    
    private static void getDynamicNodes(final CellInterface cell,
                                        final CastFileParser cfp,
                                        final Cadencize cad,
                                        final Map result) {
        final String type = cell.getFullyQualifiedType();
        final Set s = new TreeSet();
        if (CellUtils.isLeaf(cell)) {
            getDynamicNodes(cell, cfp, cad, s);
        } else {
            for (Iterator i = cell.getLocalSubcellPairs(); i.hasNext(); ) {
                final Pair p = (Pair) i.next();
                final HierName name = (HierName) p.getFirst();
                final CellInterface subcell = (CellInterface) p.getSecond();
                if (CellUtils.isWiring(subcell)) continue;
                final String subtype = subcell.getFullyQualifiedType();
                if (!result.containsKey(subtype)) {
                    getDynamicNodes(subcell, cfp, cad, result);
                }
                final Set ss = (Set) result.get(subtype);
                Debug.assertTrue(ss != null);
                for (Iterator j = ss.iterator(); j.hasNext(); ) {
                    final HierName h = (HierName) j.next();
                    s.add(HierName.prefixName(name, h));
                }
            }
        }
        result.put(type, s);
    }

    public static Set getDynamicNodes(final CellInterface cell,
                                      final CastFileParser cfp,
                                      final Cadencize cad) {
        final Map result = new HashMap();
        getDynamicNodes(cell, cfp, cad, result);
        return (Set) result.get(cell.getFullyQualifiedType());
    }

    /**
     * Returns a set of all dynamic nodes in the specified cell.  A
     * <code>NetGraph</code> is generated for the given cell, and it returns
     * all named output nodes that are not combinatorial.
     * @param cell Cell to get information for.
     **/
    public static Iterator getDynamicNodes(final CellInterface cell,
                                           final CastFileParser cfp) {
        final Map cache = new TreeMap();
        final Cadencize cad = new Cadencize(true);
        getDynamicNodes(cell, cfp, cad, cache);
        final Set names = (Set) cache.get(cell.getFullyQualifiedType());

        final CellInterface flat = cell.flatten();
        final Iterator nameIter = names.iterator();
        return new Iterator() {
            public boolean hasNext() {
                return nameIter.hasNext();
            }
            public Object next() {
                final HierName name = (HierName) nameIter.next();
                HierName canon = flat.getCanonicalName(name);
                Iterator aliases;

                // XXX: Work-around for bug 211.  Blame Jesse.
                if (canon == null) {
                    final String suffix = name.getSuffixString();
                    final HierName prefix = name.getParent();
                    final int index = suffix.lastIndexOf('[');
                    final String base = suffix.substring(0, index);
                    final String array = suffix.substring(index);
                    final HierName fake;
                    try {
                        fake = HierName.makeHierName(prefix, base);
                    } catch (InvalidHierNameException e) {
                        throw new RuntimeException("Cannot create HierName: " + e);
                    }
                    canon = flat.getCanonicalName(fake).appendString(array);
                    aliases = new MappingIterator(
                            flat.getConnectedNodes(fake),
                            new UnaryFunction() {
                                public Object execute(Object o) {
                                    final HierName h = (HierName) o;
                                    return h.appendString(array);
                                }
                            });
                } else {
                    aliases = flat.getConnectedNodes(canon);
                }
                return new Pair(canon, aliases);
            }
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;
        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");
        final String cellName = theArgs.getArgValue("cell", null);
        if (cellName == null) {
            System.err.println("ERROR: You must specify a cell name");
            usage();
        }
        final CastFileParser cfp = new CastFileParser(new FileSearchPath(castRoot), castVersion);
        final CellInterface ci = cfp.getFullyQualifiedCell(cellName);
        for (Iterator i = getDynamicNodes(ci, cfp); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            System.out.println(p.getFirst());
        }
    }

    public static Set getDynamicPortNodes(final CellInterface cell,
                                          final Cadencize mCadencizer,
                                          final CastFileParser mCastParser,
                                          final Map mDynamicPortNodesMap,
                                          final Map mDynamicNodesMap)
        throws InvalidHierNameException {

        final Set existingResult = 
            ( Set ) mDynamicPortNodesMap.get( cell.getFullyQualifiedType() );
        
        if ( existingResult == null ) {

            final CadenceInfo cadenceInfo = mCadencizer.convert( cell );

            final AliasedMap portMap = cadenceInfo.getPortNodes();

            final Set dynamicNodesInCell =
                getDynamicNodes( cell, mCadencizer, mCastParser,
                                 mDynamicPortNodesMap, mDynamicNodesMap );

            final Iterator nodeIter = dynamicNodesInCell.iterator();

            final Set result = new HashSet();

            while ( nodeIter.hasNext() ) {
                final String nodeName = ( String ) nodeIter.next();            
                if ( portMap.getValue( HierName.makeHierName( nodeName, '.' ) ) != null ) {
                    result.add( nodeName );
                }
            }
            
            mDynamicPortNodesMap.put( cell.getFullyQualifiedType(), result );
            return result;
        }
        else {
            return existingResult;
        }
    }

    public static Set getDynamicNodes(final CellInterface cell,
                                      final Cadencize mCadencizer,
                                      final CastFileParser mCastParser,
                                      final Map mDynamicPortNodesMap,
                                      final Map mDynamicNodesMap)
        throws InvalidHierNameException {

        final Set existingResult = 
            ( Set ) mDynamicNodesMap.get( cell.getFullyQualifiedType() );

        if ( existingResult == null ) {
        
            final Set result = new HashSet();
        
            if (CellUtils.isLeaf(cell)) {
                final Set hierNameSet = new HashSet();
                CastStat.getDynamicNodes( cell,
                                          mCastParser,
                                          mCadencizer,
                                          hierNameSet );
                final Iterator currHierName = hierNameSet.iterator();
                while ( currHierName.hasNext() ) {
                    final HierName curr = ( HierName )currHierName.next();
                    result.add( curr.toString() );
                }
            }
            else {
                final CadenceInfo cadenceInfo = mCadencizer.convert( cell );

                final Iterator subcellPairIter = cadenceInfo.getSubcellPairIterator();

                while ( subcellPairIter.hasNext() ) {
                    final Pair subcellPair = ( Pair ) subcellPairIter.next();
                    
                    final HierName instanceName = ( HierName ) subcellPair.getFirst();
                    final CellInterface subcellMaster = cell.getSubcell( instanceName );

                    final Set subcellDynamicPortNodes =
                        getDynamicPortNodes( subcellMaster, mCadencizer,
                                             mCastParser, mDynamicPortNodesMap,
                                             mDynamicNodesMap );


                    final Iterator dynNodeIter = subcellDynamicPortNodes.iterator();

                    while ( dynNodeIter.hasNext() ) {
                        final String dynNodeName = ( String ) dynNodeIter.next();

                        final HierName dynNodeHierName =
                            HierName.append( instanceName, 
                                             HierName.makeHierName( dynNodeName, '.' ) );

                        final HierName canonicalDynNodeName =
                            ( HierName ) cadenceInfo.getLocalNodes().getCanonicalKey( dynNodeHierName );

                        result.add( canonicalDynNodeName.toString() );
                    }
                }
                
            }
            mDynamicNodesMap.put( cell.getFullyQualifiedType(), result );
            return result;
        }
        else {
            return existingResult ;
        }
    }
}
