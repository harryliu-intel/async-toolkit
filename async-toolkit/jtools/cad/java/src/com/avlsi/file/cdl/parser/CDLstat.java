/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;
import java.util.Iterator;
import java.util.Map;
import java.util.Stack;

import com.avlsi.util.container.Counter;
import com.avlsi.util.container.HashCounter;
import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.tools.jauto.TechnologyData;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.text.StringUtil;
import com.avlsi.util.text.NumberFormatter;

public class CDLstat implements CDLSimpleInterface {
    /** Contains statistics for all the instanciations of a cell,
        NOT for all the instances in that cell. */
    private static class InstStat {
        /** Number of instances */
        public int instances = 0;

        /** Map of nets to connection count*/
        public HashCounter netCounter = new HashCounter();
        /** Map of ports */
        public HashSet portSet = new HashSet();
        /** Total number of transistors */
        public int totalTransistors = 0;
        /** Total area of all instances */
        public double totalArea = 0;
        /** Total area (with overhead) of all instances */
        public double totalAreaWithOverhead = 0;
        /** Total width of transistors in all instances */
        public double totalWidth = 0;
        /** Total length of transistors in all instances */
        public double totalLength = 0;

        /** Total number of resistors */
        public double totalResistors = 0;
        /** Total resistance */
        public double totalResistance = 0;

        /** Total number of capacitors */
        public double totalCapacitors = 0;
        /** Total capacitance */
        public double totalCapacitance = 0;

        /** The area of the largest instance */
        public double maxArea = -Double.MAX_VALUE;
        /** The area of the smallest instance */
        public double minArea = Double.MAX_VALUE;
    }

    public static class EstimatedSize {
        public double width;
        public double height;
        public final double availableArea;
        /** Density factor computed from available area. */
        public final double reportDensityFactor;

        /** 
         * Choose width or height to satisfy:
         * (width - cellWidthOverhead) * (height - cellHeightOverhead)
         * >= transistorArea * densityFactor
         **/
        public EstimatedSize(final double cellWidth,
                             final double cellHeight,
                             final double densityFactor,
                             final double cellWidthOverhead,
                             final double cellHeightOverhead,
                             final double cellWidthIncrement,
                             final double cellHeightIncrement,
                             final double cellWidthMinimum,
                             final double cellHeightMinimum,
                             final double transistorArea) {
            double a = densityFactor * transistorArea;
            if (cellHeight > 0 && cellWidth > 0) {
                height = cellHeight;
                width = cellWidth;
            } else if (cellHeight > 0) {
                height = cellHeight;
                width = a / (height - cellHeightOverhead) + cellWidthOverhead;
                width = round(width,cellWidthIncrement);
                width = Math.max(width,cellWidthMinimum);
            } else if (cellWidth > 0) {
                width = cellWidth;
                height = a / (width - cellWidthOverhead) + cellHeightOverhead;
                height = round(height,cellHeightIncrement);
                height = Math.max(height,cellHeightMinimum);
            } else {
                throw new IllegalArgumentException(
                        "Must specify at least height or width");
            }
            availableArea = (width  - cellWidthOverhead) *
                            (height - cellHeightOverhead);
            reportDensityFactor = availableArea / transistorArea;
        }

        private double round(final double value, final double incr) {
            return incr == 0 ? value : incr * Math.ceil(value / incr);
        }

        public String toString() {
            return String.format("EstimatedSize: width=%g height=%g " +
                                 "availableArea=%g reportDensityFactor=%g",
                                 width, height, availableArea,
                                 reportDensityFactor);
        }
    }

    /** Contains statistics for a cell */
    public static class CellStat {

        public int getNumNets() {
            return netCounter.size();
        }
        
        public int getNumLocalNets() {
            return netCounter.size() - portSet.size();
        }
        
        public int getNumPorts() {
            return portSet.size();
        }
        
        public int getNumConnections() {
            return netCounter.getTotalCount();
        }

        /** Map of nets to connection count*/
        public Counter netCounter = new HashCounter();
        /** Set of ports */
        public Set portSet = new HashSet();

        /** Number of instances */
        public int instances = 0;
        /** Number of transistors */
        public int transistors = 0;
        /** Gate area of transistors */
        public double area = 0;
        /** Area of transistors (including overhead) **/
        public double areaWithOverhead = 0;
        /** Total width of transistors */
        public double width = 0;
        /** Total length of transistors */
        public double length = 0;

        /** Total resistance */
        public double resistance = 0;
        /** Number of resistors */
        public int resistors = 0;

        /** Total capacitance */
        public double capacitance = 0;
        /** Number of capacitors */
        public int capacitors = 0;

        public String toString() {
            return String.format("CellStat: instances=%d transistors=%d " +
                                 "area=%g areaWithOverhead=%g width=%g " +
                                 "length=%g resistance=%g resistors=%d " +
                                 "capacitance=%g capacitors=%d numNets=%d " +
                                 "numLocalNets=%d numPorts=%d " +
                                 "numConnections=%d",
                                 instances, transistors,
                                 area, areaWithOverhead, width,
                                 length, resistance, resistors,
                                 capacitance, capacitors, getNumNets(),
                                 getNumLocalNets(), getNumPorts(),
                                 getNumConnections());
        }
    }
    
    private Exception mError;

    private final Map instStatMap;
    private final Map cellStatMap;
    private final Map cellTemplateMap;
    private final Stack stack;
    private final double transistorWidthOverhead;
    private final double transistorLengthOverhead;
    
    private CDLstat( final Map instStatMap, 
                     final Map cellStatMap,
                     final Map cellTemplateMap,
                     final double transistorWidthOverhead,
                     final double transistorLengthOverhead
                     ) {
        this.instStatMap = instStatMap;
        this.cellStatMap = cellStatMap;
        this.cellTemplateMap = cellTemplateMap;
        this.transistorWidthOverhead = transistorWidthOverhead;
        this.transistorLengthOverhead = transistorLengthOverhead;
        this.stack = new Stack();
        this.stack.push( new CellStat() );
        this.mError = null;
    }

    public Exception getError() {
        return mError;
    }

    private CellStat current() {
        if ( stack.empty() ) {
            return null;
        }
        else {
            return ( CellStat ) stack.peek();
        }
    }

    public void makeResistor( HierName name,
                              HierName n1, 
                              HierName n2,
                              double val) {
        if ( mError == null ) {
            final CellStat stat = current();
            stat.resistors++;
            // val is mho, convert to ohm
            stat .resistance += 1 / val;
            stat.netCounter.add(n1);
            stat.netCounter.add(n2);
        }
    }

    public void makeCapacitor( HierName name,
                               HierName npos,
                               HierName nneg,
                               double val ) {
        if ( mError == null ) {
            final CellStat stat = current();
            stat.capacitors++;
            stat.capacitance += val;
            stat.netCounter.add(npos);
            stat.netCounter.add(nneg);
        }
    }

    public void makeTransistor( HierName name, 
                                int type,
                                HierName ns,
                                HierName nd,
                                HierName ng,
                                HierName nb,
                                double w,
                                double l ) {
        if ( mError == null ) {
            final CellStat stat = current();
            stat.netCounter.add(ns);
            stat.netCounter.add(nd);
            stat.netCounter.add(ng);
            stat.netCounter.add(nb);
            stat.transistors++;
            stat.area += w * l;
            stat.areaWithOverhead += (w + transistorWidthOverhead) *
                                     (l + transistorLengthOverhead);
            stat.width += w;
            stat.length += l;
        }
    }

    public void makeDiode( HierName name, 
                           int type,
                           HierName npos,
                           HierName nneg,
                           double w,
                           double l,
                           double a,
                           double p ) {
        if ( mError == null ) {
            final CellStat stat = current();
            stat.netCounter.add(npos);
            stat.netCounter.add(nneg);
        }
    }

    public void makeInductor( HierName name,
                              HierName npos,
                              HierName nneg,
                              double val ) {
        if ( mError == null ) {
            final CellStat stat = current();
            stat.netCounter.add(npos);
            stat.netCounter.add(nneg);
        }
    }

    public void makeBipolar(HierName name,
                            int type,
                            HierName nc,
                            HierName nb,
                            HierName ne,
                            double a) {
        if ( mError == null ) {
            final CellStat stat = current();
            stat.netCounter.add(nc);
            stat.netCounter.add(nb);
            stat.netCounter.add(ne);
        }
    }


    private void updateInstStat( String subName, CellStat stat ) {
        if (!instStatMap.containsKey(subName)) {
            instStatMap.put(subName, new InstStat());
        }

        InstStat s = (InstStat) instStatMap.get(subName);
        s.instances++;
        
        s.netCounter.addAll(stat.netCounter);
        s.portSet.addAll(stat.portSet);

        s.instances += stat.instances;
        s.totalTransistors += stat.transistors;
        s.totalArea += stat.area;
        s.totalAreaWithOverhead += stat.areaWithOverhead;
        s.totalWidth += stat.width;
        s.totalLength += stat.length;
        if (stat.area > s.maxArea) {
            s.maxArea = stat.area;
        }
        if (stat.area < s.minArea) {
            s.minArea = stat.area;
        }

        s.totalResistors += stat.resistors;
        s.totalResistance += stat.resistance;

        s.totalCapacitors += stat.capacitors;
        s.totalCapacitance += stat.capacitance;
    }

    public void makeCall( final HierName name, 
                          final String subName,
                          final HierName[] args,
                          final Map parameters) {
        
        if ( mError == null ) {

            final CellStat stat = current();         

            for(int i=0; i<args.length; i++) {
                stat.netCounter.add(args[i]);
            }
            
            stat.instances++;

            final Template temp = ( Template ) cellTemplateMap.get( subName );
            if ( temp == null ) {
            } 
            else {
                final CellStat instanceMasterStat; 

                final CellStat existingInstanceMasterStat =
                    ( CellStat ) cellStatMap.get( subName );
                
                if ( ( parameters.isEmpty() ) &&
                     ( existingInstanceMasterStat != null ) ) {
                    instanceMasterStat = existingInstanceMasterStat;
                }
                else {
                    stack.push( new CellStat() );
                    temp.execute( parameters, this );
                    final CellStat subCellStat = ( CellStat ) stack.pop();
                    instanceMasterStat = subCellStat;
                    cellStatMap.put( subName, subCellStat );
                }
                
                stat.transistors += instanceMasterStat.transistors;
                stat.area += instanceMasterStat.area;
                stat.areaWithOverhead += instanceMasterStat.areaWithOverhead;
                stat.width += instanceMasterStat.width;
                stat.length += instanceMasterStat.length;
                stat.resistance += instanceMasterStat.resistance;
                stat.resistors += instanceMasterStat.resistors;
                stat.capacitance += instanceMasterStat.capacitance;
                stat.capacitors += instanceMasterStat.capacitors;
                
                if (instStatMap != null) {
                    updateInstStat(subName, instanceMasterStat );
                }
            }
        }
    }

    public void beginSubcircuit(String subName, String[] in, String[] out) {  
        final CellStat stat = current();
       
        for(int i=0; i<in.length; i++) {
            stat.portSet.add(in[i]);
        }
        for(int i=0; i<out.length; i++) {
            stat.portSet.add(out[i]);
        }

    }

    public void endSubcircuit(String subName) {
        cellStatMap.put( subName, current() );
    }


    public static CellStat getCellStat( final String cellName,
                                        final Map templateMap ) {
        return getCellStat( cellName, templateMap, null );
    }

    public static CellStat getCellStat( final String cellName,
                                        final Map templateMap,
                                        final Map instStatMap ) {
        return getCellStat( cellName, templateMap, new HashMap(), instStatMap,
                            0, 0 );
    }

    public static CellStat getCellStat( final String cellName,
                                        final Map templateMap,
                                        final Map cellStatMap,
                                        final Map instStatMap,
                                        final TechnologyData techData ) {
        return getCellStat( cellName, templateMap, cellStatMap, instStatMap,
                            techData.transistorWidthOverhead,
                            techData.transistorLengthOverhead );
    }

    public static CellStat getCellStat( final String cellName,
                                        final Map templateMap,
                                        final Map cellStatMap,
                                        final Map instStatMap,
                                        final double transistorWidthOverhead,
                                        final double transistorLengthOverhead ) {
        final Template cellTemplate = ( Template ) templateMap.get( cellName );
        
        final CDLstat factory = new CDLstat( instStatMap,
                                             cellStatMap,
                                             templateMap,
                                             transistorWidthOverhead,
                                             transistorLengthOverhead );

        cellTemplate.execute( new LocalEnvironment(), factory, cellName );

        if ( factory.getError() != null ) {
            throw new RuntimeException( factory.getError() );
        }

        return ( CellStat ) cellStatMap.get( cellName );
    }

    private static double getDirective( final CellInterface ci,
                                        final String directive ) {
        final Number value =
            ( Number ) DirectiveUtils.getTopLevelDirective( ci, directive );
        return value.doubleValue();
    }

    public static EstimatedSize getEstimatedSize( final CellInterface ci,
                                                  final CellStat stat ) {
        final double bitPitch = getDirective( ci, DirectiveConstants.BITPITCH );
        return new EstimatedSize(
            getDirective( ci, DirectiveConstants.WIDTH ) * bitPitch,
            getDirective( ci, DirectiveConstants.HEIGHT ) * bitPitch,
            getDirective( ci, DirectiveConstants.DENSITY_FACTOR ),
            getDirective( ci, DirectiveConstants.WIDTH_OVERHEAD ),
            getDirective( ci, DirectiveConstants.HEIGHT_OVERHEAD ),
            getDirective( ci, DirectiveConstants.WIDTH_INCREMENT ),
            getDirective( ci, DirectiveConstants.HEIGHT_INCREMENT ),
            getDirective( ci, DirectiveConstants.WIDTH_MINIMUM ),
            getDirective( ci, DirectiveConstants.HEIGHT_MINIMUM ),
            stat.areaWithOverhead );
    }

    private static String rightJustify(String text, int width) {
        final int len = text.length();
        if (len == width) return text;
        else if (len > width) return text.substring(0, width);
        else return StringUtil.repeatString(" ", width - len) + text;
    }

    private static String leftJustify(String text, int width) {
        final int len = text.length();
        if (len == width) return text;
        else if (len > width) return text.substring(0, width);
        else return text + StringUtil.repeatString(" ", width - len);
    }

    private static String centerJustify(String text, int width) {
        final int len = text.length();
        if (len == width) return text;
        else if (len > width) return text.substring(0, width);
        else {
            int half = (width - len) / 2;
            return StringUtil.repeatString(" ", half) + text + StringUtil.repeatString(" ", width - len - half);
        }
    }

    private static String format(double val) {
        if (val == 0) return "0";
        else return NumberFormatter.format(val);
    }

    private static String format(double val, int prec) {
        return NumberFormatter.format(val, prec);
    }

    private static void formatStat1(String type, InstStat s) {
        /* Ugh.  I NEED PRINTF! */
        System.out.print(leftJustify(type, 27) + " ");
        System.out.print(rightJustify(format(s.instances), 6) + " ");
        System.out.print(rightJustify(format(s.totalTransistors), 10) + " ");
        System.out.print(leftJustify(format(s.totalArea, 4), 11));
        System.out.print(leftJustify(format(s.minArea, 4), 11));
        System.out.println(leftJustify(format(s.maxArea, 4), 11));
    }
    private static void formatStat2(String type, InstStat s) {
        System.out.print(leftJustify(type, 27) + " ");
        System.out.print(rightJustify(format(s.totalResistors), 6) + " ");
        System.out.print(leftJustify(format(s.totalResistance, 4), 9) + " ");
        System.out.print(rightJustify(format(s.totalCapacitors), 6) + " ");
        System.out.print(leftJustify(format(s.totalCapacitance, 4), 11) + " ");
        System.out.println(leftJustify(format(s.totalWidth, 4), 10));
        System.out.println(leftJustify(format(s.totalLength, 4), 10));
    }

    private static void usage() {
        final String className = CDLstat.class.getName();
        System.out.println( "Usage: " + 
                            System.getProperty( "java.home" ) +
                            System.getProperty( "file.separator" ) +
                            "bin" +
                            System.getProperty( "file.separator" ) +
                            "java " +
                            " -classpath " +
                            System.getProperty( "java.class.path" ) + " " +
                            className + "\n" +
                            "[--verbose] --cell=<cell> <cdlfile> ..." );
    }



    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs );

        final CommandLineArgs cachedArgs =
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;
        final String cell = theArgs.getArgValue("cell", null);
        final boolean verbose = theArgs.argExists("verbose");
        if (cell == null) {
            System.err.println("You must specify a cell name.");
            usage();
            System.exit(1);
        }
        final Map cellTemplates = new HashMap();
        
        Template.getTemplates( theArgs.nonParsedArgumentsIterator(), 
                               cellTemplates );

        final CellStat result;
 
        if (verbose) {
            final Map cellStatMap = new HashMap();
            final Map instStatMap = new HashMap();
            result = getCellStat( cell, cellTemplates, cellStatMap, instStatMap, 0, 0 ); 

            System.out.print(leftJustify("Type", 28));
            System.out.print(centerJustify("Uses", 7));
            System.out.print(centerJustify("Transistors", 11));
            System.out.print(centerJustify("Area", 11));
            System.out.print(centerJustify("Min Area", 11));
            System.out.println(centerJustify("Max Area", 11));
            for ( Iterator i = instStatMap.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final String type = (String) entry.getKey();
                final InstStat s = (InstStat) entry.getValue();
                formatStat1(type, s);
            }
            System.out.print(leftJustify("Type", 28));
            System.out.print(centerJustify("R", 7));
            System.out.print(centerJustify("ohm", 10));
            System.out.print(centerJustify("C", 7));
            System.out.print(centerJustify("farad", 12));
            System.out.println(centerJustify("Width", 10));
            for (Iterator i = instStatMap.entrySet().iterator(); i.hasNext(); ) {
                final Map.Entry entry = (Map.Entry) i.next();
                final String type = (String) entry.getKey();
                final InstStat s = (InstStat) entry.getValue();
                formatStat2(type, s);
            }
        }
        else {
            result = getCellStat( cell, cellTemplates );
        }
      

        System.out.println("Total Nets = " + result.getNumNets());
        System.out.println("Total Ports = " + result.getNumPorts());
        System.out.println("Total Local Nets = " + result.getNumLocalNets());
        System.out.println("Total Internal Connections at top level = " + result.getNumConnections());
        System.out.println("Total transistors = " + result.transistors);
        System.out.println("Total area = " + result.area);
        System.out.println("Average area = " + result.area / result.transistors);
        System.out.println("Total width = " + result.width);
        System.out.println("Total length = " + result.length);
        System.out.println("Average width = " + result.width / result.transistors);
        System.out.println("Total resistors = " + result.resistors);
        System.out.println("Total resistance = " + result.resistance);
        System.out.println("Average resistance = " + result.resistance / result.resistors);
        System.out.println("Total capacitors = " + result.capacitors);
        System.out.println("Total capacitance = " + result.capacitance);
        System.out.println("Average capacitance = " + result.capacitance / result.capacitors);
    }
}
