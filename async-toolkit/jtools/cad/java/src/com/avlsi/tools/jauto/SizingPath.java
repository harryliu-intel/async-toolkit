/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import com.avlsi.fast.*;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.lvs.NetGraph;

import java.util.Set;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.TreeSet;
import java.io.*;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.util.container.CollectionUtils;

public class SizingPath extends AbstractPath
{
    List/*<HalfOperator>*/  path;

    SizingPath              copyFrom;

    private HierName instanceName;

    /** Return the list of half-operators */
    public List/*<HalfOperator>*/ getPath()
    {
        return path;
    }



    /** Constructor 1 */
    public SizingPath()
    {
        path = new ArrayList/*<HalfOperator>*/();
        startNets = new HashSet/*<CellNet>*/();
        endNet = null;

        container = null;

        isFragment = false;
        isReduced = false;
        isEndNetNonObservable = false;
        copyFrom = null;

        delay = null;
        slack = null;
        delaybias = null;
        criticalNet = null;
        instanceName = null;
    }


    /** Constructor 2 */
    public SizingPath(SizingPath sp1)
    {
        path = new ArrayList/*<HalfOperator>*/(sp1.path);
        startNets = new HashSet/*<CellNet>*/(sp1.startNets);
        endNet = sp1.endNet;
        container = sp1.container;

        isFragment = sp1.isFragment;
        isReduced = sp1.isReduced;
        isEndNetNonObservable = sp1.isEndNetNonObservable;
        copyFrom = sp1.copyFrom;

        delay = sp1.delay;
        slack = sp1.slack;
        delaybias = sp1.delaybias;
        criticalNet = sp1.criticalNet;
        instanceName = sp1.instanceName;
    }




    /**
     * Generate sizing paths.  Sizing paths are generated for all cells in
     * <code>cellTypes</code>.  Any existing sizing paths of each cell in
     * <code>cellTypes</code> are replaced by the newly generated paths.
     *
     * @param cellTypes
     *        <code>CellType</code>s for which sizing paths should be
     *        generated.
     * @param ignoreReset
     *        If true, do not find paths starting at reset nodes.
     * @param completeSizingPath
     *        If true, use "weak cuts", so paths will not stop at input
     *        ports to the cell.  If false, use "strong cuts", and paths
     *        will stop there
     * @param pathLimit
     *        Log a warning with the message center for any cell containing
     *        more than this many paths.
     **/
    public static void generatePaths(Set/*<CellType>*/ cellTypes,
                                     boolean ignoreReset,
                                     boolean completeSizingPath,
                                     int pathLimit)
    {
        for (Iterator ita = cellTypes.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();

            if(DebugOption.printLevel <= 3){
                System.out.println("------------- Summary for path generation ---------------");
                System.out.println(cella.typeName);
            }

            if(cella.transistors.isEmpty()){ // cell without transistors
                continue;
            }

            cella.getSizingPaths().clear();

            // Get port cell nets and internal cell nets with "cutpath" directives

            Set/*<CellNet>*/ inputPortCellNets =
                new LinkedHashSet/*<CellNet>*/();
            Set/*<CellNet>*/ cutpathCellNets =
                new LinkedHashSet/*<CellNet>*/();
            for (Iterator itb =
                    CollectionUtils.sort(CellNet.getComparator(),
                                         cella.getAllNets().iterator());
                 itb.hasNext(); ) {
                CellNet cna = (CellNet)itb.next();

                if(cna.isCutPath()){
                    cutpathCellNets.add(cna);
                    continue;
                }

                if(!cna.isPortNet()){ // not a port net
                    continue;
                }
                
                if(cna.isReset()){
                    if(DebugOption.printLevel <= 1){
                        System.out.println("Note: Found global net: " + cna.canonicalName.getCadenceString());
                    }
                    if(ignoreReset){
                        continue;
                    }
                }

                if (cna.portDirection == CellNet.OUTPUT ||
                    cna.portDirection == CellNet.UNKNOWN) {
                    // not an input port
                    continue;
                }

                inputPortCellNets.add(cna);
            }


            // Add half-operators driven by port nets

            Set/*<HalfOperator>*/ halfOperators =
                new TreeSet/*<HalfOperator>*/(HalfOperator.getComparator());
            for (Iterator itb = inputPortCellNets.iterator();
                 itb.hasNext(); ) {
                CellNet cna = (CellNet)itb.next();

                for (Iterator itc = cna.getListSinks().iterator(); itc.hasNext(); ) {
                    NetSink nska = (NetSink)itc.next();

                    if(nska.type == NetType.HALF_OPERATOR_TRANSISTOR){ // half-operator load
                        if(DebugOption.printLevel <= 1){
                            System.out.println("----------- Added one half-operator ------------");
                            System.out.println(nska.sink.toString());
                        }

                        HalfOperator hoa = nska.sink;

                        halfOperators.add(hoa);

                        /*
                        if((hoa.transistors.size() <= 1) || (!cna.isReset())){
                        // if (!cna.isReset())
                            halfOperators.add(hoa);
                        }
                        else{ // reserved for possible future branch
                            if(k > 0){
                                halfOperators.add(hoa);
                            }
                        }
                        */
                    }
                }
            }

            // End adding half-operators driven by port nets


            // Add half-operators driven by subcells

            for (Iterator itb = cella.getAllNets().iterator(); itb.hasNext(); ) {
                CellNet cna = (CellNet)itb.next();
                if(cna.hasOnlyCellTypeSources()){
                    assert cna.getHalfOperatorSinks().size() == 0
                        : "Mixing of transistors and sub-cells in the " +
                          "same cell is not allowed in sizing.\n" +
                          "CellName: " + cella.typeName;

                    halfOperators.addAll(cna.getHalfOperatorSinks());
                }
            }

            // End adding half-operators drive by subcells


            // Add half-operators driven by "cutpath" cell nets

            // Set/*<HalfOperator>*/ cutpathHalfOps =
            //     new HashSet/*<HalfOperator>*/();
            for (Iterator itb = cutpathCellNets.iterator(); itb.hasNext(); ) {
                CellNet cna = (CellNet)itb.next();

                for (Iterator itc = cna.getListSinks().iterator(); itc.hasNext(); ) {
                    NetSink nska = (NetSink)itc.next();

                    if(nska.type == NetType.HALF_OPERATOR_TRANSISTOR){ // half-operator load
                        if(DebugOption.printLevel <= 1){
                            System.out.println("----------- Added one half-operator ------------");
                            System.out.println(nska.sink.toString());
                        }

                        HalfOperator hoa = nska.sink;

                        halfOperators.add(hoa);
                        // cutpathHalfOps.add(hoa);
                    }
                }
            }

            // End adding half-operators driven by "cutpath" cell nets


            Set/*<HalfOperator>*/ startingCutHalfOps =
                new HashSet/*<HalfOperator>*/();
            if(completeSizingPath){
                // if (!cella.isFragment())
                //    startingCutHalfOps.addAll(halfOperators);
                //startingCutHalfOps.addAll(cutpathHalfOps);
            }
            else{
                startingCutHalfOps.addAll(halfOperators);
            }


            // HISTORICAL NOTE: In a previous incarnation, this function
            //   would first try to generate paths with weak cuts, then,
            //   if there were too many paths, it would fall back to
            //   strong cuts.  We may want to restore this behavior in
            //   the future.
            
            if(DebugOption.printLevel <= 3){
                System.out.println("Total number of half-operators to start with: " +
                                   halfOperators.size());
            }

            Set/*<SizingPath>*/ generatedSizingPaths =
                new LinkedHashSet/*<SizingPath>*/();
            for (Iterator itb = halfOperators.iterator(); itb.hasNext(); ) {
                HalfOperator hoa = (HalfOperator)itb.next();
                if(DebugOption.printLevel <= 1){
                    System.out.println("------------- Start with half-operator -----------------");
                    System.out.println(hoa.toString());
                }

                recursivePaths(hoa, new ArrayList/*<HalfOperator>*/(),
                               generatedSizingPaths,
                               startingCutHalfOps);
            }


            if (generatedSizingPaths.size() > pathLimit) {
                String msa = "WARNING";
                String msb = "Too many sizing paths for this cell.\n";
                String msc = "CellName: " + cella.typeName + "\n"
                    + "Number of half-operators: "
                        + cella.getListHalfOperators().size() + "\n"
                    + "Number of paths: " + generatedSizingPaths.size()
                        + "\n"
                    + "Need manual path cut by adding \"cutpath\" directives." + "\n";

                System.out.println(msa + ":" + msb + msc);

                cella.design.messageCenter.createMessage(1, 10, msa, msb, msc);
            }


            for (Iterator itc = generatedSizingPaths.iterator();
                 itc.hasNext(); ) {
                SizingPath spa = (SizingPath)itc.next();
                spa.setStartEndNets(completeSizingPath);
                spa.copyFrom = spa;
            }


            cella.getSizingPaths().addAll(generatedSizingPaths);

            if(DebugOption.printLevel <= 1){
                System.out.println("Number of paths: " +
                                   generatedSizingPaths.size());
                for (Iterator itb = generatedSizingPaths.iterator();
                     itb.hasNext(); ) {
                    System.out.println(itb.next().toString());
                }
            }

            String s = cella.checkPathCoverage();
            if(DebugOption.printLevel <= 3){
                System.out.println("Path coverage check result: " + s);
            }

        }
    }


    /**
     * Recursively generate sizing paths. Traversal starts at half operator
     * <code>nextHalfOp</code> and traverses through sink half operators
     * until either a loop to a half operator in <code>partialPath</code>,
     * a half operator in <code>startingCutHalfOps</code>, a port net, or a
     * cut path half operator is found.  All paths found will have
     * the partial path <code>partialPath</code> prepended, and will then be
     * added to <code>allSizingPaths</code>.
     * 
     * @param nextHalfOp
     *        The <code>HalfOperator</code> from which to continue searching
     *        paths.
     * @param partialPath
     *        Previous <code>HalfOperator</code>s on the path.
     * @param allSizingPaths
     *        Set of all <code>SizingPath</code>s created by this function.
     *        Created paths will be added to this set.
     * @param startingCutHalfOps
     *        Set of starting <code>HalfOperator</code>s if we are using
     *        "strong cuts" (ie <code>generatePaths</code> was called
     *        with <code>completeSizingPath == false</code>), otherwise
     *        the empty set.  Path searches will stop when a
     *        <code>HalfOperator</code> in this set is reached.
     **/
    public static void recursivePaths(HalfOperator nextHalfOp,
                                      List/*<HalfOperator>*/ partialPath,
                                      Set/*<SizingPath>*/ allSizingPaths,
                                      Set/*<HalfOperator>*/
                                          startingCutHalfOps)
    {
        if(DebugOption.printLevel <= 1){
            System.out.println("\t------------ recursive step ---------------");
            System.out.println("\t" + nextHalfOp.toString());
        }


        if (partialPath.contains(nextHalfOp)) {
            // loop in path
            SizingPath spa = new SizingPath();
            spa.path.addAll(partialPath);
            allSizingPaths.add(spa);
            if(DebugOption.printLevel <= 1){
                System.out.println("One path generated, 1");
            }
            return;
        }

        if (startingCutHalfOps.contains(nextHalfOp) &&
            partialPath.size() != 0) {
            // starting half-operator for other paths
            SizingPath spa = new SizingPath();
            spa.path.addAll(partialPath);
            allSizingPaths.add(spa);
            if(DebugOption.printLevel <= 1){
                System.out.println("One path generated, 1a");
            }
            return;
        }

        List/*<HalfOperator>*/ newPartialPath =
            new ArrayList/*<HalfOperator>*/(partialPath);
        newPartialPath.add(nextHalfOp);

        // outputNet should never be null
        CellNet outputNet = nextHalfOp.outputNet;
        if (outputNet.isPortNet() || outputNet.isCutPath()) {
            // port net, path stops here
            SizingPath spa = new SizingPath();
            spa.path.addAll(newPartialPath);
            allSizingPaths.add(spa);
            if(DebugOption.printLevel <= 1){
                System.out.println("One path generated, 2");
            }

            return;
        }

        Set/*<HalfOperator>*/ newNextHalfOps =
            new TreeSet/*<HalfOperator>*/(HalfOperator.getComparator());
        boolean isCut = false;
        for (Iterator ita = outputNet.getListSinks().iterator();
             ita.hasNext(); ) {
            NetSink nska = (NetSink)ita.next();

            if (nska.type == NetType.HALF_OPERATOR_TRANSISTOR) {
                // half-operator load
                HalfOperator sinkHalfOp = nska.sink;

                boolean visited = false;
                /*
                for (Iterator itb = newPartialPath.iterator();
                     itb.hasNext(); ) {
                    HalfOperator hob = (HalfOperator)itb.next();

                    if (hob.outputNet == sinkHalfOp.outputNet) {
                        visited = true;
                        break;
                    }
                }
                */

                if (newPartialPath.contains(sinkHalfOp) ||
                    startingCutHalfOps.contains(sinkHalfOp) ||
                    visited) {
                    isCut = true;
                } else if (nextHalfOp.driveDirection !=
                               sinkHalfOp.driveDirection) {
                    // Since we are traversing from HalfOperator to
                    // CellNet and to HalfOperator again, we will find
                    // both up and down HalfOperators.  Only add those
                    // with opposite directions.
                    newNextHalfOps.add(sinkHalfOp);
                }
            }
        }

        if (newNextHalfOps.isEmpty() || isCut) {
            SizingPath spa = new SizingPath();
            spa.path.addAll(newPartialPath);
            allSizingPaths.add(spa);
            if(DebugOption.printLevel <= 1){
                System.out.println("One path generated, 3");
            }

            if (newNextHalfOps.isEmpty()) {
                return;
            }
        }

        if(DebugOption.printLevel <= 1){
            System.out.println("Number of sinks to be traversed: " +
                               newNextHalfOps.size());
        }

        for (Iterator ita = newNextHalfOps.iterator();
             ita.hasNext(); ) {
            HalfOperator hoa = (HalfOperator)ita.next();

            if(DebugOption.printLevel <= 1){
                System.out.println("Going into one more recursive loop");
            }
            recursivePaths(hoa, newPartialPath, allSizingPaths,
                           startingCutHalfOps);
            if(DebugOption.printLevel <= 1){
                System.out.println("Pop back one level");
                System.out.println(nextHalfOp.toString());
            }
        }

        if(DebugOption.printLevel <= 1){
            System.out.println("Nothing generated in this loop");
        }
    }



    public void setStartEndNets(boolean completeSizingPath)
    {
        endNet = ((HalfOperator) path.get(path.size() - 1)).outputNet;

        startNets.clear();

        HalfOperator hoa = (HalfOperator)path.get(0);
        container = hoa.subType;

        for (Iterator ita = container.getAllNets().iterator(); ita.hasNext(); ) {
            CellNet cna = (CellNet)ita.next();
            if(completeSizingPath){
                if(container.isFragment()){
                    if(cna.isPortNet()){
                        if(cna.hasSinkAs(hoa)){
                            startNets.add(cna);
                        }
                    }
                }
                else{
                    if(cna.hasSinkAs(hoa)){
                        startNets.add(cna);
                    }
                }
            }
            else{
                if(cna.hasSinkAs(hoa)){
                    startNets.add(cna);
                }
            }
        }
    }



    public String toString()
    {
        StringBuffer sb = new StringBuffer();

        for (Iterator ita = path.iterator(); ita.hasNext(); ) {
            HalfOperator hoa = (HalfOperator)ita.next();
            CellNet cna = hoa.outputNet;
            sb.append(cna.canonicalName.getCadenceString());
            if (hoa.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
                sb.append("- ");
            }
            else{
                sb.append("+ ");
            }
        }

        return sb.toString();
    }


    public String printPath()
    {
        return printPath(true, -1);
    }

    public String printPath(boolean b1) {
        return printPath(b1, -1);
    }

    public String printPath(int n) {
        return printPath(true, n);
    }

    private static Object lastElement(final List l) {
        if (l == null || l.isEmpty()) return null;
        else return l.get(l.size() - 1);
    }

    public String printPath(boolean b1, int n)
    {
       StringBuffer sb = new StringBuffer();
       final HalfOperator lastHalfOp = (HalfOperator)
            lastElement(path);
       GlobalNet gn;
       CellNet cn;
       TechnologyData tech;
       double[] effectiveResistanceFactors;
       double effectiveResistanceFactor;
       double f = 0.0;
       double wireCap = 0.0;
       double reff = 0.0;
       if (n >= 0 && b1) {
           gn = (GlobalNet) getEndNet().getGlobalNets().get(n);
           cn = gn.getTopCellNet();
           tech = cn.container.design.getTechnologyData();
           reff = gn.getDriveResistance(lastHalfOp)*tech.elmoreDelayFactor;
           wireCap = gn.getWireCapacitance();
        }

        if(b1){
            final String dstr = n >= 0 ? printDelay(delay[n]) 
                                       : printDelay(delay);
            final String sstr = n >= 0 ? printDelay(getSlack(n)) 
                                       : printDelay(slack);
            final String rstr =
                n >= 0 && getSlack(n) != slack[n] ? printDelay(slack[n]) : null;
            final String nstr;
            if (n >= 0) {
                gn = (GlobalNet) getEndNet().getGlobalNets().get(n);
                cn = gn.getTopCellNet();
                nstr = "// endnet=" + cn.container.typeName + "/" +
                       cn.canonicalName.getCadenceString() + "\n" +
                       (delaybias[n] != 1 ?
                           ("// delaybias=" +
                           NumberFormatter.format(delaybias[n], 2) + "\n")
                         : "");
            } else {
                nstr = "";
            }
            final double theslack = n >= 0 ? getSlack(n) : 0.0;
            final double budgetcap = reff > 0.0 ? theslack/reff+wireCap : 0.0;
            sb.append("// delay=" + dstr + "ps" + " slack=" + sstr + "ps" + 
                      (rstr == null ? "" : " real_slack=" + rstr + "ps") +
                      " avg_width=" + NumberFormatter.format(getAverageWidth()*1e6,3) + "um" +
                      " c_wire="+ NumberFormatter.format(wireCap*1e15,1)+"fF" +
                      " reff="+ NumberFormatter.format(reff,1)+
                      " budget_cap="+ NumberFormatter.format(budgetcap*1e15,1)+"fF\n" +
                      nstr);
        }

        sb.append(container.typeName + ": ");

        int j = path.size();

        for(int i=0;i<j;++i){
            HalfOperator hoa = (HalfOperator)path.get(i);

            sb.append(hoa.outputNet.canonicalName.getCadenceString());
            if (hoa.driveDirection == HalfOperator.DriveDirection.PULL_DOWN) {
                sb.append("- ");
            }
            else{
                sb.append("+ ");
            }
        }

        sb.append("\n");

        return sb.toString();
    }


    /**
     * @param prefix
     *        Prefix to be prepended before each line written.
     **/
    public void dumpInfo(BufferedWriter bw1, String prefix)
    {
        try{
            bw1.write(prefix + "SIZING_PATH _ {\n");

            if(endNet != null){
                bw1.write(prefix + "\tend_net = " + endNet.canonicalName.getCadenceString() + ";\n");
            }

            if(delay != null){
                bw1.write(prefix + "\tdelay = " + printDelay(delay) + ";\n");
                bw1.write(prefix + "\tslack = " + printDelay(slack) + ";\n");
                bw1.write(prefix + "\tcritical_net = " + criticalNet.canonicalName.getCadenceString() + ";\n");
                bw1.write(prefix + "\tave_width = " + getAverageWidth() + ";\n");

            }

            bw1.write(prefix + "\troute = (");

            int j = path.size();

            for(int i=0;i<j;++i){
                HalfOperator hoa = (HalfOperator)path.get(i);

                bw1.write("(" + hoa.outputNet.canonicalName.getCadenceString() + ",");
                if (hoa.driveDirection ==
                        HalfOperator.DriveDirection.PULL_DOWN) {
                    bw1.write("-,");
                }
                else{
                    bw1.write("+,");
                }

                if(i < (j-1)){
                    bw1.write(hoa.getDelayBias() + "), ");
                }
                else{
                    bw1.write(hoa.getDelayBias() + ")");
                }
            }

            bw1.write(");\n");

            bw1.write(prefix + "}\n");
        }
        catch(IOException e){
            e.printStackTrace(System.out);
        }
    }


    public double getAverageWidth()
    {
        double totalWidth = 0.0;

        for (Iterator ita = path.iterator(); ita.hasNext(); ) {
            HalfOperator hoa = (HalfOperator)ita.next();

            totalWidth += hoa.getCurrentSize();
        }

        return totalWidth / path.size();
    }


    public boolean isComponentsFixed() {
        return container.isFixedSize();
    }

    public CatPath popUp(boolean isEndNonObservable,
                         boolean hasObservable,
                         Set/*<CellNet>*/ nonObservableStartNets,
                         CellNet endNet) {
        SizingPath spb = new SizingPath(this);
        spb.setStartNets(nonObservableStartNets);
        spb.setEndNet(endNet);
        spb.copyFrom = this;
        spb.setIsFragment(false);

        //if(isEndNonObservable || !hasObservable){ // this sizing path can be removed from the sub-cell
        //    setIsFragment(true);
        //}
        setIsFragment(true);

        //if(isEndNonObservable && hasObservable){ // this sizing path should be one of the start sizing path in cat path
        //    spb.isReduced = true;
        //}
        if(hasObservable){ // this sizing path should be one of the start sizing path in cat path
            spb.isReduced = true;
        }

        spb.isEndNetNonObservable = isEndNonObservable; // is this a end sizing path for cat path?

        CatPath cpa = new CatPath();
        cpa.getCatPath().add(spb);
        cpa.setStartNets(nonObservableStartNets);
        cpa.setEndNet(endNet);
        cpa.setIsFragment(false);
        cpa.isReduced = spb.isReduced;
        cpa.isEndNetNonObservable = spb.isEndNetNonObservable;

        return cpa;
    }

    public HierName getInstanceName() {
        return instanceName;
    }

    public void prefixInstanceName(final /*@ non_null @*/ HierName prefix) {
        instanceName =
            instanceName == null ? prefix
                                 : HierName.append(prefix, instanceName);
    }

    void getPathString(final StringBuffer buf) {
        for (Iterator i = path.iterator(); i.hasNext(); ) {
            final HalfOperator op = (HalfOperator) i.next();
            getNetString(buf, op.outputNet);
            buf.append(
                op.driveDirection == HalfOperator.DriveDirection.PULL_DOWN ?
                '-' : '+');
            buf.append(' ');
        }
        getNetString(buf, getEndNet());
    }
}
