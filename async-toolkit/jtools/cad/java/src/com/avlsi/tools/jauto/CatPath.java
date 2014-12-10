/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import com.avlsi.fast.HalfOperator;
import com.avlsi.fast.CellType;
import com.avlsi.fast.CellNet;
import com.avlsi.fast.ConnectionInfo;
import com.avlsi.tools.lvs.NetGraph;

import java.util.Set;
import java.util.HashSet;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.LinkedHashSet;
import java.util.ArrayList;
import java.util.Iterator;
import java.io.*;
import com.avlsi.util.text.NumberFormatter;
import com.avlsi.util.container.CollectionUtils;

public class CatPath extends AbstractPath
{
    // list of SizingPath
    List/*<SizingPath>*/    path;


    /** Return the list of sizing paths */
    public List/*<SizingPath>*/ getCatPath()
    {
        return path;
    }


    /** Constructor 1 */
    public CatPath()
    {
        path = new ArrayList/*<SizingPath>*/();
        startNets = new HashSet/*<CellNet>*/();

        container = null;

        isFragment = false;
        isReduced = false;
        isEndNetNonObservable = false;

        delay = null;
        slack = null;
        criticalNet = null;
    }


    /** Constructor 2 */
    public CatPath(CatPath cp1)
    {
        path = new ArrayList/*<SizingPath>*/(cp1.path);
        container = cp1.container;

        startNets = new HashSet/*<CellNet>*/(cp1.startNets);
        endNet = cp1.endNet;

        isFragment = cp1.isFragment;
        isReduced = cp1.isReduced;
        isEndNetNonObservable = cp1.isEndNetNonObservable;

        delay = cp1.delay;
        slack = cp1.slack;
        criticalNet = cp1.criticalNet;
    }


    /**
     * Generate concatenated paths for all cells in <code>CellType</code>.
     * Generated cat paths will be added to
     * <code>cell.getCatPaths()</code> for each <code>cell</code> in
     * <code>cellTypes</code>.
     * XXX: Need better documentation.
     * 
     * @param cellTypes
     *        Set of cells for which cat paths should be generated.
     * @param ignoreReset
     *        Currently ignored.  At one time it specified whether
     *        cat paths starting with a reset node would be ignored.
     * @param completeCatPath
     *        XXX: Document.  Purpose unknown.
     * @param disableImmediateReduction
     *        If false, allows the immediate reduction of concatenated paths if
     *        a cell is fully observable.  If true, disables the immediate
     *        reduction.
     **/
    public static void generateCatPaths(Set/*<CellType>*/ cellTypes,
                                        boolean ignoreReset,
                                        boolean completeCatPath,
                                        boolean disableImmediateReduction)
    {
        List/*<CellType>*/ cellTypesWorkList
            = new ArrayList/*<CellType>*/();

        // Sort cell types by their level numbers from low to high, leaf-cells are excluded
        for (Iterator ita = cellTypes.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();

            if(DebugOption.printLevel <= 3){
                System.out.println("Concatenated path generation - sort celltypes");
                System.out.println(cella.toString());
            }


            if(cella.getLevel() > 0){
                sortedInsert(cellTypesWorkList, cella);
            }

        }


        // Generate concatenated paths for cell types


        /*
        // Pop up sizing paths for all the cells first
        for (Iterator ita = cellTypesWorkList.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();

            if(DebugOption.printLevel <= 3){
                System.out.println("Concatenated path generation - paths popup");
                System.out.println(cella.toString());
            }

            popUpPaths(cella);
        }
        */


        // Recursively generate concatenated paths for all the cells
        for (Iterator ita = cellTypesWorkList.iterator(); ita.hasNext(); ) {
            CellType cella = (CellType)ita.next();

            // Pop up sizing paths for all the cells first
            popUpPaths(cella);
            if(DebugOption.printLevel <= 3){
                System.out.println("********************************************************************");
                System.out.println("Processing cell type: ");
                //System.out.println(cella.toString());
                System.out.println("Number of paths popped up: " +
                                   cella.getSizingPaths().size());
                System.out.println("********************************************************************");
            }

            // get connectivity from cell net to sizing path
            Map/*<CellNet,Set<CatPath>>*/ startNetToCatPathsMap =
                new HashMap/*<CellNet,Set<CatPath>>*/();

            for (Iterator itb = cella.getSizingPaths().iterator(); itb.hasNext(); ) {
                CatPath cpa = (CatPath)itb.next();

                if(!cpa.isFragment()){
                    for (Iterator itc =
                            CollectionUtils.sort(CellNet.getComparator(),
                                                 cpa.getStartNets().iterator());
                         itc.hasNext(); ) {
                        CellNet startNet = (CellNet)itc.next();

                        Set/*<CatPath>*/ catPathsForStartNet =
                            (Set) startNetToCatPathsMap.get(startNet);
                        if (catPathsForStartNet == null) {
                            catPathsForStartNet =
                                new LinkedHashSet/*<CatPath>*/();
                            startNetToCatPathsMap.put(startNet,
                                                      catPathsForStartNet);
                        }
                        catPathsForStartNet.add(cpa);
                    }
                }
            }

            // get all non-observable end nets
            Set/*<CellNet>*/ nonObservableEndNets =
                new LinkedHashSet/*<CellNet>*/();
            for (Iterator itb = cella.getSizingPaths().iterator(); itb.hasNext(); ) {
                CatPath cpa = (CatPath)itb.next();

                if(cpa.isEndNetNonObservable){
                    CellNet endNet = cpa.getEndNet();
                    if (!endNet.isCutPath()) {
                        nonObservableEndNets.add(endNet);

                        if(DebugOption.printLevel <= 1){
                            System.out.println("Non-observable end net added: " 
                                + cpa.getEndNet().canonicalName.getCadenceString());
                        }
                    }
                }
            }


            // get all starting sizing path
            // Set/*<SizingPath>*/ sizingPathsCopy =
            //     new HashSet/*<SizingPath>*/(cella.getSizingPaths());

            Set/*<CatPath>*/ startingCatPaths = new LinkedHashSet/*<CatPath>*/();
            Set/*<CatPath>*/ setf = new HashSet/*<CatPath>*/();
            for (Iterator itb = cella.getSizingPaths().iterator(); itb.hasNext(); ) {
                CatPath cpa = (CatPath)itb.next();

                if(!cpa.isFragment()){

                    if(DebugOption.printLevel <= 1){
                        System.out.println("Concatenated path is not fragment.");
                    }

                    if(cpa.isReduced || cpa.getStartNets().isEmpty()){

                        if(DebugOption.printLevel <= 1){
                            System.out.println("Concatenated path added to start cat path. 1");
                        }

                        startingCatPaths.add(cpa);
                    }
                    else{
                        // int numStartNets = cpa.getStartNets().size();
                        for (Iterator itc =
                                CollectionUtils.sort(
                                    CellNet.getComparator(),
                                    cpa.getStartNets().iterator());
                             itc.hasNext(); ) {
                            CellNet startNet = (CellNet)itc.next();

                            if(DebugOption.printLevel <= 1){
                                System.out.println("Got starting net: ");
                                System.out.println(startNet.canonicalName.getCadenceString());
                            }

                            if(startNet.isPortNet()){
                                /*
                                if(ignoreReset){
                                    if (startNet.isReset() &&
                                        numStartNets > 1) {
                                        continue;
                                    }
                                }
                                */

                                if (startNet.portDirection == CellNet.INPUT) {

                                    if(DebugOption.printLevel <= 1){
                                        System.out.println("Concatenated path added to start cat path. 2");
                                    }

                                    startingCatPaths.add(cpa);
                                    break;
                                }

                                if (startNet.portDirection == CellNet.INPUTOUTPUT) {

                                    if(DebugOption.printLevel <= 1){
                                        System.out.println("Concatenated path added to start cat path. 3");
                                    }

                                    startingCatPaths.add(cpa);
                                    break;
                                }
                            }
                            else{
                                if(!nonObservableEndNets.contains(startNet)){

                                    if(DebugOption.printLevel <= 1){
                                        System.out.println("Concatenated path added to start cat path. 4");
                                    }

                                    startingCatPaths.add(cpa);
                                    break;
                                }
                            }
                        }


                        
                        /*
                        boolean drivenFromSameInstance = true;

                        for (Iterator itc = cpa.getStartNets().iterator(); itc.hasNext(); ) {
                            CellNet startNet = (CellNet)itc.next();

                            // XXX: SizingPaths or CatPaths?
                            for (Iterator ite = sizingPathsCopy.iterator();
                                 ite.hasNext(); ) {
                                CatPath cpb = (CatPath)ite.next();

                                if(cpb.instanceContainer != cpa.instanceContainer){
                                    if(cpb.endNet == startNet){
                                        drivenFromSameInstance = false;
                                        break;
                                    }
                                }
                            }

                            if(!drivenFromSameInstance){
                                break;
                            }
                        }

                        if(drivenFromSameInstance){
                            if(DebugOption.printLevel <= 3){
                                System.out.println("Concatenated path added to start cat path. 5");
                                System.out.println("Name of end net: " + cpa.getEndNet().canonicalName.getCadenceString());
                            }

                            startingCatPaths.add(cpa);
                            setf.add(cpa);
                        }
                        */
                        
                        

                    }
                }

            }


            Set/*<CatPath>*/ sete;
            if(completeCatPath){
                //if(!cella.isFragment()){
                //    sete = new HashSet/*<CatPath>*/(startingCatPaths);
                //}
                //else{
                    sete = new HashSet/*<CatPath>*/(setf);
                //}
            }
            else{
                sete = new HashSet/*<CatPath>*/(startingCatPaths);
            }

            boolean reduceNow = !disableImmediateReduction &&
                                !cella.hasNonObservablePorts();
            if(DebugOption.printLevel <= 3){
                System.out.println("Direct paths reduction for:" + cella.typeName + " : " + reduceNow);
            }

            long[] numCatPaths = {0};

            Set/*<CatPath>*/ allCatPaths = new LinkedHashSet/*<CatPath>*/();

            for (Iterator itb = startingCatPaths.iterator();
                 itb.hasNext(); ) {
                CatPath cpa = (CatPath)itb.next();

                recursiveCatPaths(cpa, new ArrayList/*<CatPath>*/(),
                                  allCatPaths, sete, startNetToCatPathsMap,
                                  reduceNow, numCatPaths, cella);
            }


            for (Iterator itc = allCatPaths.iterator(); itc.hasNext(); ) {
                CatPath cpa = (CatPath)itc.next();
                cpa.setStartEndNets(completeCatPath);
            }


            cella.getCatPaths().addAll(allCatPaths);

            if(DebugOption.printLevel <= 3){
                System.out.println("\n****************  " + cella.typeName + "  **************");
                System.out.println("Total number of original concatenated paths: " +
                                   allCatPaths.size());
                System.out.println("**********************************************************");
            }


        }

    }


    /**
     * Recursively join cat paths together.  
     * Every possible sequence of cat paths starting with
     * <code>nextCatPath</code> and using cat paths from
     * <code>startNetToCatPathsMap</code> is constructed subject to the
     * constraint that the <code>i</code>-th cat path's end net is the
     * same as the <code>i + 1</code>-th cat paths's start net and
     * the last half-operator of the <code>i</code>-th cat path has the
     * opposite direction as the first half-operator of the
     * <code>i + 1</code>-th cat path.  Further, no cat path may appear
     * more than once in the sequence (concatenation is stopped when
     * this happens).  Reaching a port net, cut path net, observable
     * net, or cat path in <code>set2</code> also stops the sequence.
     * Each of these sequence of cat paths is flattened into a single
     * cat paths and added to <code>allCatPaths</code>.  
     * <code>numCatPaths</code> is incremented for each cat path added
     * (even if reduced), and a debug message is possibly printed.
     *
     * <!--  False starts with alternative wordings:
     * (Maybe these would be clearer?)
     * All possible combinations
     * of cat paths from <code>startNetToCatPathsMap</code> that start
     * with <code>nextCatPath</code> and whose start net is the same
     * as ...
     * Starting with <code>nextCatPath</code>, all cat paths from
     * <code>startNetToCatPathsMap</code> having a start net the same
     * as <code>nextCatPath<code>'s end net and whose first half operator
     * has opposite direction from <code>nextCatPath</code>'s last are
     * appended to <code>catPathsToBeCatted</code>.  This method
     * is then called on the all new cat paths.
     * -->
     *
     * @param nextCatPath
     *        The cat path to be concatenated onto
     *        <code>catPathsToBeCatted</code>.
     * @param catPathsToBeCatted
     *        A list of cat paths that represents the cat path that is being
     *        concatenated together.  <code>nextCatPath</code> will be
     *        added to the end of this list and when the cat path is ready
     *        to be terminated, this <code>List&lt;CatPath&gt;</code> will
     *        be flattened into a single <code>CatPath</code>.
     * @param allCatPaths
     *        All cat paths for the cell.  New cat paths will be added
     *        to this list.
     * @param set2
     *        TODO: Document!  Purpose unknown!
     *        Not modified by this method.
     * @param startNetToCatPathsMap
     *        Map from starting net of the cat paths in the cell to the
     *        cat paths themselves.  Not modified by this method.
     * @param reduceNow
     *        Specifies whether added cat paths should be immediately
     *        reduced by {@link #addCatPath}.
     * @param numCatPaths
     *        Array used as a reference that is incremented each time a
     *        cat path is added to <code>allCatPaths</code>.
     *        Incremented even if <code>reduceNow</code> is used.
     * @param ct1
     *        The cell type for which the cat paths are generated.
     *        The only use of this is a use of the <code>typeName</code>
     *        member in debugging printouts.
     **/
    private static void recursiveCatPaths(CatPath nextCatPath,
                                         List/*<CatPath>*/
                                             catPathsToBeCatted,
                                         Set/*<CatPath>*/ allCatPaths,
                                         Set/*<CatPath>*/ set2, 
                                         Map/*<CellNet,Set<CatPath>>*/
                                             startNetToCatPathsMap,
                                         boolean reduceNow,
                                         long[] numCatPaths,
                                         CellType ct1)
    {
        // I believe the following assertion is true because of the check
        // newCatPathsToBeCatted.contains(potentialNewNextCatPath) when
        // we are adding to newNextCatPaths.  --jmr
        // If these assertions are indeed true, where is the best place
        // to handle this, here or before the recursion?
        assert !catPathsToBeCatted.contains(nextCatPath);
        assert !set2.contains(nextCatPath);

        if (catPathsToBeCatted.contains(nextCatPath) ||
            (set2.contains(nextCatPath) && !catPathsToBeCatted.isEmpty())) {
            CatPath cpa = joinCatPaths(catPathsToBeCatted);
            addCatPath(allCatPaths, cpa, reduceNow);

            numCatPaths[0]++;
            debugOutput(catPathsToBeCatted, allCatPaths, cpa,
                        numCatPaths, ct1);

            return;
        }

        List/*<CatPath>*/ newCatPathsToBeCatted =
            new ArrayList/*<CatPath>*/(catPathsToBeCatted.size() + 1);
        newCatPathsToBeCatted.addAll(catPathsToBeCatted);
        newCatPathsToBeCatted.add(nextCatPath);

        CellNet endNet = nextCatPath.getEndNet();

        if (endNet.isPortNet() || endNet.isCutPath() ||
            !nextCatPath.isEndNetNonObservable) {
            CatPath cpa = joinCatPaths(newCatPathsToBeCatted);
            addCatPath(allCatPaths, cpa, reduceNow);

            numCatPaths[0]++;
            debugOutput(catPathsToBeCatted, allCatPaths, cpa,
                        numCatPaths, ct1);

            return;
        }

        final Set/*<CatPath>*/ catPathsStartedByEndNet =
            (Set) startNetToCatPathsMap.get(endNet);
        if (catPathsStartedByEndNet != null) {
            boolean isCut = false;
            Set/*<CatPath>*/ newNextCatPaths = new LinkedHashSet/*<CatPath>*/();
            for (Iterator ita = catPathsStartedByEndNet.iterator();
                 ita.hasNext(); ) {
                CatPath potentialNewNextCatPath = (CatPath)ita.next();

                boolean visited = false;
                /*
                for (Iterator itb = newCatPathsToBeCatted.iterator();
                     itb.hasNext(); ) {
                    CatPath cpb = (CatPath)itb.next();

                    if (potentialNewNextCatPath.instanceContainer ==
                            cpb.instanceContainer){
                        visited = true;
                        break;
                    }
                }
                */

                if (newCatPathsToBeCatted.contains(potentialNewNextCatPath) ||
                    set2.contains(potentialNewNextCatPath) || visited) {
                    isCut = true;
                }
                else{
                    List/*<SizingPath>*/ sizingPaths = nextCatPath.getCatPath();

                    // last half operator of last sizing path of nextCatPath
                    SizingPath spa =
                        (SizingPath) sizingPaths.get(sizingPaths.size() - 1);
                    HalfOperator hoa = (HalfOperator) spa.getPath().get(spa.getPath().size() - 1);

                    // first half operator of first sizing path of
                    // potentialNewNextCatPath
                    SizingPath spb =
                        (SizingPath) potentialNewNextCatPath
                                         .getCatPath().get(0);
                    HalfOperator hob = (HalfOperator)spb.getPath().get(0);

                    if (hoa.driveDirection != hob.driveDirection)
                        newNextCatPaths.add(potentialNewNextCatPath);
                }
            }

            if (newNextCatPaths.isEmpty() || isCut) {
                CatPath cpa = joinCatPaths(newCatPathsToBeCatted);
                addCatPath(allCatPaths, cpa, reduceNow);

                numCatPaths[0]++;
                debugOutput(catPathsToBeCatted, allCatPaths, cpa,
                            numCatPaths, ct1);
            }

            for (Iterator ita = newNextCatPaths.iterator();
                 ita.hasNext(); ) {
                CatPath cpa = (CatPath)ita.next();

                recursiveCatPaths(cpa, newCatPathsToBeCatted, allCatPaths,
                                  set2, startNetToCatPathsMap, reduceNow,
                                  numCatPaths, ct1);
            }
        }
        else{
            CatPath cpa = joinCatPaths(newCatPathsToBeCatted);
            addCatPath(allCatPaths, cpa, reduceNow);

            numCatPaths[0]++;
            debugOutput(catPathsToBeCatted, allCatPaths, cpa,
                        numCatPaths, ct1);
        }
    }


    /**
     * Pops up sizing paths or cat paths with non-observable start or end nodes
     * from children of <code>cell1</code> to <code>cell1</code>.
     * XXX: need better documentation
     *
     * @param cell1
     *        Cell into which child paths will be popped up.
     **/
    private static void popUpPaths(CellType cell1)
    {
        for (Iterator ita =
                CollectionUtils.sort(ConnectionInfo.getComparator(),
                                     cell1.getAllInstances().iterator());
             ita.hasNext(); ) {
            ConnectionInfo cia = (ConnectionInfo)ita.next();

            if(DebugOption.printLevel <= 1){
                System.out.println("\nGot instance: ");
                System.out.println(cia.toString());
            }

            CellType childCell = cia.child;
            Set/*<AbstractPath>*/ paths;
            if (childCell.getLevel() == 0)
                paths = childCell.getSizingPaths();
            else
                paths = childCell.getCatPaths();

            for (Iterator itb = paths.iterator(); itb.hasNext(); ) {
                AbstractPath path = (AbstractPath) itb.next();

                Set/*<CellNet>*/ nonObservableStartNets =
                    new HashSet/*<CellNet>*/();
                boolean hasNonObservable = false;
                boolean hasObservable = false;

                for (Iterator itc = path.getStartNets().iterator(); itc.hasNext(); ) {
                    CellNet startNet = (CellNet)itc.next();

                    if (startNet.isObservable()) {
                        if(DebugOption.printLevel <= 1){
                            System.out.println("\nObservable starting net found: ");
                            System.out.println(startNet.canonicalName.getCadenceString());
                        }

                        hasObservable = true;
                    } else {
                        hasNonObservable = true;

                        nonObservableStartNets
                            .addAll(cia.getParentNet(startNet));
                        
                        if(DebugOption.printLevel <= 1){
                            System.out.println("\nNon-observable starting net found: ");
                            System.out.println(startNet.canonicalName.getCadenceString());
                            // System.out.println(nonObservableStartNets.toString());
                        }

                    }
                }

                CellNet childEndNet = path.getEndNet();
                Set/*<CellNet>*/ parentNets =
                    cia.getParentNet(childEndNet);

                assert parentNets.size() <= 1
                    : "Output (or In/Out) port connected to more than 1 nets" +
                      childEndNet.canonicalName.getCadenceString();

                CellNet endNet;
                if (parentNets.size() > 0)
                    endNet = (CellNet) parentNets.iterator().next();
                else
                    endNet = childEndNet;

                final boolean isEndNonObservable =
                    !childEndNet.isObservable();

                if (DebugOption.printLevel <= 1) {
                    if (isEndNonObservable) {
                        System.out.println("\nNon-observable ending net found: ");
                        System.out.println(childEndNet.canonicalName.getCadenceString());
                        System.out.println(endNet.canonicalName.getCadenceString());
                    } else {
                        System.out.println("\nObservable ending net found: ");
                        System.out.println(childEndNet.canonicalName.getCadenceString());
                    }
                }

                if(hasNonObservable || isEndNonObservable){ // there are non-observable net(s)

                    CatPath cpa = path.popUp(isEndNonObservable,
                                            hasObservable,
                                            nonObservableStartNets,
                                            endNet);
                    for (Iterator k = cpa.getCatPath().iterator();
                         k.hasNext(); ) {
                        final SizingPath sp = (SizingPath) k.next();
                        sp.prefixInstanceName(cia.nameInParent);
                    }

                    // XXX: Why is a CatPath being added to
                    // getSizingPaths(), which is supposed to be
                    // a Set<SizingPath>?
                    cell1.getSizingPaths().add(cpa);

                }

            }

        }

    }


    public void setStartEndNets(boolean completeCatPath)
    {
        int numSizingPaths = path.size();

        if (numSizingPaths > 0) {
            endNet =
                ((SizingPath) path.get(numSizingPaths - 1)).getEndNet();

            startNets.clear();
            for (Iterator ita = ((SizingPath) path.get(0)).getStartNets().iterator(); ita.hasNext(); ) {
                CellNet startNet = (CellNet)ita.next();

                if(completeCatPath){
                    if(startNet.container.isFragment()){ // only add port net for fragment cells
                        if(startNet.isPortNet()){
                            startNets.add(startNet);
                        }
                    }
                    else{
                        startNets.add(startNet);
                    }
                }
                else{
                    startNets.add(startNet);
                }
            }

            container = endNet.container;
        }
    }



    public String toString()
    {
        StringBuffer sb = new StringBuffer();

        for (Iterator ita = path.iterator(); ita.hasNext(); ) {
            SizingPath spa = (SizingPath)ita.next();
            
            sb.append(spa.container.typeName + ": ");
            sb.append(spa.toString());
            sb.append("\n");
        }

        return sb.toString();
    }


    public String printPath() {
        return printPath(-1);
    }

    private static Object lastElement(List path) {
        HalfOperator hoa = null;
        for (Iterator ita = path.iterator(); ita.hasNext(); ) {
            SizingPath spa = (SizingPath)ita.next();

            for (Iterator itb = spa.getPath().iterator(); itb.hasNext(); ) {
                hoa = (HalfOperator)itb.next();
            }
        }
        return hoa;
    }

    public String printPath(int n)
    {
        StringBuffer sb = new StringBuffer();

        final String dstr = n >= 0 ? printDelay(delay[n]) : printDelay(delay);
        final String sstr = n >= 0 ? printDelay(getSlack(n))
                                   : printDelay(slack);
        final String rstr =
            n >= 0 && getSlack(n) != slack[n] ? printDelay(slack[n]) : null;

       final HalfOperator lastHalfOp = (HalfOperator) lastElement(path);
       GlobalNet gn;
       CellNet cn;
       TechnologyData tech;
       double[] effectiveResistanceFactors;
       double effectiveResistanceFactor;
       double f = 0.0;
       double wireCap = 0.0;
       double reff = 0.0;
       if (n >= 0) {
           gn = (GlobalNet) getEndNet().getGlobalNets().get(n);
           cn = gn.getTopCellNet();
           tech = cn.container.design.getTechnologyData();
           reff = gn.getDriveResistance(lastHalfOp)*tech.elmoreDelayFactor;
           wireCap = gn.getWireCapacitance();
        }

        final String nstr;
        if (n >= 0) {
            gn =
                (GlobalNet) getEndNet().getGlobalNets().get(n);
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

        for (Iterator ita = path.iterator(); ita.hasNext(); ) {
            SizingPath spa = (SizingPath)ita.next();

            sb.append(spa.printPath(false));

        }

        return sb.toString();
    }


    public void dumpInfo(BufferedWriter bw1, String s1)
    {
        try{
            bw1.write(s1 + "CONCATENATED_PATH _ {\n");
            // bw1.write(s1 + "\tisFragment = " + isFragment + ";\n\n");

            if(delay != null){
                bw1.write(s1 + "\tdelay = " + printDelay(delay) + ";\n");
                bw1.write(s1 + "\tslack = " + printDelay(slack) + ";\n");
                bw1.write(s1 + "\tcritical_net = " + criticalNet.canonicalName.getCadenceString() + ";\n");
                bw1.write(s1 + "\tave_width = " + getAverageWidth() + ";\n");
            }

            for (Iterator ita = path.iterator(); ita.hasNext(); ) {
                SizingPath spa = (SizingPath)ita.next();

                bw1.write(s1 + "\tCATPATH_CELL " + spa.container.typeName + " {\n");
                spa.dumpInfo(bw1, s1 + "\t\t");
                bw1.write(s1 + "\t}\n");
            }

            bw1.write(s1 + "}\n");
        }
        catch(IOException e){
            e.printStackTrace(System.out);
        }
    }


    public double getAverageWidth()
    {
        int numHalfOperators = 0;
        double totalWidth = 0.0;

        for (Iterator ita = path.iterator(); ita.hasNext(); ) {
            SizingPath spa = (SizingPath)ita.next();

            for (Iterator itb = spa.getPath().iterator(); itb.hasNext(); ) {
                HalfOperator hoa = (HalfOperator)itb.next();

                totalWidth += hoa.getCurrentSize();
                numHalfOperators++;
            }
        }

        return totalWidth / numHalfOperators;
    }


    /**
     * Inserts <code>st1</code> into <code>lst1</code>.  Leaf cells
     * appear at the front of the list and high-level cells at the end.
     **/
    private static void sortedInsert(List/*<CellType>*/ lst1, CellType st1)
    {
        int k = st1.getLevel(); // get the level number of the sub-type
        int j = lst1.size();
        for(int i=0;i<j;++i){
            CellType sta = (CellType)lst1.get(i);
            int l = sta.getLevel();

            if(l == k){
                if(sta == st1){ // the sub-type is already in the list
                    return;
                }
            }

            if(l > k){
                lst1.add(i, st1); // insert this sub-type at index i
                return;
            }
        }

        lst1.add(st1); // add this sub-type to the end of the list
    }


    /**
     * Reduce cat paths for <code>cellTypes</code> by combining cat paths
     * with similar loads into "worst case" cat paths.  A cell's cat paths
     * are partitioned into sets of cat paths having the same type (as
     * specified by {@link #isSameTypeAs}) and loads differing by less than
     * <code>threshold</code> (as specified by {@link #isLoadCloseTo}).  
     * For each set, a new reduced cat path is constructed element by
     * element from the component sizing paths that have the worst case
     * load.
     *
     * @param cellTypes
     *        set of cell types whose cat paths are to be reduced.
     * @param threshold
     *        acceptable relative load difference for cat path reduction.
     *        If threshold is less than 0, then do not attempt path
     *        reduction at all.  See {@link #isLoadCloseTo}.
     **/
    public static void reduceCatPaths(Set/*<CellType>*/ cellTypes,
                                      double threshold)
    {
        for (Iterator ita = cellTypes.iterator(); ita.hasNext(); ) {
            CellType cta = (CellType)ita.next();

            if(DebugOption.printLevel <= 3){
                System.out.println("CatPath reduction");
                System.out.println("Processing cell: " + cta.typeName);
            }

            // All cat paths in the same list of cat paths are the same
            // type as the first one and have load close to the first in
            // the same list.  The cat paths in one list will never be
            // both of the same type and have load close to the first
            // cat path in a different list.
            // 
            // That is to say the invariant for similarCatPathsList is:
            // (\forall int i,j,k; 0 <= i && i < similarCatPathsList.size() &&
            //                     0 <= j && j < similarCatPathsList.size() &&
            //                     0 <= k && k < similarCatPathsList.get(j).size();
            //          (similarCatPathsList.get(j).get(k)
            //               .isSameTypeAs(similarCatPathsList.get(i).get(0)) &&
            //           similarCatPathsList.get(j).get(k)
            //               .isLoadCloseTo(similarCatPathsList.get(i).get(0))) <==>
            //          i == j)
            List/*<List<CatPath>>*/ similarCatPathsList =
                new ArrayList/*<List<CatPath>>*/();

            // group similar cat paths together

            for (Iterator itb = cta.getCatPaths().iterator(); itb.hasNext(); ) {
                CatPath cpa = (CatPath)itb.next();
                // if(DebugOption.printLevel <= 3){
                    // System.out.println("Processing path: ");
                    // System.out.println(cpa.toString());
                // }


                if(!cpa.isFragment()){
                    boolean found = false;
                    if (threshold >= 0) {
                        for (Iterator itc = similarCatPathsList.iterator();
                             itc.hasNext(); ) {
                            List/*<CatPath>*/ similarCatPaths =
                                (List) itc.next();

                            CatPath headCatPath =
                                (CatPath) similarCatPaths.get(0);
                            if (cpa.isSameTypeAs(headCatPath)) {
                                if(DebugOption.printLevel <= 3){
                                    System.out.println("Found concatenated paths of same type");
                                    // System.out.println(cpa.toString());
                                }

                                if (cpa.isLoadCloseTo(headCatPath, threshold)) {
                                    found = true;

                                    if(DebugOption.printLevel <= 3){
                                        System.out.println("Found concatenated path to be reduced");
                                        // System.out.println(cpa.toString());
                                    }

                                    similarCatPaths.add(cpa);
                                    break;
                                }
                            }
                        }
                    }

                    if(!found){
                        List/*<CatPath>*/ similarCatPaths =
                            new ArrayList/*<CatPath>*/();
                        similarCatPaths.add(cpa);
                        similarCatPathsList.add(similarCatPaths);
                    }
                }
            }


            // Now, form one cat path with worst case load for each group
            // of similar cat paths.

            Set/*<CatPath>*/ reducedCatPaths = cta.getReducedCatPaths();
            reducedCatPaths.clear();

            for (Iterator itb = similarCatPathsList.iterator();
                 itb.hasNext(); ) {
                List/*<CatPath>*/ similarCatPaths = (List) itb.next();
                int numSimilarCatPaths = similarCatPaths.size();

                CatPath reducedCatPath;
                if(numSimilarCatPaths == 1){
                    reducedCatPath =
                        new CatPath((CatPath) similarCatPaths.get(0));
                }
                else{

                    CatPath headCatPath = (CatPath) similarCatPaths.get(0);
                    int numSizingPaths = headCatPath.getCatPath().size();

                    reducedCatPath = new CatPath(headCatPath);
                    reducedCatPath.getCatPath().clear();

                    for(int i=0;i<(numSizingPaths-1);++i){
                        double maxWireRc = -1.0;
                        SizingPath worstSizingPath = null;

                        for(int j=0;j<numSimilarCatPaths;++j){
                            CatPath cpc = (CatPath) similarCatPaths.get(j);
                            SizingPath spb = (SizingPath)cpc.getCatPath().get(i);

                            CellNet endNet = spb.getEndNet();
                            List/*<GlobalNet>*/ globalNets =
                                endNet.getGlobalNets();
                            assert globalNets.size() == 1
                                : "Invalid number of global nets 1a: " +
                                  globalNets.size();

                            double wireRc =
                                ((GlobalNet) globalNets.get(0))
                                    .getWireRC();

                            if(wireRc > maxWireRc){
                                maxWireRc = wireRc;
                                worstSizingPath = spb;
                            }
                        }

                        reducedCatPath.getCatPath().add(new SizingPath(worstSizingPath));
                    }

                    int i = numSizingPaths-1;

                    double maxWireRc = -1.0;
                    SizingPath worstSizingPath = null;

                    for(int j=0;j<numSimilarCatPaths;++j){
                        CatPath cpc = (CatPath)similarCatPaths.get(j);
                        SizingPath spb = (SizingPath)cpc.getCatPath().get(i);

                        CellNet endNet = spb.getEndNet();
                        List/*<GlobalNet>*/ globalNets =
                            endNet.getGlobalNets();
                        assert !globalNets.isEmpty();

                        double maxGlobalNetWireRc = -1.0;
                        for (Iterator itd = globalNets.iterator();
                             itd.hasNext(); ) {
                            GlobalNet gna = (GlobalNet)itd.next();

                            double wireRc = gna.getWireRC();

                            if (wireRc > maxGlobalNetWireRc)
                                maxGlobalNetWireRc = wireRc;
                        }

                        if(maxGlobalNetWireRc > maxWireRc){
                            maxWireRc = maxGlobalNetWireRc;
                            worstSizingPath = spb;
                        }
                    }

                    reducedCatPath.getCatPath()
                                  .add(new SizingPath(worstSizingPath));
                }

                reducedCatPaths.add(reducedCatPath);
            }

            if(DebugOption.printLevel <= 3){
                System.out.println("\n****************  " + cta.typeName + "  **************");
                System.out.println("Total number of reduced concatenated paths: " +
                                   reducedCatPaths.size());
                System.out.println("**********************************************************");
            }
        }

    }


    /**
     * Determines if this cat path has similar end global nets.  They are
     * considered similar if they are made up of the same number of 
     * sizing paths, and each sizing path has the same
     * <code>copyFrom</code> field and each ending global net
     * of the sizing path has a corresponding global net in the
     * other sizing path according to {@link GlobalNet#isSameTypeAs}.
     **/
    private boolean isSameTypeAs(CatPath cp1)
    {
        int numSizingPaths = cp1.getCatPath().size();

        if (numSizingPaths != path.size())
            return false;

        for(int i=0;i<numSizingPaths;++i){
            SizingPath spa = (SizingPath)cp1.getCatPath().get(i);
            SizingPath spb = (SizingPath)path.get(i);

            if (spa.copyFrom != spb.copyFrom)
                return false;

            List/*<GlobalNet>*/ globalNetsA =
                (List) spa.getEndNet().getGlobalNets();
            List/*<GlobalNet>*/ globalNetsB =
                (List) spb.getEndNet().getGlobalNets();
            if(i < (numSizingPaths-1)){
                // not the last sizing path
                assert globalNetsA.size() == 1
                    : "Invalid number of global nets 1b: " +
                      globalNetsA.size() + "\n" +
                      spa.getEndNet().toString();

                assert globalNetsB.size() == 1
                    : "Invalid number of global nets 1c: " +
                      globalNetsB.size() + "\n" +
                      spb.getEndNet().toString();

                GlobalNet gna = (GlobalNet) globalNetsA.get(0);
                GlobalNet gnb = (GlobalNet) globalNetsB.get(0);

                if(!gna.isSameTypeAs(gnb)){
                    //if(DebugOption.printLevel <= 3){
                        //System.out.println("Different types of global net.");
                        //System.out.println(gna.getTopCellNet().canonicalName.getCadenceString());
                        //System.out.println(gnb.getTopCellNet().canonicalName.getCadenceString());
                        // System.out.println(spa.getEndNet().toString());
                        //System.out.println(gna.toString());
                        // System.out.println(spb.getEndNet().toString());
                        //System.out.println(gnb.toString());
                    //}
                    return false;
                }
            }
            else{
                // the last sizing path
                if (globalNetsA.size() != globalNetsB.size())
                    return false;

                List/*<GlobalNet>*/ globalNetsBCopy =
                    new ArrayList/*<GlobalNet>*/(globalNetsB);

                for (Iterator ita = globalNetsA.iterator(); ita.hasNext(); ) {
                    GlobalNet gna = (GlobalNet)ita.next();
                    boolean found = false;

                    for (Iterator itb = globalNetsBCopy.iterator();
                         itb.hasNext(); ) {
                        GlobalNet gnb = (GlobalNet)itb.next();

                        if(gna.isSameTypeAs(gnb)){
                            found = true;
                            itb.remove();
                            break;
                        }
                    }
                    if (!found)
                        return false;
                }
                if (!globalNetsBCopy.isEmpty())
                    return false;
            }
        }

        return true;
    }


    /**
     * Determines if <code>cp1</code> has similar load to this one.
     * They are considered similar if the "total max wire rc"
     * differs by less than <code>threshold</code> from this one's.
     * The "total max wire rc" is the sum of the maximum wire
     * rcs for the global nets ending the contained sizing paths.
     * (There is only one ending global net for all but the last
     * sizing path.)  The relative difference is computed as
     * <code>Math.abs(totalRcA - totalRcB) / totalRcA</code>.
     *
     * <p> This method is only meaningful if the cat paths are
     * the same type according to {@link isSameTypeAs}.
     *
     * <p>XXX: Review handling of unfloorplanned nets
     * (<code>rc == 0.0</code>) and general soundness
     * of cat path reduction.
     * 
     * @pre isSameTypeAs(cp1)
     *
     * @param cp1
     *        The cat path whose wire rc is to be compared to this one.
     * @param threshold
     *        The fraction by which the total wire rc of
     *        <code>cp1</code> is allowed to differ from the total wire
     *        rc of this cat path.
     **/
    private boolean isLoadCloseTo(CatPath cp1, double threshold)
    {
        int numSizingPaths = cp1.getCatPath().size();

        // isSameTypeAs() ==> same sizes, and isSameTypeAs() is a
        // precondition for this method
        assert numSizingPaths == path.size();

        double totalWireRcA = 0.0;
        double totalWireRcB = 0.0;

        //double maxRelRcDiff = -1.0;

        for (int i = 0; i < numSizingPaths - 1; ++i) {
            SizingPath spa = (SizingPath)cp1.getCatPath().get(i);
            SizingPath spb = (SizingPath)path.get(i);

            List/*<GlobalNet>*/ endGlobalNetsA =
                spa.getEndNet().getGlobalNets();
            List/*<GlobalNet>*/ endGlobalNetsB =
                spb.getEndNet().getGlobalNets();

            assert endGlobalNetsA.size() == 1;
            assert endGlobalNetsB.size() == 1;

            double rca = ((GlobalNet) endGlobalNetsA.get(0))
                              .getWireRC();
            double rcb = ((GlobalNet) endGlobalNetsB.get(0))
                              .getWireRC();

            totalWireRcA += rca;
            totalWireRcB += rcb;

            //double relRcDiff = relativeRcDifference(rca, rcb);

            //if (relRcDiff > maxRelRcDiff)
            //    maxRelRcDiff = relRcDiff;
        }

        SizingPath spa = (SizingPath)cp1.getCatPath().get(numSizingPaths-1);
        SizingPath spb = (SizingPath)path.get(numSizingPaths-1);

        CellNet endNetA = spa.getEndNet();
        List/*<GlobalNet>*/ endGlobalNetsA = endNetA.getGlobalNets();

        assert !endGlobalNetsA.isEmpty();

        CellNet endNetB = spb.getEndNet();
        List/*<GlobalNet>*/ endGlobalNetsB =
            new ArrayList/*<GlobalNet>*/(endNetB.getGlobalNets());

        assert !endGlobalNetsB.isEmpty();

        if(DebugOption.printLevel <= 3){
            System.out.println("Comparing cell nets: "
                + endNetA.canonicalName.getCadenceString() 
                + " and " + endNetB.canonicalName.getCadenceString());
        }

        if (endGlobalNetsA.size() != endGlobalNetsB.size())
            return false;

        double maxWireRcA = -1.0;
        double maxWireRcB = -1.0;

        for (Iterator ita = endGlobalNetsA.iterator();
             ita.hasNext(); ) {
            GlobalNet gna = (GlobalNet)ita.next();

            double wireRcA = gna.getWireRC();
            if (wireRcA > maxWireRcA)
                maxWireRcA = wireRcA;

            boolean found = false;

            for (Iterator itb = endGlobalNetsB.iterator(); itb.hasNext(); ) {
                GlobalNet gnb = (GlobalNet)itb.next();

                double wireRcB = gnb.getWireRC();
                if (wireRcB > maxWireRcB)
                    maxWireRcB = wireRcB;

                if(gna.isSameTypeAs(gnb)){
                    found = true;
                    itb.remove();
                    break;
                }
            }

            if (!found)
                return false;
        }

        if (!endGlobalNetsB.isEmpty())
            return false;

        totalWireRcA += maxWireRcA;
        totalWireRcB += maxWireRcB;
            
        //double relRcDiff =
        //    relativeRcDifference(maxWireRcA, maxWireRcB);

        //if (relRcDiff > maxRelRcDiff)
        //    maxRelRcDiff = relRcDiff;

        // REVIEW: Why was this commented out?
        //if(maxRelRcDiff > threshold){
        //    return false;
        //}

        //if (DebugOption.printLevel <= 3)
        //    System.out.println("Maximum relative difference: " +
        //                       maxRelRcDiff);


        double avgRelRcDiff =
            relativeRcDifference(totalWireRcA, totalWireRcB);

        if (DebugOption.printLevel <= 3)
            System.out.println("Average relative difference: " + avgRelRcDiff);

        if (avgRelRcDiff > threshold)
            return false;

        return true;
    }

    /**
     * Computes the relative rc difference, defined as
     * <code>Math.abs(rcb - rca) / rca</code>, unless
     * <code>rca == 0.0 &amp;&amp; rcb == 0.0</code> in which
     * case the relative difference is <code>0.0</code>.
     **/
    private static final double relativeRcDifference(double rca,
                                                         double rcb) {
        if (rca == 0.0 && rcb == 0.0)
            return 0.0;
        else
            return Math.abs(rcb - rca) / rca;
    }
    

    private static CatPath joinCatPaths(List/*<CatPath>*/ lst1)
    {
        CatPath cp1 = new CatPath();
        // Strip out extra CatPath wrapper.

        List/*<SizingPath>*/ lstc = cp1.getCatPath();

        for (Iterator ita = lst1.iterator(); ita.hasNext(); ) {
            CatPath cpa = (CatPath)ita.next();

            List/*<SizingPath>*/ lsta = cpa.getCatPath();

            int numSizingPaths = lsta.size();

            // If List.get throws exception, the error is:
            // No sizing path in concatenated path.
            SizingPath spa = new SizingPath((SizingPath)lsta.get(0));
            spa.setStartNets(cpa.getStartNets());

            if(numSizingPaths == 1){
                spa.setEndNet(cpa.getEndNet());
                lstc.add(spa);
            }
            else{
                lstc.add(spa);

                for(int i=1;i<(numSizingPaths-1); ++i){
                    spa = new SizingPath((SizingPath)lsta.get(i));
                    lstc.add(spa);
                }

                spa = new SizingPath((SizingPath)lsta.get(numSizingPaths-1));
                spa.setEndNet(cpa.getEndNet());

                lstc.add(spa);
            }
        }

        return cp1;
    }


    private static void addCatPath(Set/*<CatPath>*/ allCatPaths,
                                  CatPath newCatPath,
                                  boolean reduceNow)
    {
        if(reduceNow){
            boolean found = false;

            for (Iterator ita = allCatPaths.iterator(); ita.hasNext(); ) {
                CatPath oldCatPath = (CatPath)ita.next();

                if(oldCatPath.isSameTypeAs(newCatPath)){
                    found = true;

                    List/*<SizingPath>*/ oldSizingPaths =
                        oldCatPath.getCatPath();
                    List/*<SizingPath>*/ newSizingPaths =
                        newCatPath.getCatPath();

                    // isSameTypeAs ==>
                    //     oldSizingPaths.size() == newSizingPaths().size
                    int numSizingPaths = oldSizingPaths.size();
                    for (int i = 0; i < numSizingPaths; ++i) {
                        SizingPath oldSizingPath =
                            (SizingPath) oldSizingPaths.get(i);
                        List/*<GlobalNet>*/ oldGlobalNets =
                            oldSizingPath.getEndNet().getGlobalNets();

                        SizingPath newSizingPath =
                            (SizingPath) newSizingPaths.get(i);
                        List/*<GlobalNet>*/ newGlobalNets =
                            newSizingPath.getEndNet().getGlobalNets();

                        if (i < numSizingPaths - 1) {
                            GlobalNet gna = (GlobalNet) oldGlobalNets.get(0);
                            double oldWireRc =
                                gna.getWireRC();

                            GlobalNet gnb = (GlobalNet) newGlobalNets.get(0);
                            double newWireRc =
                                gnb.getWireRC();

                            if (newWireRc > oldWireRc)
                                oldSizingPaths.set(i, newSizingPath);
                        }
                        else{
                            double maxOldWireRc = -1.0;
                            for (Iterator itb = oldGlobalNets.iterator(); itb.hasNext(); ) {
                                GlobalNet gna = (GlobalNet)itb.next();
                                double oldWireRc =
                                    gna.getWireRC();
                                if (oldWireRc > maxOldWireRc)
                                    maxOldWireRc = oldWireRc;
                            }
                            
                            double maxNewWireRc = -1.0;
                            for (Iterator itb = newGlobalNets.iterator(); itb.hasNext(); ) {
                                GlobalNet gnb = (GlobalNet)itb.next();
                                double newWireRc =
                                    gnb.getWireRC();
                                if (newWireRc > maxNewWireRc)
                                    maxNewWireRc = newWireRc;
                            }

                            if (maxNewWireRc > maxOldWireRc)
                                oldSizingPaths.set(i, newSizingPath);
                        }
                    }

                    break;
                }
            }

            if(!found){
                allCatPaths.add(newCatPath);
            }
        }
        else{
            allCatPaths.add(newCatPath);
        }
    }


    private static void debugOutput(List/*<CatPath>*/ catPathsToBeCatted,
                                    Set/*<CatPath>*/ allCatPaths,
                                    CatPath cp1,
                                    long[] numCatPaths,
                                    CellType ct1)
    {
        if(DebugOption.printLevel <= 3){
	    //if(true){
            // FIXME: hardcoded constant
            if (numCatPaths[0] % 5000 == 0) {
                System.out.println();
                System.out.println("DEBUG: Concatenated paths generation in: " + ct1.typeName);
                System.out.println("Number of visited concatenated paths: " + numCatPaths[0]);
                System.out.println("Total number of distinguished paths: " + allCatPaths.size());
                System.out.println("RC of current path: " +
                                   catPathsToBeCatted.size());
                System.out.println();
                
                // FIXME: hardcoded constant
                if (numCatPaths[0] > 100000 && allCatPaths.size() > 2500) {
                    System.err.println("\033[1;31m"
                        + "ERROR 1: " + "\033[0m"
                        + "Jauto is generating more than 2500 concatenated paths in: "
                        + ct1.typeName + "\n"
                        + "Please search output file for the last \"DEBUG: Concatenated paths generation in " 
                        + ct1.typeName + "\" for detailed information and add cutpath.");
                }
            }

            if (numCatPaths[0] % 5000 < 5) {
                System.out.println(cp1.toString());
            }
        }
    }


    public boolean isComponentsFixed() {
        for (Iterator ita = getCatPath().iterator(); ita.hasNext(); ) {
            SizingPath spa = (SizingPath)ita.next();
            if (!spa.copyFrom.container.isFixedSize())
                return false;
        }
        return true;
    }

    public CatPath popUp(boolean isEndNonObservable,
                         boolean hasObservable,
                         Set/*<CellNet>*/ nonObservableStartNets,
                         CellNet endNet) {
        CatPath cpb = new CatPath(this);

        cpb.getCatPath().clear();

        for (Iterator itd = getCatPath().iterator(); itd.hasNext(); ) {
            SizingPath spa = (SizingPath)itd.next();
            SizingPath spb = new SizingPath(spa);

            cpb.getCatPath().add(spb);
        }

        cpb.setStartNets(nonObservableStartNets);
        cpb.setEndNet(endNet);
        cpb.setIsFragment(false);

        //if(isEndNonObservable || !hasObservable){ // this sizing path can be removed from the sub-cell
        //    setIsFragment(true);
        //}
        setIsFragment(true);

        //if(isEndNonObservable && hasObservable){ // this sizing path should be one of the start sizing path in cat path
        //    cpb.isReduced = true;
        //}
        if(hasObservable){ // this sizing path should be one of the start sizing path in cat path
            cpb.isReduced = true;
        }

        cpb.isEndNetNonObservable = isEndNonObservable; // is this a end sizing path for cat path?

        return cpb;
    }

    void getPathString(final StringBuffer buf) {
        for (Iterator i = path.iterator(); i.hasNext(); ) {
            final AbstractPath apath = (AbstractPath) i.next();
            apath.getPathString(buf);
            buf.append(' ');
        }
        getNetString(buf, getEndNet());
    }
}
