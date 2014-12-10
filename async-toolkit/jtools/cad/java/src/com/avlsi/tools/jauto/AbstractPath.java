/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.io.BufferedWriter;
import java.util.Iterator;
import java.util.Set;

import com.avlsi.fast.CellNet;
import com.avlsi.fast.CellType;
import com.avlsi.util.text.NumberFormatter;

/**
 * Abstract base class for <code>SizingPath</code> and <code>CatPath</code>.
 *
 * @author Jesse Rosenstock
 * @version $Revision$ $Date$
 **/
abstract class AbstractPath {
    Set/*<CellNet>*/        startNets;

    CellNet                 endNet;

    /**
     * The highest level cell containing this cat path.
     **/
    CellType                container;

    boolean                 isFragment;

    boolean                 isReduced;

    boolean                 isEndNetNonObservable;

    /** Calculated delay from timing analysis */
    double[]                delay;

    /** Calculated slack from timing analysis */
    double[]                slack;

    /** Instance delaybias applied to this path */
    double[]                delaybias;

    /** The acceptable delay budgets for this path */
    double[]                signoff;

    /** Cell net of last half-operator with least slack */
    CellNet                 criticalNet;

    abstract void setStartEndNets(boolean completeCatPath);

    abstract String printPath();

    abstract void dumpInfo(BufferedWriter bw1, String prefix);

    abstract double getAverageWidth();

    public abstract boolean isComponentsFixed();

    public boolean isFixedSize() {
        if (!isComponentsFixed()) return false;

        for (Iterator ita = criticalNet.getGlobalNets().iterator();
             ita.hasNext(); ) {
            GlobalNet gna = (GlobalNet)ita.next();
            for (Iterator itb = gna.getListSinks().iterator();
                 itb.hasNext(); ) {
                final NetSink nska = (NetSink) itb.next();
                if (nska.getType() == NetType.HALF_OPERATOR_TRANSISTOR) {
                    if (!nska.sink.subType.isFixedSize()) return false;
                }
            }
        }

        return true;
    }

    public boolean isFixedSize(int n) {
        if (!isComponentsFixed()) return false;

        final GlobalNet gna = (GlobalNet) getEndNet().getGlobalNets().get(n);
        for (Iterator itb = gna.getListSinks().iterator(); itb.hasNext(); ) {
            final NetSink nska = (NetSink) itb.next();
            if (nska.getType() == NetType.HALF_OPERATOR_TRANSISTOR) {
                if (!nska.sink.subType.isFixedSize()) return false;
            }
        }

        return true;
    }

    public boolean isFragment()
    {   
        return isFragment;
    }


    public boolean setIsFragment(boolean b1)
    {   
        isFragment = b1;

        return isFragment;
    }



    public Set/*<CellNet>*/ getStartNets()
    {
        return startNets;
    }


    public Set setStartNets(Set/*<CellNet>*/ set1)
    {
        startNets.clear();
        startNets.addAll(set1);

        return startNets;
    }


    public CellNet getEndNet()
    {
        return endNet;
    }


    public CellNet setEndNet(CellNet cn1)
    {
        endNet = cn1;

        return endNet;
    }

    abstract CatPath popUp(boolean isEndNonObservable,
                           boolean hasObservable,
                           Set/*<CellNet>*/ nonObservableStartNets,
                           CellNet endNet);

    protected static String printDelay(final double x) {
        return NumberFormatter.format(x * 1e12, 3);
    }

    protected static String printDelay(final double[] x) {
        final StringBuffer result = new StringBuffer();
        result.append('[');
        for (int i = 0; i < x.length; ++i) {
            result.append(printDelay(x[i]));
            if (i < x.length - 1) result.append(' ');
        }
        result.append(']');
        return result.toString();
    }

    public abstract String printPath(int n);

    abstract void getPathString(StringBuffer buf);

    public double getBudget(int i) {
        return signoff == null || Double.isNaN(signoff[i]) ?
               (delay[i] + slack[i]) : signoff[i];
    }

    public double getDelay(int i) {
        return delay[i];
    }

    public double getSlack(int i) {
        return getBudget(i) - getDelay(i);
    }

    public double getRealSlack(int i) {
        return slack[i];
    }

    protected void getNetString(final StringBuffer buf, final CellNet net) {
        buf.append(net.container.typeName);
        buf.append('/');
        buf.append(net.canonicalName.getCadenceString());
    }
}
