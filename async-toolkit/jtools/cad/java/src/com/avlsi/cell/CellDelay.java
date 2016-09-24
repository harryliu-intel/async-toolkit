/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.cell;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.util.container.AliasedSet;

/**
 * Calculate the delay associated with a half-operator.
 *
 * @author Harry Liu
 * @version $DateTime$ $Revision$
 **/
public final class CellDelay {
    private final CellInterface cell;
    private final AliasedSet localNodes;
    private final float nativeUpDelay, nativeDnDelay;
    private final Map upAfterDelays;
    private final Map dnAfterDelays;
    private final Map<HierName,Boolean> isAbsDelay;
    private final Map upDelayBias;
    private final Map dnDelayBias;
    private final Map upExtraDelay;
    private final Map dnExtraDelay;
    private final Map upAstaExtraDelay;
    private final Map dnAstaExtraDelay;
    private final float cellDelayBias;
    
    public CellDelay(final CellInterface cell, final Cadencize cadencizer) {
        this(cell, cadencizer, Float.NaN);
    }

    public CellDelay(final CellInterface cell, final Cadencize cadencizer,
                     final float cellDelayBias) {
        this(cell, cadencizer.convert(cell).getLocalNodes(),
             cell.getProductionRuleSet().getProductionRules(), cellDelayBias,
             false);
    }

    public CellDelay(final CellInterface cell, final AliasedSet localNodes,
                     final Iterator prsIterator) {
        this(cell, localNodes, prsIterator, Float.NaN, false);
    }

    public CellDelay(final CellInterface cell, final AliasedSet localNodes,
                     final Iterator prsIterator, final float cellDelayBias,
                     final boolean useAstaExtraDelay) {
        this.cell = cell;
        this.localNodes = localNodes;
        this.cellDelayBias = cellDelayBias;

        this.nativeUpDelay = ((Float) DirectiveUtils.getTopLevelDirective(cell,
          DirectiveConstants.DEFAULT_UP_DELAY)).floatValue();
        this.nativeDnDelay = ((Float) DirectiveUtils.getTopLevelDirective(cell,
          DirectiveConstants.DEFAULT_DN_DELAY)).floatValue();

        this.upAfterDelays = new HashMap();
        this.dnAfterDelays = new HashMap();
        this.isAbsDelay = new HashMap<>();

        final Set badUpSet = new HashSet();
        final Set badDnSet = new HashSet();

        final Set badAbsDelay = new HashSet();

        while (prsIterator.hasNext()) {
            final ProductionRule pr = (ProductionRule) prsIterator.next();
            final HierName target = canon(pr.getTarget());
            final int direction = pr.getDirection();
            assert direction == ProductionRule.UP ||
                   direction == ProductionRule.DOWN;
            final Map delayMap =
                direction == ProductionRule.UP ? upAfterDelays : dnAfterDelays;

            isAbsDelay.merge(target, pr.isAbsolute(),
                             (oldV, newV) -> {
                                if (oldV != newV) badAbsDelay.add(target);
                                return oldV || newV;
                             });

            final int after = pr.getAfter();
            assert after >= 0 || after == -1 : "Invalid after delay: " + pr;

            final Integer oldAfter = (Integer) delayMap.get(target);
            final Set badSet =
                direction == ProductionRule.UP ? badUpSet : badDnSet;

            Integer newAfter = new Integer(after);
            if (oldAfter != null && oldAfter.intValue() != after) {
                badSet.add(target);
                if (after == -1) newAfter = null;
                else if (oldAfter.intValue() != -1 &&
                         after >= oldAfter.intValue()) newAfter = null;
            }

            if (newAfter != null) delayMap.put(target, newAfter);
        }

        for (Iterator i = badUpSet.iterator(); i.hasNext(); ) {
            final HierName target = (HierName) i.next();
            System.err.println("Warning: half operator " + target + "+ in cell " + cell.getFullyQualifiedType() + " has inconsistent after delay, using a delay of " + upAfterDelays.get(target));
        }

        for (Iterator i = badDnSet.iterator(); i.hasNext(); ) {
            final HierName target = (HierName) i.next();
            System.err.println("Warning: half operator " + target + "- in cell " + cell.getFullyQualifiedType() + " has inconsistent after delay, using a delay of " + dnAfterDelays.get(target));
        }

        for (Iterator i = badAbsDelay.iterator(); i.hasNext(); ) {
            final HierName target = (HierName) i.next();
            System.err.println("Warning: half operator " + target + " in cell " + cell.getFullyQualifiedType() + " has both after and after_ps delay");
        }

        final Map delayBias = DirectiveUtils.getPrsDirective(cell, DirectiveConstants.DELAYBIAS, DirectiveConstants.HALFOP_TYPE);
        this.upDelayBias = DirectiveUtils.canonizeKey(localNodes, DirectiveUtils.getUps(delayBias));
        this.dnDelayBias = DirectiveUtils.canonizeKey(localNodes, DirectiveUtils.getDowns(delayBias));

        final Map extraDelay = DirectiveUtils.getPrsDirective(cell, DirectiveConstants.EXTRA_DELAY, DirectiveConstants.HALFOP_TYPE);
        this.upExtraDelay = DirectiveUtils.canonizeKey(localNodes, DirectiveUtils.getUps(extraDelay));
        this.dnExtraDelay = DirectiveUtils.canonizeKey(localNodes, DirectiveUtils.getDowns(extraDelay));

        if (useAstaExtraDelay) {
            final Map astaExtraDelay = DirectiveUtils.getPrsDirective(cell, DirectiveConstants.ASTA_EXTRA_DELAY, DirectiveConstants.HALFOP_TYPE);
            this.upAstaExtraDelay = DirectiveUtils.canonizeKey(localNodes, DirectiveUtils.getUps(astaExtraDelay));
            this.dnAstaExtraDelay = DirectiveUtils.canonizeKey(localNodes, DirectiveUtils.getDowns(astaExtraDelay));
        } else {
            this.upAstaExtraDelay = Collections.EMPTY_MAP;
            this.dnAstaExtraDelay = Collections.EMPTY_MAP;
        }
    }

    private final HierName canon(final HierName name) {
        return (HierName) localNodes.getCanonicalKey(name);
    }

    /**
     * Return the native delay associated with N or P transistors.
     **/
    public float getNativeDelay(final boolean up) {
        return up ? nativeUpDelay : nativeDnDelay;
    }

    private final float getDelay(final HierName node, final boolean up,
                                 final Map upMap, final Map dnMap,
                                 final float defaultDelay) {
        final Map delayMap = up ? upMap : dnMap;
        final Number delay = (Number) delayMap.get(canon(node));
        return delay == null ? defaultDelay : delay.floatValue();
    }

    /**
     * Return the delay specified by the <code>after</code> keyword in the PRS
     * block.
     **/
    public float getAfterDelay(final HierName node, final boolean up) {
        float delay = getDelay(node, up, upAfterDelays, dnAfterDelays, 100);
        if (delay<0) delay = getNativeDelay(up); // use native delay instead
        return delay;
    }

    /**
     * Return the delay specified by the <code>delaybias</code> directive in
     * the PRS block.
     **/
    public float getDelayBias(final HierName node, final boolean up) {
        return getDelay(node, up, upDelayBias, dnDelayBias, 1);
    }

    /**
     * Return the delay specified by the <code>extra_delay</code> directive
     * (including the <code>asta_extra_delay</code> directive, if requested).
     **/
    public float getExtraDelay(final HierName node, final boolean up) {
        return getDelay(node, up, upExtraDelay, dnExtraDelay, 0) +
               getDelay(node, up, upAstaExtraDelay, dnAstaExtraDelay, 0);
    }

    /**
     * Return the delay specified by the <code>delaybias</code> directive in
     * the top level block, or the instance hierarchy propagated
     * <code>delaybias</code> directive, if that is available.
     **/
    public float getCellDelayBias() {
        if (Float.isNaN(cellDelayBias)) {
            return ((Float) DirectiveUtils.getTopLevelDirective(cell, DirectiveConstants.DELAYBIAS)).floatValue();
        } else {
            return cellDelayBias;
        }
    }

    /**
     * Return the delay associated with a node.  This is computed as:
     * <pre>delay = [ [afterDelay or nativeDelay] + extraDelay ] * delayBias * cellDelayBias / 100.</pre>
     **/
    public float getDelay(final HierName node, final boolean up) {
        return (getAfterDelay(node, up) + getExtraDelay(node, up))
            * getDelayBias(node, up) * getCellDelayBias() / 100;
    }

    /**
     * Return the delay associated with a node given the unit delay.  This is
     * computed as multiplying the result of
     * {@link getDelay(HierName, boolean) getDelay}
     * by <code>tau</code>.
     **/
    public float getDelay(final HierName node, final boolean up,
                          final float tau) {
        return getDelay(node, up) * tau;
    }

    /**
     * Return whether the node has an absolute delay.
     **/
    public boolean isAbsolute(final HierName node, final boolean up) {
        return isAbsDelay.getOrDefault(node, false);
    }

    public String toString() {
        final StringBuffer buf = new StringBuffer();
        buf.append("CellDelay for " + cell.getFullyQualifiedType() + "\n");
        buf.append("nativeUpDelay: " + nativeUpDelay);
        buf.append(" nativeDnDelay: " + nativeDnDelay + "\n");
        buf.append("upAfterDelays: " + upAfterDelays + "\n");
        buf.append("dnAfterDelays: " + dnAfterDelays + "\n");
        buf.append("upDelayBias: " + upDelayBias + "\n");
        buf.append("dnDelayBias: " + dnDelayBias + "\n");
        buf.append("upExtraDelay: " + upExtraDelay + "\n");
        buf.append("dnExtraDelay: " + dnExtraDelay + "\n");
        return buf.toString();
    }
}
