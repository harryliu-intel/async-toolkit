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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Set;
import java.util.HashSet;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.ExclusiveNodeSet;
import com.avlsi.cell.ExclusiveNodeSets;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.prs.ProductionRule;
import com.avlsi.prs.ProductionRuleSet;
import com.avlsi.tools.lvs.Dnf;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.util.bool.*;
import com.avlsi.util.container.AliasedSet;
import com.avlsi.util.container.MultiSet;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.NumberFormatter;

/**
 * A prs and netlist body organized by operators.
 * Can check netlist versus prs.
 *
 * @author Andrew Lines
 * @version $Revision$ $Date$
 **/
public final class LvsOperators {
    boolean verbose = false;
    AliasedSet namespace;
    ExclusiveNodeSets exclusives;
    List problems;
    MultiSet operators;
    NetGraph netgraph;
    final String cellname;
    final double minStaticizerRatio; // minimum staticizerG/logicG
    final double maxStaticizerRatio; // maximum staticizerG/logicG
    final static double NPratio=1; // N conductance / P conductance
    // used to override minStaticizerRatio and maxStaticizerRatio on a per half
    // operator basis
    private final Map statRatioSignoffUp, statRatioSignoffDn;

    /** Inner class for a single operator. */
    public class LvsOperator implements Comparable {
	HierName name;
	final String fullname;
	HierName feedbackFrom;
	Collection paths; // raw paths from NetGraph
	ArrayList prsUp, prsDn; // lists of guards from PRS
	MultiSet netUp, netDn; // same, except from AspiceFile
	MultiSet holdUp, holdDn; // feedback terms
	static final int NO_STATICIZER=0;
	static final int WEAK_STATICIZER=1;
	static final int COMBINATIONAL_STATICIZER=2;
        static final int NO_STATICIZER_DIRECTIVE=3;
	int staticizerUp=0, staticizerDn=0;
        final boolean noStaticizerDirective;

        /**
         * Construct an LvsOperator by name (and
         * optionally flag staticizers as unnecessary).
         **/
        LvsOperator (HierName name, boolean noStaticizerDirective) {
            prsUp = new ArrayList();
            prsDn = new ArrayList();
            netUp = new MultiSet();
            netDn = new MultiSet();
            holdUp = new MultiSet();
            holdDn = new MultiSet();
            this.name = name;
            this.noStaticizerDirective = noStaticizerDirective;
            fullname = cellname + "/" + name;
            feedbackFrom = null;
        }

        /** Construct an LvsOperator by name. */
        LvsOperator (HierName name) {
            this(name,false);
        }

	/** Compare by name. */
	public int compareTo (Object B) {
	    return this.name.compareTo(((LvsOperator)B).name);
	}

	/** Debugging output. */
	public String toString () {
	    return "LvsOperator: target=" + name +
		"\n  prsUp=" + prsUp + " prsDn=" + prsDn +
		"\n  netUp=" + netUp + " netDn=" + netDn +
		"\n  holdUp=" + holdUp + " holdDn=" + holdDn;
	}

	/** Append a production rule guard to prsUp or prsDn lists. */
	void addPrs(ProductionRule prs) {
	    int dir = prs.getDirection();
	    if      (dir == ProductionRule.UP)   prsUp.add(prs.getGuard());
	    else if (dir == ProductionRule.DOWN) prsDn.add(prs.getGuard());
	}

	/** Add a NetNode's paths to op, report any sneak paths. */
	void addPaths(NetGraph.NetNode node) {
            paths = node.getPaths();
            node.reportSneakPaths();
	    Dnf.fromNetNode(node,netUp,netDn,holdUp,holdDn);
	}

	/** Find aggregate conductance of all equivalent paths. */
	double aggregateConductance(NetGraph.NetPath path) {
            double R = path.getAggregateSquares(paths);
	    return (path.type==DeviceTypes.N_TYPE ? NPratio : 1)/R;
	}

	/** Check the ratio of feedback paths versus opposing paths. */
	void checkStaticizerRatio() {
	    if (paths==null) return;

	    // for each feedback path
	    for (Iterator t = paths.iterator(); t.hasNext(); ) {
		NetGraph.NetPath pathF = (NetGraph.NetPath) t.next();
		if (!pathF.feedback) continue;
		double Gstaticizer = aggregateConductance(pathF);
		double minratio=1e9;
		double maxratio=0;
		MultiSet gatesF = pathF.getGates();

		// for each opposing logic path
		for (Iterator u = paths.iterator(); u.hasNext(); ) {
		    NetGraph.NetPath pathL = (NetGraph.NetPath) u.next();
		    if (pathL.feedback) continue;
		    if (pathF.getDir() == pathL.getDir()) continue;

		    // check if interfering guards
		    MultiSet gatesL = pathL.getGates();
		    boolean interfering = true;
		    for (Iterator v = gatesF.iterator(); v.hasNext(); )
			if (gatesL.find((HierName) v.next())!=null) interfering = false;

		    // check ratio
		    double Glogic = aggregateConductance(pathL);
		    double ratio = Glogic/Gstaticizer;
		    if (interfering&&(ratio<minratio)) minratio=ratio;
		    if (ratio>maxratio) maxratio=ratio;
		}

                List signoff = (List)
                    (pathF.getDir() > 0 ? statRatioSignoffDn
                                        : statRatioSignoffUp).get(name);
                if (signoff != null && signoff.size() != 2) {
                    problems.add(new LVSProblem("invalid staticizer_ratio_signoff directive specified for " + fullname + (pathF.getDir() > 0 ? "-" : "+")));
                    signoff = null;
                }

		// report errors
		if (minratio < minStaticizerRatio) {
                    final Float f =
                        signoff == null ? null : (Float) signoff.get(0);
                    final int type = (f != null && minratio >= f.floatValue())
                        ? LVSProblem.LVS_WARNING : LVSProblem.LVS_ERROR;
		    problems.add(new LVSProblem("small logic to staticizer ratio " +
						NumberFormatter.format(minratio,1) +
                                                (f == null ? "" : " (signoff " + f + ")") +
						" for " + fullname +
						"" + (pathF.getDir()>0 ? "-" : "+") +
						"\n  staticizer=" + pathF,
                                                type));
                }
		if (maxratio > maxStaticizerRatio) {
                    final Float f =
                        signoff == null ? null : (Float) signoff.get(1);
                    final int type = (f != null && maxratio <= f.floatValue())
                        ? LVSProblem.LVS_WARNING : LVSProblem.LVS_ERROR;
		    problems.add(new LVSProblem("large logic to staticizer ratio " +
						NumberFormatter.format(maxratio,1) +
                                                (f == null ? "" : " (signoff " + f + ")") +
						" for " + fullname +
						"" + (pathF.getDir()>0 ? "-" : "+") +
						"\n  staticizer=" + pathF,
                                                type));
                }
	    }
	}

	/** Check that prs, net, and staticizer match for this LvsOperator. */
	void check() {
	    MultiSet temp,dnfUp,dnfDn,dualUp,dualDn;
	    boolean ok = true;

	    // check ratio of weak feedback
	    checkStaticizerRatio();

	    // create DNF version of PRS pullup
	    OrBooleanExpression orUp = new OrBooleanExpression(true,prsUp);
	    dnfUp = Dnf.fromExpression(orUp,ProductionRule.UP,namespace,exclusives);
	    if (dnfUp == null) {
		problems.add(new LVSProblem("non-inverse-monotonic rule: " +
					    fullname + "+\n  orUp=" + orUp));
		ok = false;
	    }

	    // create DNF version of PRS pulldn
	    OrBooleanExpression orDn = new OrBooleanExpression(true,prsDn);
	    dnfDn = Dnf.fromExpression(orDn,ProductionRule.DOWN,namespace,exclusives);
	    if (dnfDn == null) {
		problems.add(new LVSProblem("non-inverse-monotonic rule: " +
					    fullname + "-\n  orDn=" + orDn));
		ok = false;
	    }

	    // bail because of bad production rules
	    if (!ok) return;

	    // check netUp
	    if (dnfUp.compareTo(netUp)!=0) {
		problems.add(new LVSProblem("mismatch: " + fullname + "+" +
					    "\n  prsUp=" + dnfUp + "\n  netUp=" + netUp));
		ok = false;
	    }

	    // check netDn
	    if (dnfDn.compareTo(netDn)!=0) {
		problems.add(new LVSProblem("mismatch: " + fullname + "-" +
					    "\n  prsDn=" + dnfDn + "\n  netDn=" + netDn));
		ok = false;
	    }

	    // bail because of a logic mismatch
	    if (!ok) return;

	    // get dualUp, but don't get dualDn yet because it might be too damn big
	    dualUp = Dnf.fromExpression(orUp.negated(),ProductionRule.DOWN,
					namespace,exclusives);

	    // check for combinational logic
	    if (dnfDn.compareTo(dualUp)==0) {
		staticizerUp = staticizerDn = COMBINATIONAL_STATICIZER;
		if ((holdUp.size()>0)||(holdDn.size()>0)) {
		    problems.add(new LVSProblem("staticized combinational logic on: " +
						fullname));
		}
		return;
	    }

            // check for staticizer on a nostaticizer node
            if (noStaticizerDirective) {
                staticizerUp = staticizerDn = NO_STATICIZER_DIRECTIVE;
		if ((holdUp.size()>0)||(holdDn.size()>0)) {
		    problems.add(new LVSProblem("staticized \"nostaticizer\" node: " +
						fullname));
		}
		return;
            }

	    // check for feedback inverter
	    if (feedbackFrom == null) {
		problems.add(new LVSProblem("missing feedback inverter for staticizer: " +
					    fullname));
		return;
	    }

	    // check staticizer pullup
	    temp = Dnf.createSingleGateDnf(feedbackFrom);
	    if (temp.compareTo(holdUp)==0) staticizerUp = WEAK_STATICIZER;
	    else {
		// probably OK to compute dualDn if its used for a combinational staticizer
                dualDn = Dnf.fromExpression(orDn.negated(),ProductionRule.UP,
                                            namespace,exclusives);

                // expected (holdUp | dnfUp)
                temp = Dnf.addConjunctGate(dualDn,feedbackFrom);
                temp.addAll(dnfUp);

                // actual (holdUp | dnfUp)
                MultiSet temp2 = new MultiSet();
                temp2.addAll(holdUp);
                temp2.addAll(dnfUp);

                // compare
                temp  = Dnf.getCanonicalForm(temp, ProductionRule.UP,exclusives);
                temp2 = Dnf.getCanonicalForm(temp2,ProductionRule.UP,exclusives);
		if (temp.compareTo(temp2)==0) staticizerUp = COMBINATIONAL_STATICIZER;
	    }

	    // check staticizer pulldn
	    temp = Dnf.createSingleGateDnf(feedbackFrom);
	    if (temp.compareTo(holdDn)==0) staticizerDn = WEAK_STATICIZER;
	    else {
                // expected (holdDn | dnfDn)
		temp = Dnf.addConjunctGate(dualUp,feedbackFrom);
                temp.addAll(dnfDn);

                // actual (holdDn | dnfDn)
                MultiSet temp2 = new MultiSet();
                temp2.addAll(holdDn);
                temp2.addAll(dnfDn);

                // compare
                temp  = Dnf.getCanonicalForm(temp, ProductionRule.DOWN,exclusives);
                temp2 = Dnf.getCanonicalForm(temp2,ProductionRule.DOWN,exclusives);
                if (temp.compareTo(temp2)==0) staticizerDn = COMBINATIONAL_STATICIZER;
	    }

	    // report staticizer errors
	    if (staticizerUp == NO_STATICIZER)
		problems.add(new LVSProblem((holdUp.size()>0 ? "bad" : "missing") +
					    " staticizer: " + fullname +
					    "+\n  prsDn= " + dnfDn +
					    "\n  holdUp= " + holdUp));
	    if (staticizerDn == NO_STATICIZER)
		problems.add(new LVSProblem((holdDn.size()>0 ? "bad" : "missing") +
					    " staticizer: " + fullname +
					    "-\n  prsUp= " + dnfUp +
					    "\n  holdDn= " + holdDn));
	}
    }

    /** Construct a representation of all operators in both prs and netlist. */
    public LvsOperators(final CellInterface prsCell,
			final AspiceFile aspiceCell,
			final AliasedSet commonNamespace,
			final ExclusiveNodeSets exclusiveSets,
			final List problemList,
			final double minStaticizerRatio,
			final double maxStaticizerRatio,
			final HierName VddName,
			final HierName GndName) {

        // Canonize nostaticizer so as to make check later easier.
        final Map nostatsmap =
            DirectiveUtils.getPrsDirective(prsCell,
                                           DirectiveConstants.NO_STAT,
                                           DirectiveConstants.NODE_TYPE);
        final Set nostats = DirectiveUtils.canonize(commonNamespace, DirectiveUtils.getExplicitTrues(nostatsmap));

        final Map staticizerRatioSignoff =
            DirectiveUtils.getPrsDirective(
                    prsCell,
                    DirectiveConstants.STATICIZER_RATIO_SIGNOFF,
                    DirectiveConstants.HALFOP_TYPE);
        statRatioSignoffUp = DirectiveUtils.canonizeKey(
                    commonNamespace,
                    DirectiveUtils.getUps(staticizerRatioSignoff));
        statRatioSignoffDn = DirectiveUtils.canonizeKey(
                    commonNamespace,
                    DirectiveUtils.getDowns(staticizerRatioSignoff));

        // Get operators which are expected to fail LVS
        final Map mismatchOkMap =
            DirectiveUtils.getPrsDirective(prsCell,
                    DirectiveConstants.PRS_NETLIST_MISMATCH_OK,
                    DirectiveConstants.NODE_TYPE);
        final Set mismatchOk = DirectiveUtils.canonize(commonNamespace, DirectiveUtils.getExplicitTrues(mismatchOkMap));
        final Set mismatchWarned = new HashSet();

	// look through PRS
	namespace = commonNamespace;
	exclusives = exclusiveSets;
	problems = problemList;
	operators = new MultiSet();
	cellname = aspiceCell.getName();
	this.minStaticizerRatio = minStaticizerRatio;
	this.maxStaticizerRatio = maxStaticizerRatio;
	ProductionRuleSet allprs = prsCell.getProductionRuleSet();
	for (Iterator t = allprs.getProductionRules(); t.hasNext(); ) {
	    ProductionRule prs = (ProductionRule) t.next();
            HierName target = (HierName) namespace.getCanonicalKey(prs.getTarget());
            if (mismatchOk.contains(target)) {
                mismatchWarned.add(target);
                continue;
            }
            boolean noStaticizer=nostats.contains(target);
	    LvsOperator op = new LvsOperator(target, noStaticizer);
	    LvsOperator x = (LvsOperator) operators.find(op);
	    if (x!=null) op = x;
	    else operators.add(op);
	    op.addPrs(prs);
	}

	// look through EXT or CDL
	netgraph = new NetGraph(commonNamespace,exclusives,problems,
            VddName,GndName, Collections.EMPTY_SET);
	netgraph.addAspice(aspiceCell);
	netgraph.prepareForLvs();
	if (verbose) System.err.println(netgraph);
        if (verbose) System.err.println(aspiceCell.getNamespaceString());
        if (verbose) System.err.println(namespace);
	for (Iterator t = netgraph.nodes.iterator(); t.hasNext(); ) {
	    NetGraph.NetNode node = (NetGraph.NetNode) t.next();
	    if (!node.output) continue;
	    if (node.isStaticizerInverter()) continue; // don't check these
            if (mismatchOk.contains(node.getName())) {
                mismatchWarned.add(node.getName());
                continue;
            }
	    LvsOperator x,op;
	    op = new LvsOperator(node.name);
	    x = (LvsOperator) operators.find(op);
	    if (x!=null) op = x;
	    else operators.add(op);
	    op.addPaths(node);
	    if (node.feedbackFrom != null) op.feedbackFrom = node.feedbackFrom.name;
	}

        for (Iterator i = mismatchWarned.iterator(); i.hasNext(); ) {
            problems.add(new LVSProblem("LVS bypassed for operator " + cellname + "/" + i.next() + " because of prs_netlist_mismatch_ok", LVSProblem.LVS_WARNING));
        }

	// debugging
	if (verbose) System.err.println(this);
    }

    /** Check all operators. */
    public void check() {
	for (Iterator t = operators.iterator(); t.hasNext(); ) {
	    ((LvsOperator) t.next()).check();
	}
    }

    /** Debugging output. */
    public String toString () {
	String s="";
	s += "Exclusives\n";
	for (Iterator t = exclusives.getIterator(); t.hasNext(); ) {
	    s += " " + t.next() + "\n";
	}
	s += "Operators\n";
	for (Iterator t = operators.iterator(); t.hasNext(); ) {
	    s += " " + t.next() + "\n";
	}
	return s;
    }

    private String hierslash(HierName h) {
        return (h == null) ? null : h.getAsString('/');
    }
}
