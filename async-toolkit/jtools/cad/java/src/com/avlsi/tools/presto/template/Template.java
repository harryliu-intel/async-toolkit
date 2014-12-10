/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.template;

import com.avlsi.tools.presto.output.AliasNodeExpression;
import com.avlsi.tools.presto.output.And2C2Expression;
import com.avlsi.tools.presto.output.ArbitraryNodeExpression;
import java.util.Arrays;
import com.avlsi.util.functions.BinaryFunction;
import java.util.BitSet;
import com.avlsi.tools.presto.output.CElementExpression;
import com.avlsi.tools.presto.ChannelName;
import com.avlsi.tools.presto.ChannelNameImpl;
import java.util.Collection;
import java.util.Collections;
import com.avlsi.util.container.CollectionUtils;
import com.avlsi.tools.presto.output.Direction;
import com.avlsi.util.container.FilteringIterator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import com.avlsi.util.container.MultiMap;
import com.avlsi.tools.presto.output.NandExpression;
import com.avlsi.tools.presto.complete.Node;
import com.avlsi.tools.presto.output.NodeExpression;
import com.avlsi.tools.presto.output.NodeName;
import com.avlsi.util.functions.NonNull;
import com.avlsi.tools.presto.output.ParameterizedNodeName;
import com.avlsi.tools.presto.output.Section;
import java.util.Set;
import java.util.TreeSet;
import com.avlsi.tools.presto.TruthTable;
import com.avlsi.tools.presto.TruthTableImpl;
import com.avlsi.tools.presto.output.UnparameterizedNodeName;
import com.avlsi.tools.presto.WarningAccumulator;
import com.avlsi.tools.presto.output.WholeOperator;
import com.avlsi.tools.presto.output.WholeOperatorSink;

public class Template {
    private static final String prefix = "do";
    
    private static Node[] condAck(ChannelName[] condGroup,
                                  ChannelName[] allInputs,
                                  BitSet[] dependencies,
                                  Node[] outputCompletions) {
        BitSet which = new BitSet();

        /* If I had designed my data structures better, I wouldn't
         * need to do these bizarre contortions. */
        outer_loop:
        for (int i = 0; i < condGroup.length; i++) {
            for (int j = 0; j < allInputs.length; j++)
                if (condGroup[i] == allInputs[j]) {
                    which.set(j);
                    continue outer_loop;
                }
            throw new RuntimeException("internal error");
        }

        LinkedList result = new LinkedList();
        for (int i = 0; i < dependencies.length; i++)
            if (which.intersects(dependencies[i]))
                result.add(outputCompletions[i]);

        /* I believe this assertion is invalid, because result will
         * be empty if no outputs depend on the conditional inputs.
         * Yes, this is a dumb thing to do in the first place, but
         * sometimes our users like to be dumb. */
        // assert (result.size() > 0);

        return (Node[]) result.toArray(new Node[0]);
    }

    /**
     * Groups together inputs by acknowledge condition.  Note that you
     * only need to give one TruthTable, because the input information is
     * the same for all the TruthTables.  (Yeah, this is a little weird.)
     * @param  tt   only the getInputs() and getInputAcknowledge() matter
     * @param  conditionalInputs  on return, there is a 1 bit for each
     *                            conditional input
     * @return MultiMap(TruthTable, ChannelName), where the TruthTable
     *         is the acknowledge truth table for all the given ChannelNames.
     *         The TruthTable is null for unconditional inputs.
     */
    private static MultiMap findConditionalGroups(TruthTable tt,
                                                  BitSet conditionalInputs) {
        final int[] ctr = new int[] { 0 };
        final MultiMap result = new MultiMap(new LinkedHashMap(),
                                             MultiMap.ARRAY_LIST_FACTORY);
        ChannelName[] inputs = tt.getInputs();
        for (int i = 0; i < inputs.length; i++) {
            boolean[] ack = tt.getInputAcknowledge(i);
            final byte[] byteAck = new byte[ack.length];
            boolean unconditional = true;
            for (int j = 0; j < ack.length; j++) {
                byteAck[j] = (byte)(ack[j] ? 1 : 0);
                unconditional &= ack[j];
            }
            ChannelName output = new ChannelName() {
                    private ChannelName orig = null;
                    
                    private void figureOutChannel() {
                        Collection c =
                            result.get(new TruthTableImpl(null, null,
                                                          byteAck, null));
                        assert (c != null && c.size() > 0);
                        if (c.size() == 1)
                            orig = (ChannelName) c.iterator().next();
                        else
                            orig = new ChannelNameImpl(ctr[0]++ + ":0");
                    }

                    public int get1of() {
                        return 2;
                    }

                    public String getName() {
                        if (orig == null)
                            figureOutChannel();
                        return prefix + orig.getName();
                    }

                    public int[] getArrayIndices() {
                        if (orig == null)
                            figureOutChannel();
                        return orig.getArrayIndices();
                    }

                    public String getNameWithIndices() {
                        if (orig == null)
                            figureOutChannel();
                        return prefix + orig.getNameWithIndices();
                    }
                };
            TruthTable newTruth = new TruthTableImpl(inputs, output, byteAck,
                                                     null);
            result.put((unconditional ? null : newTruth), inputs[i]);
            if (!unconditional)
                conditionalInputs.set(i);
        }

        return result;
    }

    /**
     * @param  dependOn    on return, a bit will be set in this BitSet for
     *                     each input that this output depends on
     * @return Set of Numbers, which are the rail numbers which are
     *         actually used.  (The "no output" rail is n)
     */
    private static Set doChannel(TruthTable tt, WholeOperatorSink wos,
                                 Declarations decl, boolean go, int chnum,
                                 boolean justEn, BitSet dependOn) {
        Set result = new TreeSet(); // needs to be in numerical order
        ChannelName output = tt.getOutput();
        int n = output.get1of();
        assert (n <= 1 + (int) Byte.MAX_VALUE);

        NodeName dest =
            new ParameterizedNodeName("_" + output.getNameWithIndices());
        Node goNode = null;
        Node _goNode = null;
        NodeName goNodeName = null;
        NodeName _goNodeName = null;
        NodeName re =
            new UnparameterizedNodeName(output.getNameWithIndices() + ".e");
        NodeName[] boilerplate =
            new NodeName[] { new UnparameterizedNodeName("en") };
        NodeName[] unconditionalBoilerplate;

        if (go) {
            goNode = new Node("go" + output.getName(),
                              output.getArrayIndices(), Node.NO_RAIL, 2, null);
            _goNode = new Node("_go" + output.getName(),
                               output.getArrayIndices(),
                               Node.NO_RAIL, 1, null);
            decl.declareNode(goNode);
            decl.declareNode(_goNode);
            goNodeName = DoCompletion.nodeToNodeName(goNode);
            _goNodeName = DoCompletion.nodeToNodeName(_goNode);
            unconditionalBoilerplate = new NodeName[] { goNodeName };
            boilerplate = unconditionalBoilerplate;
        } else if (justEn) {
            unconditionalBoilerplate = boilerplate;
        } else {
            unconditionalBoilerplate = new NodeName[] { boilerplate[0], re };
        }

        // loop over all rails
        for (int i = -1; i < n; i++) {
            NodeName[][] downs = DoLogic.doLogic(tt, i, boilerplate, dependOn);
            int rail = i < 0 ? n : i;
            if (downs.length > 0)
                result.add(new Integer(rail));
            if (downs.length > 0 || i >= 0) {
                NodeName[][] ups = new NodeName[][] { boilerplate };
                NodeExpression expr = new ArbitraryNodeExpression(ups, downs);
                Section downSection =
                    SectionFactory.mkLogicSection(chnum, rail, Direction.DOWN);
                Section upSection =
                    SectionFactory.mkLogicSection(chnum, rail, Direction.UP);
                WholeOperator wop = new WholeOperator(dest, expr, rail,
                                                      Direction.DOWN,
                                                      upSection, downSection);
                wos.hereYaGo(wop);
            }
            boilerplate = unconditionalBoilerplate;
        }

        // emit go if needed
        if (go) {
            Section goSection = SectionFactory.mkGoSection(chnum);
            boolean conditional = result.contains(new Integer(n));
            NodeName le = new UnparameterizedNodeName("le");
            NodeExpression _goExpr;
            if (conditional)
                _goExpr = new And2C2Expression(dest, re, le, false);
            else
                _goExpr = new CElementExpression(le, re, false);
            WholeOperator _goWop = new WholeOperator(_goNodeName, _goExpr, n,
                                                     Direction.DOWN,
                                                     goSection);
            wos.hereYaGo(_goWop);
            NodeExpression goExpr =
                new NandExpression(new NodeName[] { _goNodeName }, false);
            WholeOperator goWop = new WholeOperator(goNodeName, goExpr,
                                                    Direction.UP, goSection);
            wos.hereYaGo(goWop);
        }

        return result;
    }

    /**
     * Takes the given array of TruthTables (one for each output) and
     * produces a precharged half-buffer cell for it.
     * @param  tt   an array one TruthTables, one for each output
     * @param  wos  the production rules are sent here
     * @param  decl the internal declarations are sent here
     * @param  go   a Set(String), if an output channel is in this
     *              set, it gets a go signal
     * @param  cycle_node the name of the enable of an unconditional
     *                    channel gets written here
     * @param  wacc warnings are issued through this object
     * @return the ntpc spec of the generated cell
     */
    public static int doTemplate(TruthTable[] tt, WholeOperatorSink wos,
                                 Declarations decl, Set go,
                                 StringBuffer cycle_node,
                                 WarningAccumulator wacc) {
        BitSet conditionalInputs = new BitSet();
        MultiMap inputGroups = findConditionalGroups(tt[0], conditionalInputs);
        Collection unconditional = inputGroups.get(null);
        if (unconditional == null) {
            wacc.warn("warning: no unconditional inputs... this " +
                      "might not work");
            unconditional = Collections.EMPTY_SET;
        } else {
            ChannelName foo = (ChannelName) unconditional.iterator().next();
            cycle_node.append(foo.getNameWithIndices());
            cycle_node.append(".e");
        }

        LinkedList completion = new LinkedList();
        boolean nogo = false;
        boolean yesgo = false;

        // add input channels to completion
        for (Iterator it = unconditional.iterator(); it.hasNext(); )
            completion.add(DoCompletion.nodesForChannel((ChannelName)it.next(),
                                                        0, Doodad.IN));

        /* Note to self: "truths" is a list of all truth tables--
         * first the output truth tables (same as "tt" array) and then
         * the conditional input acknowledge truth tables. */
        LinkedList truths = new LinkedList();
        truths.addAll(Arrays.asList(tt));
        CollectionUtils
            .addAll(truths,
                    new FilteringIterator(inputGroups.keySet().iterator(),
                                          new NonNull()));

        // do logic and inverters
        BitSet[] dependencies = new BitSet[truths.size()];
        Node[] ciOutputCompletions = new Node[truths.size()];
        int i = 0;
        for (Iterator it = truths.iterator(); it.hasNext(); i++) {
            dependencies[i] = new BitSet();
            /* "isCondition" indicates this is a conditon for a 
             * conditional input.  (i. e. doL)  The boolean "conditional"
             * (below), indicates this is a conditional *output*. */
            boolean isCondition = (i >= tt.length);
            TruthTable theTruth = (TruthTable) it.next();
            final ChannelName output = theTruth.getOutput();
            boolean goNow = go.contains(output.getNameWithIndices());
            if (goNow)
                yesgo = true;
            else
                nogo = true;
            if (theTruth.getOutput().get1of() > 128)
                wacc.warn("Warning: channels larger than e1of128 not supported");
            Set railsUsed = doChannel(theTruth, wos, decl, goNow, i,
                                      isCondition, dependencies[i]);
            final boolean conditional =
                railsUsed.contains(new Integer(output.get1of()));
            ChannelName underscore_channel = new ChannelName() {
                    public int get1of() {
                        int n = output.get1of();
                        if (conditional)
                            n++;
                        return n;
                    }

                    public String getName() {
                        return "_" + output.getName();
                    }

                    public int[] getArrayIndices() {
                        return output.getArrayIndices();
                    }

                    public String getNameWithIndices() {
                        return "_" + output.getNameWithIndices();
                    }
                };
            /* Declare the _R channel.  And for conditional input conditions,
             * also declare the R channel, since it is not in the portlist. */
            decl.declareChannel(underscore_channel, Declarations.CH_1of);
            if (isCondition)
                decl.declareChannel(output, Declarations.CH1of);

            boolean isOutputThatDependsOnConditionalInput =
                dependencies[i].intersects(conditionalInputs);
            Node[] nfc = DoCompletion.nodesForChannel(underscore_channel,
                                                      1, Doodad.OUT,
                                                      railsUsed);
            if (isOutputThatDependsOnConditionalInput) {
                /* For an output that depends on a conditional input, create
                 * its own tiny little completion tree, so we know it
                 * isn't mixed in with some 2and2c2 or something, and so
                 * we know it ends on a positive sense.  (For 1of5s or less,
                 * this will just result in a single NAND gate.)  We remember
                 * this node for later use, and then feed it into the
                 * main completion tree. */
                Node[][] ops = new Node[][] { nfc };
                Node rv = DoCompletion.doCompletion(ops, Node.ACTIVE_HIGH,
                                                    wos, decl, false,
                                                    output.getName() + "v",
                                                    output.getArrayIndices());
                ciOutputCompletions[i] = rv;
                completion.add(new Node[] {DoCompletion.makeTerminal(rv)});
            } else {
                /* Add the rails of this channel (minus unused ones) to the
                 * nodes that need to go into the completion tree. */
                completion.add(nfc);
            }

            NodeName underscore_node = new
                ParameterizedNodeName(underscore_channel.getNameWithIndices());
            NodeName output_node = new
                ParameterizedNodeName(output.getNameWithIndices());
            NodeExpression expr =
                new NandExpression(new NodeName[] { underscore_node }, false);
            for (int j = 0; j < output.get1of(); j++) {
                Section sec = SectionFactory.mkOutputInvertersSection(i, j);
                WholeOperator wop =
                    new WholeOperator(output_node, expr, j, Direction.UP, sec);
                wos.hereYaGo(wop);
            }
        }

        // warn about inputs that nothing depends on
        ChannelName[] inputs = tt[0].getInputs();
        for (int j = 0; j < inputs.length; j++) {
            boolean used = false;
            for (int k = 0; k < dependencies.length; k++)
                used |= dependencies[k].get(j);
            if (!used) {
                wacc.warn("Warning: No outputs depend on " +
                          inputs[j].getNameWithIndices() + ".");
                wacc.warn("         You might as well just " +
                          "bitbucket it.  You should strongly");
                wacc.warn("         reconsider whether this is " +
                          "really what you want.");
            }
        }

        // warn about outputs that depend on nothing
        assert tt.length <= dependencies.length;
        for (int j = 0; j < tt.length; j++) {
            boolean depends = false;
            for (int k = 0; k < inputs.length; k++)
                depends |= dependencies[j].get(k);
            if (!depends) {
                wacc.warn("Warning: " +
                          tt[j].getOutput().getNameWithIndices() +
                          " does not depend on any inputs.");
                wacc.warn("         You might as well just " +
                          "bitgenerate it.  You should strongly");
                wacc.warn("         reconsider whether this is " +
                          "really what you want.");
            }
        }

        // do completion tree
        CompletionInfo[] cinfo =
            new CompletionInfo[inputGroups.keySet().size()];
        i = 0;
        for (Iterator it = inputGroups.keySet().iterator();
             it.hasNext(); i++) {
            TruthTable theTruth = (TruthTable)it.next();
            Collection comp;
            if (theTruth == null) {
                comp = completion;
            } else {
                comp = new LinkedList();
                Collection channels = (Collection) inputGroups.get(theTruth);
                for (Iterator it2 = channels.iterator(); it2.hasNext(); )
                    comp.add(DoCompletion.nodesForChannel((ChannelName)
                                                          it2.next(),
                                                          0, Doodad.IN));
            }
            assert (comp.size() > 0);
            cinfo[i] = new CompletionInfo((Node[][])comp
                                          .toArray(new Node[0][]),
                                          Node.ACTIVE_HIGH, (theTruth == null),
                                          (theTruth == null ? "la" : null));
        }
        DoCompletion.doCompletion(wos, decl, cinfo);
        Node ctreeRoot = null;
        LinkedHashMap inputCompletions = new LinkedHashMap();
        i = 0;
        for (Iterator it = inputGroups.keySet().iterator();
             it.hasNext(); i++) {
            TruthTable theTruth = (TruthTable)it.next();
            Node root = cinfo[i].root;
            if (theTruth == null)
                ctreeRoot = root;
            else
                inputCompletions.put(theTruth, root);
        }
        assert (ctreeRoot != null);

        // do ack for each conditional input group
        Doodad enableDoodad = new Doodad(null, Doodad.EN, null,
                                         new BitSet(), 1);
        LinkedList doneMaker = new LinkedList();
        boolean warnN = false;
        boolean hasConditionalInputs = false;
        i = 0;
        for (Iterator it =
                 new FilteringIterator(inputGroups.keySet().iterator(),
                                       new NonNull()); it.hasNext(); i++) {
            TruthTable theTruth = (TruthTable)it.next();
            Collection channels = (Collection) inputGroups.get(theTruth);

            Node[] positiveR = condAck((ChannelName[])
                                       channels.toArray(new ChannelName[0]),
                                       tt[0].getInputs(), dependencies,
                                       ciOutputCompletions);

            int positiveRtier = 0;
            for (int j = 0; j < positiveR.length; j++)
                if (positiveR[j].getTier() > positiveRtier)
                    positiveRtier = positiveR[j].getTier();

            if (positiveR.length > 4)
                warnN = true;

            ChannelName ch = theTruth.getOutput();
            String name = ch.getName();
            int[] indices = ch.getArrayIndices();
            String basename = name.substring(prefix.length());
            String __ename = "__" + basename + "e";
            String _ename  = "_" +  basename + "e";
            String ename   =        basename + "e";
            if (Character.isDigit(ename.charAt(0))) {
                assert indices.length == 0;
                ename = "\"" + ename + "\"";
            }
            Node do0node = new Node(name, indices, 0, positiveRtier,
                                    enableDoodad);
            Node do1node = new Node(name, indices, 1, positiveRtier, null);
            Node __enode = new Node(__ename, indices, Node.NO_RAIL,
                                    positiveRtier + 1, null);
            Node _enode  = new Node(_ename, indices, Node.NO_RAIL,
                                    positiveRtier + 2, enableDoodad);
            Node enode   = new Node(ename, indices, Node.NO_RAIL,
                                    positiveRtier + 3, null);
            NodeName do1 = DoCompletion.nodeToNodeName(do1node);
            NodeName __e = DoCompletion.nodeToNodeName(__enode);
            NodeName _e  = DoCompletion.nodeToNodeName(_enode);
            NodeName e   = DoCompletion.nodeToNodeName(enode);
            Node inputCompletion = (Node) inputCompletions.get(theTruth);
            NodeName lv  = DoCompletion.nodeToNodeName(inputCompletion);
            LinkedList pulldown = new LinkedList();
            pulldown.add(do1);
            pulldown.add(lv);
            int fixed = pulldown.size();
            LinkedList pulldowns = new LinkedList();

            for (int j = 0; j < positiveR.length; j++)
                pulldown.add(DoCompletion.nodeToNodeName(positiveR[j]));
            pulldowns.add(pulldown.toArray(new NodeName[0]));
            decl.declareNode(__enode);
            decl.declareNode(_enode);
            decl.declareNode(enode);
            NodeName[] pu = new NodeName[] { do1, lv };
            NodeName[][] pd = (NodeName[][])
                pulldowns.toArray(new NodeName[0][]);
            ArbitraryNodeExpression expr =
                new ArbitraryNodeExpression(new NodeName[][] { pu }, pd);
            Section downSect = SectionFactory.mkCondAckSection(i, 0);
            Section upSect = SectionFactory.mkCondAckSection(i, 1);
            Section sect = SectionFactory.mkCondAckSection(i, 2);
            wos.hereYaGo(new WholeOperator(__e, expr, Direction.DOWN,
                                           upSect, downSect));
            wos.hereYaGo(new WholeOperator(_e,
                                           new NandExpression(new NodeName[]
                                               { __e }, true), Direction.UP,
                                           sect));
            wos.hereYaGo(new WholeOperator(e,
                                           new NandExpression(new NodeName[]
                                               { _e }, false), Direction.DOWN,
                                           sect));
            Section aliasSect = SectionFactory.mkCondAckSection(i, 3);
            for (Iterator it2 = channels.iterator(); it2.hasNext(); ) {
                ChannelName theChannel = (ChannelName) it2.next();
                NodeName aliasMe =
                    new UnparameterizedNodeName(theChannel.getNameWithIndices()
                                                + ".e");
                wos.hereYaGo(new WholeOperator(aliasMe,
                                               new AliasNodeExpression(e, 0),
                                               Direction.DOWN, aliasSect));
            }
            doneMaker.add(new Node[] { _enode, do0node });
            hasConditionalInputs = true;
        }
        
        if (hasConditionalInputs && yesgo) {
            wacc.warn("Warning: conditional inputs do not work " +
                      "with go signal");
        }

        if (warnN) {
            wacc.warn("Warning: more than 6 Ns in series in the "
                      + "conditional acknowledge");
            wacc.warn("         You will need to fix this up " +
                      "manually.");
        }

        // add enable, if needed
        Node lenode = new Node("le", new int[0], Node.NO_RAIL,
                               ctreeRoot.getTier() + 1, enableDoodad);
        NodeName le = DoCompletion.nodeToNodeName(lenode);
        int halfHandshake = 2 + lenode.getTier();
        decl.declareNode(lenode);
        doneMaker.add(new Node[] { lenode });
        if (nogo) {
            Node[][] ops = (Node[][]) doneMaker.toArray(new Node[0][]);
            Node _en = DoCompletion.doCompletion(ops, Node.ACTIVE_HIGH,
                                                 wos, decl, false, "_en",
                                                 new int[0]);
            NodeName en = new UnparameterizedNodeName("en");
            decl.declareNode(new Node("en", new int[0],
                                      Node.NO_RAIL, 1, null));
            Section sect = SectionFactory.mkEnableSection(1 + _en.getTier());
            NodeExpression expr =
                new NandExpression(new NodeName[]
                    { DoCompletion.nodeToNodeName(_en) }, false);
            WholeOperator wop =
                new WholeOperator(en, expr, Direction.DOWN, sect);
            wos.hereYaGo(wop);
            halfHandshake = 1 + _en.getTier();
        }

        // do that final inverter
        NodeName la = DoCompletion.nodeToNodeName(ctreeRoot);
        Section sect = SectionFactory.mkAckSection(ctreeRoot.getTier() + 1);
        NodeExpression expr = new NandExpression(new NodeName[] { la }, false);
        WholeOperator wop = new WholeOperator(le, expr, Direction.DOWN, sect);
        wos.hereYaGo(wop);

        // alias left enables together
        Section postAck = SectionFactory.mkPostAckSection();
        NodeExpression leAlias = new AliasNodeExpression(le, 0);
        for (Iterator it = unconditional.iterator(); it.hasNext(); ) {
            ChannelName ch = (ChannelName) it.next();
            String e = ch.getNameWithIndices() + ".e";
            wos.hereYaGo(new WholeOperator(new UnparameterizedNodeName(e),
                                           leAlias, Direction.UP, postAck));
        }

        return 2 * halfHandshake;
    }
}
