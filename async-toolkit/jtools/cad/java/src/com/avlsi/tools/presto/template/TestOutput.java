/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.tools.presto.template;

import com.avlsi.tools.presto.output.ArbitraryNodeExpression;
import com.avlsi.tools.presto.output.CElementExpression;
import com.avlsi.tools.presto.ChannelNameImpl;
import com.avlsi.tools.presto.output.Direction;
import com.avlsi.tools.presto.output.NandExpression;
import com.avlsi.tools.presto.complete.Node;
import com.avlsi.tools.presto.output.NodeName;
import com.avlsi.tools.presto.output.NorExpression;
import com.avlsi.tools.presto.output.Organizer;
import java.io.OutputStreamWriter;
import com.avlsi.tools.presto.output.ParameterizedNodeName;
import java.io.PrintWriter;
import com.avlsi.tools.presto.output.PrsPrinter;
import com.avlsi.tools.presto.output.UnparameterizedNodeName;
import com.avlsi.tools.presto.output.WholeOperator;

public class TestOutput {
    private static Node mkNode(String name) {
        return new Node(name, new int[0], Node.NO_RAIL, 0, null);
    }

    public static void main(String[] argv) {
        Organizer org = new Organizer();
        
        org.hereYaGo(new WholeOperator(new UnparameterizedNodeName("_lv"),
                                       new NorExpression(new NodeName[] {
                                           new UnparameterizedNodeName("L.0"),
                                           new UnparameterizedNodeName("L.1")
                                       }, false), Direction.DOWN,
                                       SectionFactory.mkInputSection(0)));

        org.hereYaGo(new WholeOperator(new UnparameterizedNodeName("lv"),
                                       new NandExpression(new NodeName[] {
                                           new UnparameterizedNodeName("_lv")
                                       }, false), Direction.UP,
                                       SectionFactory.mkInputSection(1)));

        for (int i = 0; i < 2; i++) {
            org.hereYaGo(new WholeOperator(new ParameterizedNodeName("R"),
                                           new NandExpression(new NodeName[] {
                                               new ParameterizedNodeName("_r")
                                           }, false), i, Direction.UP,
                                           SectionFactory.mkOutputInvertersSection(0,i)));
            NodeName[] downs = new NodeName[] { new UnparameterizedNodeName("L.e"),
                                                new UnparameterizedNodeName("R.e"),
                                                new ParameterizedNodeName("L") };
            NodeName[] ups = new NodeName[] { new UnparameterizedNodeName("L.e"),
                                              new UnparameterizedNodeName("R.e") };
            org.hereYaGo(new WholeOperator(new ParameterizedNodeName("_r"),
                                           new ArbitraryNodeExpression(new NodeName[][] {ups},
                                                                       new NodeName[][] {downs}),
                                           i, Direction.DOWN,
                                           SectionFactory.mkLogicSection(0,i,Direction.UP),
                                           SectionFactory.mkLogicSection(0,i,Direction.DOWN)));
        }

        org.hereYaGo(new WholeOperator(new UnparameterizedNodeName("rv"),
                                       new NandExpression(new NodeName[] {
                                           new UnparameterizedNodeName("_r.0"),
                                           new UnparameterizedNodeName("_r.1")
                                       }, false), Direction.UP,
                                       SectionFactory.mkOutputTreeSection(1)));

        org.hereYaGo(new WholeOperator(new UnparameterizedNodeName("__le"),
                                       new CElementExpression(new UnparameterizedNodeName("lv"),
                                                              new UnparameterizedNodeName("rv"),
                                                              false), Direction.DOWN,
                                       SectionFactory.mkAckSection(2)));

        org.hereYaGo(new WholeOperator(new UnparameterizedNodeName("_le"),
                                       new NandExpression(new NodeName[] {
                                           new UnparameterizedNodeName("__le")
                                       }, true), Direction.UP,
                                       SectionFactory.mkAckSection(3)));

        org.hereYaGo(new WholeOperator(new UnparameterizedNodeName("L.e"),
                                       new NorExpression(new NodeName[] {
                                           new UnparameterizedNodeName("_le")
                                       }, false), Direction.DOWN,
                                       SectionFactory.mkAckSection(4)));

        PrsPrinter p = new PrsPrinter();
        org.simplify(p);
        PrintWriter w = new PrintWriter(new OutputStreamWriter(System.out));
        DeclarationsImpl decl = new DeclarationsImpl();
        decl.declareNode(mkNode("_lv"));
        decl.declareNode(mkNode("lv"));
        decl.declareNode(mkNode("rv"));
        decl.declareNode(mkNode("__le"));
        decl.declareNode(mkNode("_le"));
        decl.declareChannel(new ChannelNameImpl("_r:2"), Declarations.CH_1of);
        w.println("define PCHBUF_1of2 ()(e1of2 -L,+R) <: PCHB(2) {");
        w.println("  prs {");
        w.println("    // 14 transitions/cycle");
        // w.println("    node _lv,lv,rv,__le,_le;");
        // w.println("    _1of2 _r;");
        decl.emit(w, 4, 78);
        p.emit(w, 4, 4, 40, SectionFactory.mkAncestorSection());
        w.println("  }");
        w.println("}");
        w.flush();
    }
}
