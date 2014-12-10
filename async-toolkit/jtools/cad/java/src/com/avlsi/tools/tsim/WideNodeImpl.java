package com.avlsi.tools.tsim;

import java.math.BigInteger;
import java.util.LinkedHashMap;
import java.util.Map;

import com.avlsi.fast.ports.NodeType;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DSimUtil;
import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.NodeWatcher;
import com.avlsi.util.text.StringUtil;

/**
 * A collection of nodes to represents a "wide" node.
 **/
public class WideNodeImpl implements WideNode {
    private final Node[] nodes;
    private final BigInteger[] values;
    private final boolean disableWrite;
    private final boolean isArrayed;
    public WideNodeImpl(final String name, final int size, final int direction,
                        final boolean isArrayed, final boolean cosim) {
        nodes = new Node[size];
        values = new BigInteger[size];
        for (int i = 0; i < size; ++i) {
            final String nodeName;
            if (isArrayed) {
                nodeName = StringUtil.replaceSubstring(name + "[" + i + "]",
                                                       "][", ",");
            } else {
                nodeName = name;
            }
            nodes[i] = DSim.get().findOrAddNode(nodeName);
            if (nodes[i] == null) {
                throw new NoSuchNodeException(nodeName);
            }
            values[i] = i == 0 ? BigInteger.ONE : values[i - 1].shiftLeft(1);
        }
        disableWrite = cosim && (direction == PortDefinition.OUT ||
                                 direction == PortDefinition.INOUT);
        this.isArrayed = isArrayed;
    }
    private void printStatus() {
        for (int i = 0; i < nodes.length; ++i) {
            System.err.println("node = " + nodes[i] +
                               "(" + Node.getNameForValue(nodes[i].getValue()) +
                               ") value = " + values[i]);
        }
    }
    public WideNodeImpl(final Node[] nodes, final BigInteger[] values,
                        final boolean isArrayed) {
        this.nodes = nodes;
        this.values = values;
        this.isArrayed = isArrayed;
        disableWrite = false;
        resetNode();
    }
    private void resetNode() {
        final Node resetNode = DSimUtil.getResetNode();
        if (resetNode != null) resetNode.addWatch(new ResetNodeWatcher());
    }
    public BigInteger getValue() {
        BigInteger result = BigInteger.ZERO;
        for (int i = 0; i < nodes.length; ++i) {
            switch (nodes[i].getValue()) {
              case Node.VALUE_1: result = result.add(values[i]);
                                 break;
              case Node.VALUE_U: return null;
            }
        }
        return result;
    }
    public void setValue(final int[] indices, final byte[] values) {
        if (!disableWrite)
            for (int i = 0; i < indices.length; ++i) {
                nodes[indices[i]].scheduleImmediate(values[i]);
            }
    }
    public void setValue(final int begin, final int end, final BigInteger val) {
        final int[] indices = new int[end - begin + 1];
        final byte[] values = new byte[end - begin + 1];
        for (int i = begin; i <= end; ++i) {
            indices[i] = i;
            values[i] = val.testBit(i - begin) ? Node.VALUE_1 : Node.VALUE_0;
        }
        setValue(indices, values);
    }
    public void setValue(final BigInteger val) {
        setValue(0, nodes.length - 1, val);
    }
    public Node[] getNodes() {
        return nodes;
    }
    public boolean isArrayed() {
        return isArrayed;
    }
    private final class ResetNodeWatcher implements NodeWatcher {
        public void nodeChanged(Node node, long time) {
            if (node.getValue() == Node.VALUE_0) {
                for (int i = 0; i < nodes.length; ++i) {
                    nodes[i].setValueAndEnqueueDependents(Node.VALUE_0);
                }
            }
        }
    }
    public boolean stable() {
        for (int i = 0; i < nodes.length; ++i) {
            if (nodes[i].getValue() == Node.VALUE_U) return false;
        }
        return true;
    }
}
