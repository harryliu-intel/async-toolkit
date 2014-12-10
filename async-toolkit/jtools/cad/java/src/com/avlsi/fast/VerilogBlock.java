/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.avlsi.cell.CellImpl;
import com.avlsi.fast.BlockInterface;
import com.avlsi.util.container.FlatteningIterator;
import com.avlsi.util.debug.Debug;

/**
 * A class to represent named verilog blocks in a cell.  For example:
 * <pre>
 * define TEST()(node +a, -b) {
 *   verilog {
 *     rtl {
 *       TEST_RTL (a, b) : 'test_rtl.v';
 *     }
 *     gate {
 *       TEST_GATE (.a(a), .b(b)) : 'test_gate.v';
 *     }
 *   }
 * }
 * </pre>
 * Each <code>VerilogBlock</code> contains any number of
 * <code>NamedBlock</code>s, each of which contains any number of
 * <code>Instance</code>s.  See the CASTv2 spec for more information.
 **/
public class VerilogBlock extends BlockCommon {
    /**
     * Represent an instantiation of a verilog module.  There are 2 ways to
     * make the connections between CAST and Verilog: by implicit ordering of
     * the ports (the TEST_RTL example) or by explicit naming of ports (the
     * TEST_GATE example).
     **/
    public static class Instance {
        private final String module;
        private final List parameters;
        private final List files;
        private final List impliedPorts;
        private final Map explicitPorts;
        public Instance(final String module, final List parameters,
                        final List impliedPorts, final Map explicitPorts,
                        final List files) {
            this.module = module;
            this.parameters = parameters;
            this.impliedPorts = impliedPorts;
            this.explicitPorts = explicitPorts;
            this.files = files;
            assert (impliedPorts == null) != (explicitPorts == null);
        }

        /**
         * Get the name of the verilog module being instantiated.
         *
         * @return Name of the verilog module.
         **/
        public String getModule() {
            return module;
        }

        /**
         * Return parameters of the instantiation.
         **/
        public Iterator getParameters() {
            return parameters.iterator();
        }

        /**
         * Get a list of files this is depended on by the verilog module.
         *
         * @return An iterator of <code>String</code>s.
         **/
        public Iterator getFiles() {
            return files.iterator();
        }

        /**
         * Get a list of ports that are used to instantiate the verilog module.
         * The order of the ports as specified by the iterator is the order
         * that they were specified in CAST.
         *
         * @return An iterator of <code>String</code>s, or <code>null</code> if
         * port connections is specified explicitly.
         **/
        public Iterator getPortsByOrder() {
            return impliedPorts == null ? null : impliedPorts.iterator();
        }

        /**
         * Get a list of ports that are used to instantiate the verilog module.
         * The result is an iterator over a <code>Map</code>.  The key is the
         * name of the port in Verilog, and the value is the value of the CAST
         * expression to pass in to that port.
         *
         * @return An iterator of <code>Map.Entry</code>s (<code>String</code>
         * to <code>Value</code>), or <code>null</code> if port connections is
         * specified implicitly.
         **/
        public Iterator getPortsByName() {
            return explicitPorts == null ? null : explicitPorts.entrySet().iterator();
        }
    }

    public static abstract class NamedBlock extends BlockCommon {
        @Override
        public String getType() {
            return BlockInterface.VERILOG;
        }

        @Override
        public BlockInterface merge(BlockInterface o) {
            Debug.assertTrue(false, "VerilogBlock does not support merge");
            return null;
        }

        @Override
        public void refineFrom(final BlockInterface nb) {
            super.refineFrom(nb);
        }

        @Override
        public BlockInterface replace(BlockInterface o) {
            Debug.assertTrue(false, "VerilogBlock does not support replace");
            return null;
        }

        /**
         * Add a verilog module instantiate to this verilog block.
         *
         * @param inst the instance to add
         **/
        public abstract void addInstance(final Instance inst);

        /**
         * Get an iterator over all verilog module instantiations in this
         * verilog block.
         *
         * @return an iterator of <code>Instance</code>s.
         **/
        public abstract Iterator getInstances();

        /**
         * Add a list of files depended on by this verilog block.
         *
         * @param a list of <code>String</code>s
         **/
        public abstract void addFiles(final List files);

        /**
         * Get a list of files depended on by this verilog block.
         *
         * @return an iterator of <code>String</code>s.
         **/
        public abstract Iterator getFiles();

        /**
         * Get the name of this verilog block.
         *
         * @return name of the verilog block.
         **/
        public abstract String getName();
    }

    public static class SimpleNamedBlock extends NamedBlock {
        private final List instances;
        private final LinkedList files;
        private final String name;

        public SimpleNamedBlock(final String name) {
            this.name = name;
            this.instances = new ArrayList();
            this.files = new LinkedList();
        }

        @Override
        public void addInstance(final Instance inst) {
            instances.add(inst);
        }

        @Override
        public Iterator getInstances() {
            return instances.iterator();
        }

        @Override
        public void addFiles(final List files) {
            this.files.addAll(files);
        }

        @Override
        public Iterator getFiles() {
            return files.iterator();
        }

        @Override
        public String getName() {
            return name;
        }
    }

    private static class MergedNameBlock extends NamedBlock {
        private final NamedBlock parent;
        private final NamedBlock child;

        public MergedNameBlock(final NamedBlock parent,
                               final NamedBlock child) {
            this.parent = parent;
            this.child = child;
            refineFrom(parent);
            refineFrom(child);
        }

        @Override
        public void addInstance(final Instance inst) {
            throw new AssertionError("addInstance shouldn't be called");
        }

        @Override
        public Iterator getInstances() {
            final Iterator i = child.getInstances();
            return i.hasNext() ? i : parent.getInstances();
        }

        @Override
        public void addFiles(final List files) {
            throw new AssertionError("addFiles shouldn't be called");
        }

        @Override
        public Iterator getFiles() {
            return new FlatteningIterator(
                    Arrays.asList(parent.getFiles(),
                                  child.getFiles()).iterator());
        }

        @Override
        public String getName() {
            return parent.getName();
        }
    }


    private Map namedBlocks;

    public VerilogBlock() {
        this.namedBlocks = new HashMap();
    }

    public String getType() {
        return BlockInterface.VERILOG;
    }

    public void addNamedBlock(final NamedBlock block) {
        namedBlocks.put(block.getName(), block);
    }

    /**
     * Get the verilog block associated with a particular name.
     *
     * @param name Which verilog block to return
     * @return A <code>NamedBlock</code> associated with <code>name</code> or
     * <code>null</code> if there is no block by that name.
     **/
    public NamedBlock getNamedBlock(final String name) {
        return (NamedBlock) namedBlocks.get(name);
    }

    /**
     * Get an iterator over all named verilog blocks.
     *
     * @return An iterator of <code>String</code>s.
     **/
    public Iterator getNamesIterator() {
        return namedBlocks.keySet().iterator();
    }

    public BlockInterface merge(BlockInterface o) {
        Debug.assertTrue(false, "VerilogBlock does not support merge");
        return null;
    }

    public void refineFrom(BlockInterface o) {
        super.refineFrom(o);
        final VerilogBlock vb = (VerilogBlock) o;
        for (Iterator i = vb.getNamesIterator(); i.hasNext(); ) {
            final String name = (String) i.next();
            final NamedBlock onb = vb.getNamedBlock(name);
            final NamedBlock nb = getNamedBlock(name);
            addNamedBlock(nb == null ? onb
                                     : new MergedNameBlock(onb, nb));
        }
    }

    public BlockInterface replace(BlockInterface o) {
        Debug.assertTrue(false, "VerilogBlock does not support replace");
        return null;
    }

}
