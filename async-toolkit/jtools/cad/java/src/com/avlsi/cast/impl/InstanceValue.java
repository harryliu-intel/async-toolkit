/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.cast.impl;

import com.avlsi.cell.CellImpl;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.debug.Debug;
import com.avlsi.file.common.HierName;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.container.Pair;

/**
 * A value of an instantiated type.  Its fields may be accessed.
 * An instance value needs to know the name and type of its ports.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class InstanceValue
    extends Value
    implements FieldedValueInterface {

    public static final int INLINE_NONE = 0;
    public static final int INLINE_INLINE = 1;
    public static final int INLINE_FLATTEN = 2;
    private final HierName instanceName;

    /**
     * The cell definition for this instance.
     **/
    private final CellInterface defCell;
    private final Symbol typeName;
    private final TupleValue metaParams;
    private final BlockEnvironment paramEnv;
    private final Environment prsEnv;
    private final Environment subcellsEnv;
    private final Symbol[] portParams;
    private final Symbol[] impliedPortParams;
    private final String[] impliedPortParentNodes;
    private final int[] inlineState;

    public static InstanceValue valueOf(final Value val)
        throws InvalidOperationException{
        if (val instanceof InstanceValue)
            return (InstanceValue) val;
        else
            throw new InvalidOperationException(
                    "no conversion of " +
                    (val.getInstanceName() == null ? 
                        "unamed value " + val :
                        "value named " + val.getInstanceName()) +
                    " to instance value");
    }

    /**
     * The environment that is passed in should be have an entry for each
     * field in the type.
     **/
    public InstanceValue(final HierName instanceName,
                         final CellInterface defCell,
                         final Symbol typeName,
                         final TupleValue metaParams,
                         final BlockEnvironment paramEnv,
                         final Environment prsEnv,
                         final Environment subcellsEnv,
                         final Symbol[] portParams,
                         final Symbol[] impliedPortParams,
                         final String[] impliedPortParentNodes,
                         final int[] inlineState) {
        super(true);

        this.instanceName = instanceName;
        this.defCell = defCell;
        this.typeName = typeName;
        this.metaParams = metaParams;
        this.paramEnv = paramEnv;
        this.prsEnv = prsEnv;
        this.subcellsEnv = subcellsEnv;
        this.portParams = portParams;
        this.impliedPortParams = impliedPortParams;
        this.impliedPortParentNodes = impliedPortParentNodes;
        this.inlineState = inlineState;
    }

    /**
     * Return a duplicate suitable for use in constructing an array.
     * Everying is the same, but the environment is a sibling
     * of the original environment (same parent, copied local contents).
     **/
    public Value duplicate() {
        return new InstanceValue(instanceName, defCell, typeName, metaParams,
                paramEnv.getSiblingEnvironment(), prsEnv, subcellsEnv,
                portParams, impliedPortParams, impliedPortParentNodes,
                new int[] { inlineState[0] });
    }

    public Pair processSubtypes(final boolean duplicate) {
        final Environment env;
        final BlockEnvironment envParam;
        final Environment envSubcells;

        assert inlineState[0] == INLINE_NONE ||
               inlineState[0] == INLINE_INLINE :
               "Cannot flatten in the subtypes block: " + this;

        if (inlineState[0] == INLINE_NONE) {
            envParam = new BlockEnvironment(paramEnv);
            envSubcells = subcellsEnv;
            env = envParam;
        } else {
            if (subcellsEnv == null) {
                envParam = new BlockEnvironment(paramEnv);
                envSubcells = subcellsEnv;
                env = envParam;
            } else {
                envParam = paramEnv;
                envSubcells = new BlockEnvironment(subcellsEnv);
                env = envSubcells;
            }
        }

        final InstanceValue iv =
            duplicate ? new InstanceValue(instanceName, defCell, typeName,
                                          metaParams, envParam, prsEnv,
                                          envSubcells, portParams,
                                          impliedPortParams,
                                          impliedPortParentNodes,
                                          new int[] { inlineState[0] })
                      : this;

        return new Pair(env, iv);
    }

    private Value accessField(final Symbol sym) throws
        InvalidOperationException {
        return accessField(sym, FieldedValueInterface.INSTANCE_PERMISSION);
    }

    public Value accessField(final Symbol sym, final int permission)
        throws InvalidOperationException {
        assert permission == FieldedValueInterface.ALL_PERMISSION ||
               permission == FieldedValueInterface.INSTANCE_PERMISSION :
               "Unknown field access permission: " + permission;
        try {
            final Environment env;
            // prsEnv and subcellsEnv will be null if this is a channel
            if (inlineState[0] == INLINE_NONE &&
                permission != FieldedValueInterface.ALL_PERMISSION)
                env = paramEnv;
            else if (inlineState[0] == INLINE_INLINE &&
                     permission != FieldedValueInterface.ALL_PERMISSION)
                env = subcellsEnv == null ? paramEnv : subcellsEnv;
            else {
                if (subcellsEnv != null && prsEnv != null) {
                    env = new SplicingEnvironment(subcellsEnv, prsEnv);
                } else if (subcellsEnv != null) {
                    env = subcellsEnv;
                } else if (prsEnv != null) {
                    env = prsEnv;
                } else {
                    env = paramEnv;
                }
            }
            final Value v = env.lookup(sym);
            if (v == null)
                throw new InvalidOperationException("symbol not bound: " +
                                                    sym);
            final HierName newName =
                HierName.makeHierName(getInstanceName(), v.getInstanceName());
            final Value result;
            if (v instanceof InstanceValue &&
                inlineState[0] == INLINE_FLATTEN) {
                final InstanceValue iv = (InstanceValue) v;
                result = new InstanceValue(newName, iv.defCell, iv.typeName,
                                           iv.metaParams, iv.paramEnv,
                                           iv.prsEnv, iv.subcellsEnv,
                                           iv.portParams, iv.impliedPortParams,
                                           iv.impliedPortParentNodes,
                                           new int[] { INLINE_FLATTEN } );
            } else {
                result = v.newInstanceName(newName);
            }
            return result;
        } catch (AmbiguousLookupException e) {
            throw (AssertionFailure)
                new AssertionFailure("ambiguous lookup in cell/channel")
                    .initCause(e);
        }
    }

    /**
     * Helper function for assign: matches port lists (implied or not)
     * to values.  Not used when the implied ports aren't specified.
     **/
    private void assignPorts(Symbol[] ports, TupleValue values, CellImpl cell)
        throws InvalidOperationException {
        if (values.getSize()==0) return; // empty () is OK and makes no connections
        if (ports.length != values.getSize())
            throw new InvalidOperationException(
                        "Initializer with wrong number of ports: "
                        + ports.length + "!=" + values.getSize());
        for (int i = 0; i < ports.length; ++i) {
            final Value fieldValue = values.accessTuple(i);
            
            if (!(fieldValue instanceof AnonymousValue))
                try {
                    accessField(ports[i]).assign(fieldValue, cell);
                } catch (InvalidOperationException e) {
                    throw new InvalidOperationException(
                        "cannot connect port " + ports[i].getString(), e);
                }
        }
    }

    /**
     * Looks up the implied port symbols in the parent cell and pulls
     * them into the child cell too.  Called when assign isn't,
     * because the implied ports should always be hooked up.
     * @param cell parent cell for this cell
     **/
    public void assignDefaultsToImpliedPorts(CellImpl cell)
        throws InvalidOperationException {
        for (int i = 0; i < impliedPortParams.length; ++i) {
            final HierName parentNodeName =
                HierName.makeHierName(impliedPortParentNodes[i]);
            CellInterface defaultCell =
                (CellInterface) cell.getSubcell(parentNodeName);
            
            // If the node isn't in the parent cell, then the parent
            // cell ought to be a top-level cell which isn't expected
            // to necessarily have globals.  If it's a top-level cell,
            // it ought to alias names with a '!' after them to their
            // '!'less forms, because external tools expect both forms
            // to be available.
            if (defaultCell == null) {
                if (cell.isSyntheticP()) {
                    final CellInterface node =
                        new NodeValue(parentNodeName).getCell();

                    Debug.assertTrue(!parentNodeName.isGlobal());
                    cell.addSubcellPair(parentNodeName, node, false);
                    defaultCell = node;
                } else {
                    throw new InvalidOperationException(
                        "implied port " + impliedPortParams[i].getString() +
                        " not bound in parent cell: " +
                        cell.getFullyQualifiedType() +
                        " so can't bind it in child: " +
                        defCell.getFullyQualifiedType());
                }
            }
            
            // If the name isn't a node
            if (! defaultCell.isNode()) {
                throw new InvalidOperationException("implied port " + impliedPortParams[i] + " must be a node");
            }
            final Value defaultValue = new NodeValue(parentNodeName);
            
            accessField(impliedPortParams[i]).assign(defaultValue, cell);
      }
    }

    /**
     * Assignment from another InstanceValue or a
     * TupleValue/TupleGroupValue.  A TupleValue or TupleGroupValue
     * can only come from an initializer.  If the value is a
     * TupleValue/TupleGroupValue, each component is assigned unless
     * it is an AnonymousValue.
     **/
    public Value assign(final Value v, final CellImpl cell)
        throws InvalidOperationException {
        if (v instanceof TupleValue) {
            final TupleValue tv = (TupleValue) v;
            assignPorts(portParams, tv, cell);
            assignDefaultsToImpliedPorts(cell);
        }
        else if (v instanceof TupleGroupValue) {
            final TupleGroupValue tgv = (TupleGroupValue) v;
            final TupleValue portValues = tgv.getPortList();
            final TupleValue impliedPortValues = tgv.getImpliedPortList();
            assignPorts(portParams, portValues, cell);

            // If the implied port list is explictly empty, it means the
            // implied connections should be skipped
            if (impliedPortValues.getSize() != 0) {
                assignPorts(impliedPortParams, impliedPortValues, cell);
            }
        }
        else {
            final InstanceValue instanceVal = valueOf(v);

            //            cell.addConnection(getInstanceName(), instanceVal.getInstanceName());

            if (!getCell().eventuallyRefinesFrom(instanceVal.getCell()) &&
                !instanceVal.getCell().eventuallyRefinesFrom(getCell()))
                throw new InvalidOperationException(
                        "Types are not compatible: "
                        + getType().getString() + " / "
                        + instanceVal.getType().getString());

            // recursively assign the port parameters
            Debug.assertTrue(portParams.length == instanceVal.portParams.length);

            for (int i = 0; i < portParams.length; ++i) {
                final Symbol sym1 = portParams[i];
                final Symbol sym2 = instanceVal.portParams[i];

                Debug.assertTrue(sym1.equals(sym2));

                accessField(sym1).assign(instanceVal.accessField(sym1), cell);
            }
        }

        return this;
    }

    public String getTypeName() {
        return typeName.getString();
    }

    public CellInterface getCell() {
        return defCell;
    }

    public HierName getInstanceName() {
        return instanceName;
    }

    public Environment getParamEnv() {
        return paramEnv;
    }

    public Symbol[] getPortParams() {
        return portParams;
    }

    public Value newInstanceName(final HierName newInstanceName) {
        return new InstanceValue(newInstanceName, defCell, typeName,
                metaParams, paramEnv, prsEnv, subcellsEnv, portParams,
                impliedPortParams, impliedPortParentNodes, inlineState);
    }

    public String toString() {
        return "InstanceValue(typeName: "
            + typeName + ", metaParams: "
            + metaParams + ", paramEnv: "
            + paramEnv.getLocalEnvironmentString() + ", prsEnv: "
            + prsEnv + ", subcellsEnv: "
            + subcellsEnv + ")";
    }

    public Type getType() {
        return new InstanceValueType(this);
    }

    public boolean eventuallyRefinesFrom(Value v)
        throws InvalidOperationException {
        if (! (v instanceof InstanceValue))
            throw new InvalidOperationException("Can't refine an instance value from " + v.getType().getString());
        InstanceValue iv = (InstanceValue) v;
        return this.getCell().eventuallyRefinesFrom(iv.getCell());
    }

    public void setInline(final int inlineState) {
        assert inlineState == INLINE_NONE || inlineState == INLINE_INLINE ||
               inlineState == INLINE_FLATTEN :
               "Invalid inline state: " + inlineState;

        this.inlineState[0] = inlineState;
    }

    final class InstanceValueType extends Type {
        private final InstanceValue iv;

        // for some reason, inner classes aren't working like I want,
        // so I need to do this.  What's the right syntax?
        //
        // public class X {
        //     int n = 0;
        // 
        //     class Y {
        //         void f(Y y) {
        //             n = 3;
        //             y.n = 4;
        //         }
        //     }
        // }

        public InstanceValueType(final InstanceValue iv) {
            this.iv = iv;
        }

        public boolean equals(final Object o) {
            if (o instanceof InstanceValueType)
                return equals((InstanceValueType) o);
            else
                return false;
        }

        public boolean equals(final InstanceValueType ivt) {
            return iv.typeName.equals(ivt.iv.typeName)
                && iv.metaParams.equals(ivt.iv.metaParams);
        }

        public boolean typeEquals(final InstanceValueType ivt) {
            return iv.typeName.equals(ivt.iv.typeName);
        }

        public String toString() {
            return "InstanceValueType( typeName: "
                + iv.typeName + ",  metaParams: "
                + iv.metaParams + ")";
        }

        public String getString() {
            return UserDefinedValue.getTypeName(iv.typeName.getString(),
                                                iv.metaParams);
        }
    }
}
