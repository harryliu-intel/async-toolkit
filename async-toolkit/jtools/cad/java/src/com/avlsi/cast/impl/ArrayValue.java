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
import com.avlsi.fast.metaparameters.ArrayMetaParam;
import com.avlsi.fast.metaparameters.MetaParamTypeInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.cosim.CoSimChannelNames;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;

import java.util.Arrays;
import java.util.Iterator;

/**
 * This class represents an array. Assignment is element by element -
 * elements from the RHS are assigned into the corresponding index in
 * the LHS array.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class ArrayValue
    extends Value
    implements MetaParamValueInterface {

    /** None of the Values in vals are ever ArrayValues. **/
    private Value[] vals;
    private SubscriptSpecInterface spec;
    private final HierName instanceName;
    /**
     * Whether the last dimension of this array is actually a channel width.
     **/
    private final boolean wideChannelP;

    /**
     * Construct an ArrayValue with the given values, spec, and
     * instanceName.  The instanceName will be null if the array 
     * was constructed from a tuple (anonymous array).
     **/
    public ArrayValue(final Value[] vals,
                      final SubscriptSpecInterface spec,
                      final HierName instanceName,
                      final boolean wideChannelP) {
        super(true);

        if (vals.length != spec.getNumElements())
            throw new IllegalArgumentException("sizes don't match");

        this.spec = spec;
        this.vals = new Value[vals.length];

        for (int i = 0; i < vals.length; ++i)
            this.vals[i] = vals[i];

        // TODO: somewhere, we need to make sure that the element
        // type is homogeneous and that none of them are ArrayValues

        this.instanceName = instanceName;
        this.wideChannelP = wideChannelP;
    }

    /**
     * Convert if the value is an ArrayValue, return it with 
     * type ArrayValue.  Otherwise, throw an InvalidOperationException.
     **/
    public static ArrayValue valueOf(final Value val)
        throws InvalidOperationException {
        if (val instanceof ArrayValue)
            return (ArrayValue) val;
        else
            throw new InvalidOperationException("value named " + val.getInstanceName() + " isn't an array.");
    }

    public Value duplicate() {
        assert isDefined();  // arrays are always defined

        final Value[] newVals = new Value[vals.length];

        for (int i = 0; i < vals.length; ++i)
            newVals[i] = vals[i].duplicate().newInstanceName(instanceName.appendString(DenseSubscriptSpec.idxToString(spec.indexOf(i))));

        return new ArrayValue(newVals, spec, instanceName, wideChannelP);
    }

    /**
     * Return a copy of the array, duplicating only the subscript spec.
     * The underlying pointers are the same.  Thus, changes to the
     * elements of the new array affect the old.  Changes to the
     * size do not.
     **/
    public Value shallowCopy() {
        assert isDefined();  // arrays are always defined

        return new ArrayValue(vals, spec, instanceName, wideChannelP);
    }

    /**
     * Returns the value resulting from subscripting.  This will be
     * an ArrayValue if there is more than one element, or some
     * other kind of Value if there is only one.  This prevents
     * all other types from needing to know how to convert from array.
     * <p>
     * In order for the access to be valid, the specifications must
     * both have the same number of dimensions, and all the indices of the
     * accessSpec must present in the array's spec.
     **/
    public Value accessArray(final SubscriptSpecInterface accessSpec)
        throws InvalidOperationException {
        if (spec.getNumDimensions() != accessSpec.getNumDimensions())
            throw new InvalidOperationException("dims don't agree.  array has " + spec.getNumDimensions() + " dimensions but accessor has " + accessSpec.getNumDimensions());

        final Value[] newVals = new Value[accessSpec.getNumElements()];

        for (int i = 0; i < newVals.length; ++i) {
            // spec.positionOf(idx) will throw IndexOutOfBoundsException
            // if idx is not in the spec
            final int[] idx = accessSpec.indexOf(i);
            final int pos;

            try {
                pos = spec.positionOf(idx);
            } catch (IndexOutOfBoundsException e) {
                throw new InvalidOperationException("bad array access of "
                        + instanceName + ": index "
                        + DenseSubscriptSpec.idxToString(idx)
                        + " not in spec " + spec, e);
            }

            newVals[i] = vals[pos];
        }

        if (newVals.length == 1) {
            if (instanceName == null)
                return newVals[0];
            else if (instanceName.isGlobal()) {
                // code in CastTree.type duplicates this
                final int bangIdx =
                    instanceName.getSuffixString().indexOf('!');
                assert bangIdx == instanceName.getSuffixString().length() - 1;
                return newVals[0].newInstanceName(
                        HierName.makeSiblingName(instanceName, 
                            instanceName.getSuffixString().substring(0,
                                bangIdx)
                            + DenseSubscriptSpec.idxToString(
                                accessSpec.indexOf(0)) + '!'));
            } else {
                // code in CastTree.type duplicates this
                return newVals[0].newInstanceName(
                        HierName.makeSiblingName(instanceName, 
                            instanceName.getSuffixString()
                            + DenseSubscriptSpec.idxToString(
                                accessSpec.indexOf(0))));
            }
        } else
            return new ArrayValue(newVals, accessSpec, instanceName,
                                  wideChannelP);
    }

    /**
     * Access a single index.
     **/
    public Value accessArray(final int[] idx) {
        try {
            final Range[] rs = new Range[idx.length];

            for (int i = 0; i < rs.length; ++i)
                rs[i] = new Range(idx[i], idx[i]);

            return accessArray(new DenseSubscriptSpec(rs));
        } catch (InvalidOperationException e) {
            throw (AssertionFailure)
                new AssertionFailure("bad index? " + e).initCause(e);
        }
    }


    public void replaceArray(final ArrayValue other)
        throws InvalidOperationException {
        if (spec.getNumDimensions() != other.spec.getNumDimensions())
            throw new InvalidOperationException("dims don't agree.  array has " + spec.getNumDimensions() + " dimensions but replace has " + other.spec.getNumDimensions());

        final int elements = other.spec.getNumElements();

        for (int i = 0; i < elements; ++i) {
            // spec.positionOf(idx) will throw IndexOutOfBoundsException
            // if idx is not in the spec
            final int[] idx = other.spec.indexOf(i);
            final int pos;

            try {
                pos = spec.positionOf(idx);
            } catch (IndexOutOfBoundsException e) {
                throw new InvalidOperationException("bad array access of "
                        + instanceName + ": index "
                        + DenseSubscriptSpec.idxToString(idx)
                        + " not in spec " + spec, e);
            }

            vals[pos] = other.vals[i];
        }
    }

    /**
     * Array assignment is valid as long as the sizes are conforming.
     * The sizes conform if the sequences of non-one sizes for each
     * dimension are the same.  Performs implicit conversion from 
     * singleton to array[1].
     **/
    public Value assign   (final Value v, final CellImpl cell)
        throws InvalidOperationException {
        final ArrayValue av;
        if (v instanceof ArrayValue)
            av = (ArrayValue) v;
        else
            av = new ArrayValue(
                    new Value[]{v},
                    new DenseSubscriptSpec(new Range[]{new Range(0, 0)}),
                    // PR 228, use null here
                    null,
                    false);

        if (spec.getNumElements() != av.spec.getNumElements())
            throw new InvalidOperationException
                ("sizes don't conform: trying to assign " + av.spec +
                 " with " + av.spec.getNumElements() + " elements to " + spec +
                 " with " + spec.getNumElements() + " elements.");

        // do assignment in lexicographic order
        assert vals.length == av.vals.length;
        for (int i = 0; i < vals.length; ++i) {
            final Value avv = av.accessArray(av.spec.indexOf(i));
            if (!(avv instanceof AnonymousValue)) {
                accessArray(spec.indexOf(i)).assign(avv, cell);
            }
        }

        return this;
    }

    /**
     * Attempt to augment the existing array with additional indices.
     * Several conditions must be met for this to be legal:
     * <ul>
     *   <li> The element types must agree
     *   <li> The number of dimensions must agree
     *   <li> The indices that exist in the old array must be disjoint
     *        from the ones that are augmenting it.
     * </ul>
     * The resulting array will have a sparse spec.
     * The augmenting array must have a dense spec.
     **/
    public void augment(final ArrayValue av) throws InvalidOperationException
    {
        if (!typeCompatible(getElementType(), av.getElementType()))
            throw new InvalidOperationException("element types don't agree");

        if (spec.getNumDimensions() != av.spec.getNumDimensions())
            throw new InvalidOperationException("dims must agree");

        final int n1 = vals.length;
        final int n2 = av.vals.length;

        final Value[] newVals = new Value[n1 + n2];

        spec = SparseSubscriptSpec.augment(spec, vals,
                                           av.spec, av.vals,
                                           newVals);
        vals = newVals;
    }

    /**
     * Makes an array with the given index specification, filled with
     * copies of the value <code>v</code>.  Assumes that the array is
     * not a wide channel.
     **/
    public static ArrayValue makeArray(final Value v,
                                       final SubscriptSpecInterface spec)
    {
        return makePossiblyWideArray(v, spec, false);
    }

    public static ArrayValue makeWideArray(final Value v,
                                           final SubscriptSpecInterface spec)
    {
        return makePossiblyWideArray(v, spec, true);
    }

    private static ArrayValue makePossiblyWideArray(final Value v,
                                                    final SubscriptSpecInterface spec,
                                                    final boolean wideP)
    {
        final Value[] vals = new Value[spec.getNumElements()];
        final HierName instanceName;
        if (v.getInstanceName() == null)
            instanceName = HierName.makeHierName("<unknown>");
        else
            instanceName = v.getInstanceName();

        for (int i = 0; i < vals.length; ++i)
            vals[i] = v.duplicate().newInstanceName(
                    instanceName.appendString(DenseSubscriptSpec.idxToString(spec.indexOf(i))));

        // v.getInstanceName() may be null, ie it is an integer array

        // spec is be immutable
        return new ArrayValue(vals, spec, instanceName, wideP);
    }

    /**
     * Turns the tuple into an array.  Fails if the types are 
     * not all the same.  Used to provide support for anonymous arrays.
     **/
    public static ArrayValue fromTuple(final Value val) 
        throws InvalidOperationException {

        final TupleValue tv = TupleValue.valueOf(val);

        final int n = tv.getSize();

        if (n == 0)
            throw new InvalidOperationException("0-tuple connot be "
                    + "converted to an array");

        Type t = null;
        int tidx = -1;
        final Value[] vals = new Value[n];

        // make sure the types agree, and copy pointer to value
        for (int i = 0; i < n; ++i) {
            final Value v = tv.accessTuple(i);
            if (!(v instanceof AnonymousValue)) {
                if (t == null) {
                    t = v.getType();
                    tidx = i;
                } else if (!v.getType().equals(t)) {
                    throw new InvalidOperationException("types don't agree "
                            + "for elems " + i + " and " + tidx);
                }
            }
            vals[i] = v;
        }

        if (t instanceof ArrayValueType) {
            // We must manually construct a multi-dimensional array or
            // we will end up with an array of arrays, which none of
            // the other code supports.
            final ArrayValueType avt = (ArrayValueType) t;
            final int m = avt.getSubscriptSpec().getNumElements();
            final Value[] vs = new Value[n * m];

            // make the new value array
            int k = 0;
            for (int j = 0; j < n; ++j) {
                // The Value must be an ArrayValue because the type
                // was ArrayValueType.
                final ArrayValue v = (ArrayValue) vals[j];
                final SubscriptSpecInterface spec = v.getSpec();
                for (int i = 0; i < m; ++i, ++k)
                    vs[k] = v.accessArray(spec.indexOf(i));
            }

            // make the new subscript spec
            // XXX: when we support SparseSubscriptSpecs, we will
            // need to pass all the specs, not just the first
            final SubscriptSpecInterface spec =
                repeatSubscriptSpec(avt.getSubscriptSpec(), n);

            return new ArrayValue(vs, spec, val.getInstanceName(), false);
        } else {
            return new ArrayValue(vals,
                    new DenseSubscriptSpec(new Range[]{new Range(0, n - 1)}),
                    val.getInstanceName(),
                    false);
        }
    }

    /**
     * Returns a new subscript spec of with one more dimension than
     * <code>spec</code>, the first dimension of the new spec will
     * have bounds <code>[0..n - 1]</code>.
     *
     * <pre><jml>
     *   private normal_behavior
     *     requires spec != null;
     *     requires spec instanceof DenseSubscriptSpec;
     *     requires n > 0;
     *     ensures \result != null;
     * </jml></pre>
     **/
    private static /*@ non_null @*/ SubscriptSpecInterface
        repeatSubscriptSpec(
            final /*@ non_null @*/ SubscriptSpecInterface spec,
            final int n) 
            throws InvalidOperationException {
        if (!(spec instanceof DenseSubscriptSpec))
            throw new InvalidOperationException("Cannot yet create " +
                    "anonymous multi-dimensional arrays from sparse arrays.");

        return repeatSubscriptSpec((DenseSubscriptSpec) spec,
                                   new Range(0, n - 1));
    }

    /**
     * Returns a new subscript spec of with one more dimension than
     * <code>spec</code>, the first dimension of the new spec will
     * have bounds specified by <code>r</code>.
     **/
    private static /*@ non_null @*/ DenseSubscriptSpec
        repeatSubscriptSpec(
            final /*@ non_null @*/ DenseSubscriptSpec spec, final Range r) {

        final Range[] newRanges = new Range[spec.getNumDimensions() + 1];
        newRanges[0] = r;
        for (int i = 1; i < newRanges.length; ++i)
            newRanges[i] = spec.getRange(i - 1);

        return new DenseSubscriptSpec(newRanges);
    }

    /**
     * Returns a new anonymous array, created by combining the arrays specified
     * in <code>vals</code> into a new dimension.  All the constituent arrays
     * must be dense arrays with the same shape.
     **/
    public static /*@ non_null @*/ ArrayValue
        augmentDimension(final ArrayValue[] vals, final Range r)
        throws InvalidOperationException {
        if (vals.length != r.size())
            throw new InvalidOperationException("Size of new dimension does " +
                    "not agree with number of elements in new dimension.");
        for (int i = 1; i < vals.length; ++i) {
            if (!vals[0].getSpec().equals(vals[i].getSpec())) {
                throw new InvalidOperationException("All elements of the " +
                        "array must have the same shape");
            }
        }
        if (!(vals[0].getSpec() instanceof DenseSubscriptSpec))
            throw new InvalidOperationException("Cannot yet create " +
                    "anonymous multi-dimensional arrays from sparse arrays.");

        final DenseSubscriptSpec newSpec =
            repeatSubscriptSpec((DenseSubscriptSpec) vals[0].getSpec(), r);
        final Value[] newVals = new Value[newSpec.getNumElements()];
        final int[] idx = new int[newSpec.getNumDimensions()];

        final DenseSubscriptSpec spec = (DenseSubscriptSpec) vals[0].getSpec();
        for (int i = 0; i < spec.getNumElements(); ++i) {
            final int[] subidx = spec.indexOf(i);
            System.arraycopy(subidx, 0, idx, 1, subidx.length);
            for (int j = 0; j < vals.length; ++j) {
                idx[0] = r.getMin() + j;
                newVals[newSpec.positionOf(idx)] = vals[j].accessArray(subidx);
            }
        }
        return new ArrayValue(newVals, newSpec, null, false);
    }

    public Type getType() throws InvalidOperationException {
        return new ArrayValueType(getElementType(), spec);
    }

    private static final class ArrayValueType extends Type {
        private final Type elementType;
        private final SubscriptSpecInterface spec;

        private ArrayValueType(final Type elementType,
                               final SubscriptSpecInterface spec) {
            this.elementType = elementType;
            this.spec = spec;
        }

        public boolean equals(final Object o) {
            if (!(o instanceof ArrayValueType))
                return false;

            final ArrayValueType that = (ArrayValueType) o;
            return typeCompatible(elementType, that.elementType) &&
                spec.getNumElements() == that.spec.getNumElements();
        }

        private Type getElementType() {
            return elementType;
        }

        private SubscriptSpecInterface getSubscriptSpec() {
            return spec;
        }

        public String getString() {
            // XXX: shouldn't use toString, but it does what we want
            return elementType.getString() + spec.toString();
        }
    }

    private static boolean typeCompatible(final Type t, final Type s) {
        if (t instanceof InstanceValue.InstanceValueType &&
            s instanceof InstanceValue.InstanceValueType) {
            final InstanceValue.InstanceValueType ivt =
                (InstanceValue.InstanceValueType) t;
            final InstanceValue.InstanceValueType ivs =
                (InstanceValue.InstanceValueType) s;
            return ivt.typeEquals(ivs);
        } else {
            return t.equals(s);
        }
    }

    public Type getElementType() {
        assert vals.length > 0;
        Type t = null;

        try {
            t = vals[0].getType();
            for (int i = 1; i < vals.length; ++i)
                assert typeCompatible(t, vals[i].getType());
        } catch (InvalidOperationException e) {
            // FIXME This should be handled properly
            throw new AssertionError("getElementType() called on " + this
                                     + " produced error message: " + e);
        }
        return t;
    }

    public HierName getInstanceName() {
        return instanceName;
    }

    public Value newInstanceName(final HierName newInstanceName) {
        assert isDefined();  // arrays are always defined

        return new ArrayValue(vals, spec, newInstanceName, wideChannelP);
    }

    public String getMetaParamString() throws InvalidOperationException {
        final StringBuffer sb = new StringBuffer();
        sb.append('{');

        for (int i = 0; i < vals.length; ++i) {
            final Value v = vals[i];

            if (i > 0)
                sb.append(",");

            // code similiar to UserDefinedValue.getTypeName
            if (v instanceof IntValue)
                sb.append(((IntValue) v).getValue());
            else if (v instanceof BoolValue)
                sb.append(((BoolValue) v).getValue());
            else
                throw new AssertionFailure("bad meta-param type");
        }

        sb.append('}');
        return sb.toString();
    }

    public String toString() {
        final StringBuffer sb = new StringBuffer();

        sb.append("ArrayValue(\n size: ");
        sb.append(vals.length + ",\n bounds: ");
        sb.append(spec + ",\n");

        for (int i = 0; i < vals.length; ++i) {
            final int[] idx = spec.indexOf(i);
            sb.append(DenseSubscriptSpec.idxToString(idx));
            sb.append(": ");
            sb.append(vals[i].toString());
            sb.append("\n");
        }

        sb.append(")\n");

        return sb.toString();
    }

    /**
     * Two arrays are equal if they have exactly the same subscript
     * specification, and all elements are the same according
     * to their equals methods.
     **/
    public boolean equals(final Object o) {
        if (!(o instanceof ArrayValue))
            return false;
        else
            return equals((ArrayValue) o);
    }

    public boolean equals(final ArrayValue av) {
        if (!spec.equals(av.spec))
            return false;
        else {
            assert vals.length == av.vals.length;
            for (int i = 0; i < vals.length; ++i)
                if (!vals[i].equals(av.vals[i]))
                    return false;
            return true;
        }
    }

    public int hashCode() {
        int hc = 0;

        for (int i = 0; i < vals.length; ++i)
            hc ^= vals[i].hashCode();

        return hc;
    }

    //
    // implements MetaParamValueInterface
    //

    public MetaParamTypeInterface toMetaParam() {
        final DenseSubscriptSpec denseSpec = (DenseSubscriptSpec) spec;

        return new ArrayMetaParam(
                toMetaParam(0, new int[denseSpec.getNumDimensions()]),
                denseSpec.getRange(0).getMin(),
                denseSpec.getRange(0).getMax());
    }

    /**
     * idx is a partially-specified coordinate into the array.  All of
     * the dimensions before dim are specified.
     **/
    private MetaParamTypeInterface[] toMetaParam(final int dim,
            final int[] idx) {
        final DenseSubscriptSpec denseSpec = (DenseSubscriptSpec) spec;

        final MetaParamTypeInterface[] subArray =
            new MetaParamTypeInterface[denseSpec.getSizeForDimension(dim)];

        // If this is the last dimension, describe a point value
        if (dim == denseSpec.getNumDimensions() - 1) {
            for (int i = 0; i < subArray.length; ++i) {
                idx[dim] = i + denseSpec.getRange(dim).getMin();
                /* The code commented out should work, and be functionally
                 * equivalent to the 3 lines below, but does not due to an
                 * apparent JIT bug in Sun's JVM build 1.4.2-b28, in mixed and
                 * 64-bit mode.  See bug 3043 for more information.
                subArray[i] =
                    (MetaParamTypeInterface) vals[denseSpec.positionOf(idx)];
                **/
                final int posOf = denseSpec.positionOf(idx);
                subArray[i] =
                    (MetaParamTypeInterface) vals[posOf];
            }
        }
        // If this isn't the last dimension, get ArrayMetaParams which
        // wrap up the following dimensions.
        else {
            for (int i = 0; i < subArray.length; ++i) {
                idx[dim] = i + denseSpec.getRange(dim).getMin();
                subArray[i] = new ArrayMetaParam(
                        toMetaParam(dim + 1, idx),
                        denseSpec.getRange(dim + 1).getMin(),
                        denseSpec.getRange(dim + 1).getMax());
            }
        }

        return subArray;
    }

    /**
     * Utility function.  Value returned should not be modified.
     **/

    public SubscriptSpecInterface getSpec() {
        return spec;
    }

    /**
     * Iterate over the values in the array.  If ArrayValues a and b
     * have the same SubscriptSpecInterface, their values are
     * guaranteed to be iterated in the same order.
     **/

    public Iterator<Value> getIterator() {
        return Arrays.asList(vals).iterator();
    }

    /**
     * For an array to be a specialization of another array, they have
     * to have matching SubscriptSpecInterface and each position in
     * the first must specialize the equivalent position in the second
     **/
    public boolean eventuallyRefinesFrom(Value v)
        throws InvalidOperationException {
        if (! (v instanceof ArrayValue))
            throw new InvalidOperationException("Can't refine an array value from " + v.getType().getString());
        ArrayValue av = (ArrayValue) v;
        if (this.spec.getNumElements() != av.spec.getNumElements())
            return false;

        Iterator thisVals = this.getIterator();
        Iterator otherVals = av.getIterator();

        while (thisVals.hasNext()) {
            Value thisVal = (Value) thisVals.next();
            Value otherVal = (Value) otherVals.next();
            if (! (thisVal.eventuallyRefinesFrom(otherVal)))
                return false;
        }

        assert !otherVals.hasNext() : "problem comparing arrays: contents theoretically match up, but not really";

        return true;
    }

    /**
     * Returns a name or array of names.  Under the cosim framework,
     * each of these names corresponds to a NodeChannel.  Note that
     * wide channels are handled properly here (one name for the whole
     * wide channel).
     **/
    public CoSimChannelNames getCoSimChannelNames() {
        return spec.getCoSimChannelNames(instanceName.getAsString('.'),
                                         wideChannelP);
    }

    /**
     * Return whether the last dimension of this array is actually a channel
     * width.
     **/
    public boolean isWideChannel() {
        return wideChannelP;
    }
}
