package com.avlsi.util.container;

/**
 * A class can implement the <code>LinearInterpolatable</code> interface to
 * indicate it can be viewed as a scalar field sampled on a grid.
 **/
public interface LinearInterpolatable {
    /**
     * Return the smaller of the indicies of the two closest points along the
     * specified dimension of a table that brackets <code>value</code>, or a
     * negative value if no bracket exists.
     **/
    int getBracket(int dimension, double value);

    /**
     * Return the point associated with index in the specified dimension of a
     * table.
     **/
    double getEntry(int dimension, int index);

    /**
     * Return the scalar value associated with the indices.
     **/
    double getEntry(int[] indices);
}
