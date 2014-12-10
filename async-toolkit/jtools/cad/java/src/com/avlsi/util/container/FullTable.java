package com.avlsi.util.container;

import java.util.Arrays;

public class FullTable implements LinearInterpolatable {
    private final double[] rows;
    private final double[] columns;
    private final double[][] table;
    
    public FullTable(final double[] rows,
                     final double[] columns,
                     final double[][] table) {
        this.rows = rows;
        this.columns = columns;
        this.table = table;
    }

    public FullTable(final SparseTable table) {
        final Double[] rows =
            (Double[]) table.getRows().toArray(new Double[0]);
        Arrays.sort(rows);
        this.rows = new double[rows.length];
        for (int i = 0; i < rows.length; ++i) {
            this.rows[i] = rows[i].doubleValue();
        }

        final Double[] columns =
            (Double[]) table.getColumns().toArray(new Double[0]);
        Arrays.sort(columns);
        this.columns = new double[columns.length];
        for (int i = 0; i < columns.length; ++i) {
            this.columns[i] = columns[i].doubleValue();
        }

        this.table = new double[rows.length][columns.length];
        for (int i = 0; i < rows.length; ++i) {
            for (int j = 0; j < columns.length; ++j) {
                final Double entry = table.getEntry(rows[i], columns[j]);
                if (entry == null) {
                    throw new IncompleteException("No data found for (" +
                                                   rows[i] + ", " +
                                                   columns[j] + ")");
                } else {
                    this.table[i][j] = entry.doubleValue();
                }
            }
        }
    }

    private double[] chooseDimension(final int dimension) {
        assert dimension == 0 || dimension == 1;
        return dimension == 0 ? rows : columns;
    }

    public int getBracket(final int dimension, final double value) {
        final double[] vals = chooseDimension(dimension);
        int index = Arrays.binarySearch(vals, value);
        if (index < 0) {
            assert -vals.length - 1 <= index && index <= -1;
            index = -index - 2;
            if (index == vals.length - 1) index = -index;
        }
        return index;
    }

    public double getEntry(final int dimension, final int index) {
        final double[] vals = chooseDimension(dimension);
        return vals[index];
    }

    public double getEntry(final int[] indices) {
        assert indices.length == 2;
        return table[indices[0]][indices[1]];
    }

    public static class IncompleteException extends RuntimeException {
        public IncompleteException(String message) {
            super(message);
        }
    }
}
