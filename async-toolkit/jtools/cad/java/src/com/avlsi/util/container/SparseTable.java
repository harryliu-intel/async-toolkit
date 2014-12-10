package com.avlsi.util.container;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class SparseTable {
    private final Map table;
    private final Set columns;
    private final Set rows;

    public SparseTable() {
        table = new HashMap();
        columns = new HashSet();
        rows = new HashSet();
    }

    public void putEntry(final double row, final double column,
                         final double value) {
        putEntry(new Double(row), new Double(column), new Double(value));
    }

    public void putEntry(final Double row, final Double column,
                         final Double value) {
        table.put(new Pair(row, column), value);
        columns.add(column);
        rows.add(row);
    }

    public Double getEntry(final Double row, final Double column) {
        return (Double) table.get(new Pair(row, column));
    }

    public Set getColumns() {
        return Collections.unmodifiableSet(columns);
    }

    public Set getRows() {
        return Collections.unmodifiableSet(rows);
    }
}
