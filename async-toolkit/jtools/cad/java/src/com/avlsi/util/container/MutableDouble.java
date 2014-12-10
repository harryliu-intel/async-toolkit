package com.avlsi.util.container;

public class MutableDouble {
    private double value;
    public MutableDouble(double value) {
        this.value = value;
    }
    /** Return current value **/
    public double get() {
        return value;
    }
    /** Set value **/
    public void set(double value) {
        this.value = value;
    }
    public void max(double other) {
        value = Math.max(value, other);
    }
    public void min(double other) {
        value = Math.min(value, other);
    }
    public boolean equals(Object o) {
        return o instanceof MutableDouble && ((MutableDouble) o).get() == get();
    }
    public int hashCode() {
        return (int) value;
    }
}
