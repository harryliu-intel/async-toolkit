package com.avlsi.util.container;

public class MutableInt {
    private int value;
    public MutableInt(int value) {
        this.value = value;
    }
    /** Increment and return new value **/
    public int inc() {
        return ++value;
    }
    /** Decrement and return new value **/
    public int dec() {
        return --value;
    }
    /** Return current value **/
    public int get() {
        return value;
    }
    /** Set value **/
    public void set(int value) {
        this.value = value;
    }
    public void max(int other) {
        value = Math.max(value, other);
    }
    public boolean equals(Object o) {
        return o instanceof MutableInt && ((MutableInt) o).get() == get();
    }
    public int hashCode() {
        return value;
    }
}
