package com.avlsi.csp.csp2java.runtime;

import com.avlsi.csp.csp2java.runtime.Fold.BinaryFunction; 

public class CspString implements CspCloneableValue {
    private String val;

    public CspString() {
        this("");
    }

    public CspString(final String val) {
        assert val != null;
        this.val = val;
    }

    public void setValue(CspValue v) {
        setValue((CspString) v);
    }

    public void setValue(CspString v) {
        this.val = v.val;
    }

    public void setValue(CspValue v, BinaryFunction modifier) {
        setValue((CspString) modifier.evaluate(this, v));
    }

    public CspString add(final CspString s) {
        return new CspString(val + s.val);
    }

    public String toString() {
        return val;
    }

    public boolean equals(final CspString s) {
        return val.equals(s.val);
    }

    public boolean equals(final Object o) {
        return o instanceof CspString && equals((CspString) o);
    }

    public int hashCode() {
        return val.hashCode();
    }

    public CspCloneableValue duplicate() {
        return new CspString(val);
    }
}
