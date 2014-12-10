package com.avlsi.csp.csp2java.runtime;

import com.avlsi.csp.csp2java.runtime.Fold.BinaryFunction; 

public interface CspValue {
    void setValue(CspValue value);
    void setValue(CspValue value, BinaryFunction modifier);
}
