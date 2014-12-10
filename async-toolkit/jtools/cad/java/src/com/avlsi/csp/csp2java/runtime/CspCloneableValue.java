package com.avlsi.csp.csp2java.runtime;

import com.avlsi.csp.csp2java.runtime.Fold.BinaryFunction; 

public interface CspCloneableValue extends CspValue {
    CspCloneableValue duplicate();
}
