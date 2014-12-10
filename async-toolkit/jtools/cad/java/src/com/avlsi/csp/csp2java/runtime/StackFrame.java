package com.avlsi.csp.csp2java.runtime;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.avlsi.util.container.Pair;

public class StackFrame {
    private final String funcName;
    private final String callLocation;
    private final Map<String,Pair<String,CspValue>> variables;
    public StackFrame(final String funcName, final String callLocation) {
        this.funcName = funcName;
        this.callLocation = callLocation;
        this.variables =
            new HashMap<String,Pair<String,CspValue>>();
    }
    public void addVariable(final String name, final String ppos,
                            final CspValue val) {
        variables.put(name, new Pair<String,CspValue>(ppos, val));
    }
    public String getCallLocation() {
        return callLocation;
    }
    public String getFunctionName() {
        return funcName;
    }
    public Map<String,Pair<String,CspValue>> getVariables() {
        return Collections.unmodifiableMap(variables);
    }
}
