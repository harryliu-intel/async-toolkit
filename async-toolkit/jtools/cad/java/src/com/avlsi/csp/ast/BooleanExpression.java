package com.avlsi.csp.ast;

public class BooleanExpression extends IntegerExpression {
    private final boolean val;
    public BooleanExpression(final boolean val) {
        super(val ? "-1" : "0", 10);
        this.val = val;
    }
    public boolean booleanValue() {
        return val;
    }
}
