package com.avlsi.cast.impl;

import com.avlsi.cell.CellImpl;
import com.avlsi.file.common.HierName;

public final class AlintFaninValue extends Value {
    public enum State {
        ZERO   ("=0"),
        ONE    ("=1"),
        RISING ("+"),
        FALLING("-");

        private final String text;
        private State(final String text) {
            this.text = text;
        }
        public String toString() {
            return text;
        }
    }

    private final Value node;
    private final State state;

    public AlintFaninValue(final Value node, final State state) {
        super(true);
        this.node = node;
        this.state = state;
    }

    public Value duplicate() {
        throw new AssertionError("can't dup alint fanins; can't happen");
    }

    public Value assign(final Value v, final CellImpl Cell) 
        throws InvalidOperationException {
        throw new InvalidOperationException();
    }

    public Type getType() throws InvalidOperationException {
        throw new InvalidOperationException();
    }

    public State getState() {
        return state;
    }

    public HierName getNode() {
        return node.getInstanceName();
    }

    public String toString() {
        return "AlintFaninValue(" + node + " " + state + ")";
    }

    public Value newInstanceName(final HierName newInstanceName) {
        return new AlintFaninValue(node.newInstanceName(newInstanceName),
                                   state);
    }
}
