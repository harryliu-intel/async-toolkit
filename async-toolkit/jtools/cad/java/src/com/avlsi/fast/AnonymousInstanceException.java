package com.avlsi.fast;

import java.util.Collection;

public class AnonymousInstanceException extends RuntimeException {
    /**
     * A collection of names of cell types that contain anonymous instances.
     **/
    private Collection<String> cells;
    public AnonymousInstanceException(final Collection<String> cells) {
        this.cells = cells;
    }
    public Collection<String> getCells() {
        return cells;
    }
}
