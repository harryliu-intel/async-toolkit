package com.avlsi.csp.csp2java.runtime;

/**
 * Exception thrown by CSP runtime if non-determinism occurs in a deterministic
 * statement.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class NonDeterminismException extends RuntimeException {
    /**
     * Class constructor.
     **/
    public NonDeterminismException(final String message) {
        super(message);
    }
}
