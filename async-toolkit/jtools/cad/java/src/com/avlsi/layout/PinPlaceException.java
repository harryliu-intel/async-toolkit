package com.avlsi.layout;

import com.avlsi.layout.CellProcessorException;

public class PinPlaceException extends CellProcessorException {
    
    public PinPlaceException(final String str) {
	super(str);
    }


    public PinPlaceException(final Exception e) {
	super(e);
    }
}
