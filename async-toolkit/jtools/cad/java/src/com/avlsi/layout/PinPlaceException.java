// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

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
