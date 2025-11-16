// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.io;

import com.avlsi.io.SearchPathFile;

/**
   A version of the FileFilter interface for search path's.
 */
public interface SearchPathFileFilter {

    /**
       @return true if the filter is allow the file f to pass through or false if the file f should be filtered.
     */
    boolean accept( SearchPathFile f );

}
