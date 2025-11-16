// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.parser;


import java.util.NoSuchElementException;

import com.avlsi.file.cdl.parser.Template;

public interface TemplateIterator {

    boolean hasNext();

    Template next() throws NoSuchElementException;

}
