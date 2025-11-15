// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.graph;

import com.avlsi.util.functions.UnaryPredicate;

public interface Graph {
    public void foreachVertex(final UnaryPredicate<Vertex> func);
}
