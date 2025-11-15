// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2012 Intel Corporation.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.graph;

public interface Edge {
    public Vertex getSnk();
    public Vertex getSrc();
    public double getWeight();
    public double relax(double srcWeight, double currSnkWeight);
}
