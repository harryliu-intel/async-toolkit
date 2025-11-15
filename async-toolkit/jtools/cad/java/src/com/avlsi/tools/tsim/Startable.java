// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 *      Startable.java - interface commmon to AbstractDevice and interior
 *
 *      Copyright 2002 Fulcrum Microsystems, Inc.  All rights reserved.
 *
 *      $Id$
 */

package com.avlsi.tools.tsim;

/**
 * This is an interface for anything which can be started and
 * has a name.  Basically, these are the two methods from AbstractDevice
 * which we also want interior containers (such as Maelstrom) to have.
 *
 * @author Patrick Pelletier
 */
public interface Startable {
    /**
     * Starts this Startable.
     */
    void start();

    /**
     * Get the full name of this Startable.
     */
    String getFullname();
}
