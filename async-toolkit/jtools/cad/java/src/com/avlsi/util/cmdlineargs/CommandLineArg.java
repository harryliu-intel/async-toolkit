// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs;


/**
   Interface to a parsed command line argument.
 */
public interface CommandLineArg {

    /**
       @return The name of the command line argument.
     */
    String getName();

    /**
       @return The value of the command line argument or null if the
       argument had no value.
     */
    String getValue();

}
