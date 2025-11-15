// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.sigscan;

/**
 * Class for scheduling the logging of signals
 * This event ONLY logs on fire, and does not set the var
 * @author Dan Daly
 * @version $Date$
 **/

class LoggedVarEvent extends LogEvent{

    private final LoggedVar var;
    /**
     * Constructor.
     * @param var The LoggedVar to log on fire
     * @param time The fire time of this event
     **/
    LoggedVarEvent(LoggedVar var, long time) {
        super(time);
        this.var = var;
    }

    /** During fire, log that event **/
    protected void actionPerformed() { var.logEvent(time); }
}

