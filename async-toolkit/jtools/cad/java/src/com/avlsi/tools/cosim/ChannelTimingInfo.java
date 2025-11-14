// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.tools.cosim;

public interface ChannelTimingInfo {
    /**
     * Return slack as number of full buffers.
     **/
    int getSlack();

    /**
     * Return internal slack as number of full buffers.
     **/
    int getInternalSlack();

    /**
     * Return forward latency, in DSim units.
     **/
    int getLatency();

    /**
     * Return data valid to enable latency, in DSim units.
     **/
    int getDataValidEnableLatency();

    /**
     * Return data neutral to enable latency, in DSim units.
     **/
    int getDataNeutralEnableLatency();

    /**
     * Return enable to data latency, in DSim units.
     **/
    int getEnableDataLatency();

    /**
     * Return cycle time, in DSim units.
     **/
    int getCycleTime();

    /**
     * Return cycle in time, in DSim units.
     **/
    int getCycleTimeIn();

    /**
     * Return cycle out time, in DSim units.
     **/
    int getCycleTimeOut();

    /**
     * Return latency per slack, in DSim units.
     **/
    int getLatencyPerSlack();

    /**
     * Return the csp_time directive, in DSim units.
     **/
    default int getCspTime() {
        throw new UnsupportedOperationException();
    }
}
