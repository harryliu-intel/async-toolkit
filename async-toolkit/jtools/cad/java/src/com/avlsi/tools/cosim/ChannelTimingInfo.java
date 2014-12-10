package com.avlsi.tools.cosim;

public interface ChannelTimingInfo {
    /**
     * Return slack as number of full buffers.
     **/
    int getSlack();

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
}
