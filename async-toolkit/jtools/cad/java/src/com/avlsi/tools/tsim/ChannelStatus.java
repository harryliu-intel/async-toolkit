package com.avlsi.tools.tsim;

public interface ChannelStatus {
    /**
     * Return the total capacity of this channel.
     **/
    int getCapacity();

    /**
     * Return the current availability of this channel.
     **/
    int getAvailable();

    /**
     * Get name of this channel.
     **/
    String getName();

    /**
     * Get verbose status of this channel.
     **/
    String getVerboseStatus();
}
