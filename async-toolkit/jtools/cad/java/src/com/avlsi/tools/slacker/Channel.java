package com.avlsi.tools.slacker;

import java.util.*;
import java.io.*;

public class Channel implements Comparable {

    /** Name */
    final String name;

    /** Parent */
    final String parent;

    /** is this a port? */
    boolean isPort = false;

    /** channel index to identify passthrus **/
    final int passthru;

    /** Cost per slack **/
    double cost = 0;
    
    /** Subcell (or aligned port) time indicies connected to this channel */
    int srcTimeIndex = -1;
    int dstTimeIndex = -1;
    
    /** Time offsets of this channel when used as an OutPort/InPort */
    double srcTimeOffset = 0;
    double dstTimeOffset = 0;

    /** Total time for debugging output **/
    double srcTime = 0;
    double dstTime = 0;

    /** Forward latency due to extra_delay on data rails of channel **/
    double latency = 0; 
    
    /** Initial tokens after reset for an outPort */
    int initialTokens = 0;

    /** Number of channel handshakes associated with this port */
    int handshakes = 0;

    /** Alignment group id */
    int alignment = -1;

    /** Ignore this slack constraint? */
    boolean ignore = false;

    /** Avoid adding buffers on this channel? */
    boolean dontTouch = false;

    /** Does this channel definitely need non-zero slack? */
    boolean nonzeroSlack = false;

    /** Number of buffers */
    double numbuf = 0;

    /** Slack of channel */
    double slack = 0;

    /** 
     * Negative slack to make all loops feasible at greatly increased
     * cost.  Only used when needsAntiSlack() is true.  Adds an
     * additional independent variable.
     */
    double antiSlack = 0;

    /** Extra free slack added for this port */
    double extraFreeSlack = 0;

    /** Total of all extraFreeSlack attached to this channel */
    double totalFreeSlack = 0;

    /** Index of numbuf variable */
    int numbufIndex = -1;
    
    /** Index of slack variable */
    int slackIndex = -1;

    /** Index of antiSlack variable */
    int antiSlackIndex = -1;

    /** Constructor */
    public Channel(String name,
                   String parent,
                   int passthru,
                   double time,
                   double extraFreeSlack,
                   double cost,
                   double latency,
                   int handshakes,
                   int initialTokens,
                   int alignment,
                   boolean ignore,
                   boolean isPort,
                   boolean dontTouch) {
        this.name = name;
        this.parent = parent;
        this.passthru = passthru;
        srcTimeOffset = dstTimeOffset = time;
        this.extraFreeSlack = extraFreeSlack;
        this.cost = cost;
        this.latency = latency;
        this.handshakes = handshakes;
        this.initialTokens = initialTokens;
        this.alignment = alignment;
        this.ignore = ignore;
        this.isPort = isPort;
        this.dontTouch = dontTouch;
    }

    /** Only allow antiSlack on channels which have initialTokens */
    public boolean allowAntiSlack() {
        return (initialTokens>0);
    }

    /** Compare Channels by name */
    public int compareTo (Object b) {
        return name.compareTo(((Channel) b).name);
    }
    
    public boolean equals(Object b) {
        if (b instanceof Channel) {
            return name.equals(((Channel) b).name);
        } else {
            return false;
        }
    }

    public int hashCode() {
        return name.hashCode();
    }

    /** Debugging string */
    public String toString() {
        String s = "(" + (isPort ? "port " : "channel ") + name + 
            (parent!=null ? " parent=" + parent : "") +
            (numbuf!=0 ? " numbuf=" + numbuf : "") +
            (ignore ? " ignore=" + ignore : "") +
            (dontTouch ? " dontTouch=" + dontTouch : "") +
            (cost!=0 ? " cost=" + cost : "") +
            (handshakes!=0 ? " handshakes=" + handshakes : "") +
            (extraFreeSlack!=0 ? " extraFreeSlack=" + extraFreeSlack : "") +
            (totalFreeSlack!=0 ? " totalFreeSlack=" + totalFreeSlack : "") +
            (alignment>=0 ? " alignment=" + alignment : "") +
            (passthru>=0 ? " passthru=" + passthru : "") +
            (initialTokens!=0 ? " initialTokens=" + initialTokens : "") +
            (srcTimeIndex>=0 ? "" : " dstTimeOffset=" + dstTimeOffset) +
            (dstTimeIndex>=0 ? "" : " srcTimeOffset=" + srcTimeOffset) +
            " srcTime=" + srcTime + " dstTime=" + dstTime +
            ")";
        return s;
    }
}
