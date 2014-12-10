/*
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.NodeWatcher;

import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DSimUtil;
import java.math.BigInteger;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.text.StringUtil;

/**
 * @todo Undocumented.
 *
 * @author Dan Daly
 * @version $Revision$ $Date$
 **/

public abstract class BufferedNodeChannel implements Statusable, ChannelStatus {
    
    protected final Node [][][]  rail;
    protected final Node [][]    enable;
    protected final RailList   rlist;
    protected final int        M;
    protected final int        index;
    protected final int        fbNeutral, fbValid;

    protected static final int BEGIN = 0, VALID = 1, NEUTRAL = 2,
                             DESTROYED = 3;
    /**
     * Indicates either that the channel has just begun or has ended,
     * or that it's waiting for some rails to be valid (VALID) or
     * neutral (NEUTRAL).
     **/
    protected int cycle_status = BEGIN;

    protected Node resetNode;

    protected boolean connect_status = false; //Not Connected
    
    protected String name;

    /**
     * The number of different values this channel can transmit.  If
     * it's an even power of 2, it's equal to 2^bitwidth.  If the
     * channel can transmit any arbitrary BigInteger, this value is
     * -1.  This isn't used in any way yet.  Messages are truncated by
     * the dsim side of the channel, silently.
     **/
    private final BigInteger numPossibleValues;

    protected NodeWatcher[][] enableNodeWatchers = null;
    protected NodeWatcher resetNodeWatcher = null;
    // XXX: what about watches on data rails?

    /** Internal channel **/
    protected final BufferedChannel bc;
    /** If the internal channel isn't able to handle the waiting dsim state **/
    protected boolean waitingOnChannel = false;
    /** The device the channel end connects to **/
    protected AbstractDevice owner = null;

    /**
     * @throws IllegalSlackException  If slack is nonpositive.
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public BufferedNodeChannel(int slack, String name, RailList rlist, int M,
                               int index, boolean startNow) {
        this(slack, 1, 1, 1, 1, 4, 4, name, rlist, M, index, startNow);
    }

    public BufferedNodeChannel(int slack, int ffLatency, int bbLatency,
                               int fbLatency, int bfLatency, int cycleTimeIn,
                               int cycleTimeOut, String name, RailList rlist,
                               int M, int index, boolean startNow) {
        this(slack, ffLatency, bbLatency, fbLatency, fbLatency, bfLatency,
             cycleTimeIn, cycleTimeOut, name, rlist, M, index, startNow);
    }
    

    public BufferedNodeChannel(int slack, int ffLatency, int bbLatency,
                               int fbNeutral, int fbValid, int bfLatency,
                               int cycleTimeIn, int cycleTimeOut, String name,
                               RailList rlist, int M, int index,
                               boolean startNow) {
         connect_status = startNow;
         this.name = name;
         this.rlist = rlist;
         rlist.setNpowM(M);
         this.M = M;
         this.index = index;
         this.fbNeutral = fbNeutral;
         this.fbValid = fbValid;
         rail = new Node[rlist.getLength()][M][];
         enable = new Node[rlist.getLength()][M];
         if (startNow) instantiateNM();
         numPossibleValues = rlist.getBigNpowM(0);
         int fbLatency = (fbNeutral + fbValid) / 2;
         bc = new BufferedChannel(slack,
                                  ffLatency, bbLatency, fbLatency, bfLatency,
                                  cycleTimeIn, cycleTimeOut,
                                  0,  // debug
                                  name + " internal channel",
                                  numPossibleValues);
    }
    
    /**
     * @throws NoSuchNodeException
     **/
    protected void instantiateNM() {
        resetNode = DSimUtil.getResetNode();
        for (int k=0; k < rlist.getLength();k++) {
            for (int i = 0; i < M; i++) {
                String fullname =
                    StringUtil.replaceSubstring(name+printIndex(k,i),
                            "][", ",");
                enable[k][i] = DSim.get().findOrAddNode(/*name + ind+*/
                                                fullname+".e");
                if (enable[k][i] == null) {
                    System.err.println("Can't find " +
                        fullname+/*name + ind*/ ".e");
                    throw new NoSuchNodeException(fullname + ".e");
                }
                rail[k][i] = new Node[rlist.get(k)];
                for (int j = 0; j < rlist.get(k); j++) {
                    rail[k][i][j] = DSim.get().findNode(fullname+"." + j);
                    // The following block supports e1of(n) channels.
                    if (rail[k][i][j] == null) {
                        rail[k][i][j] =
                            DSim.get().findNode(fullname+".d[" + j + "]");
                    }
                    // hack for e1of1, which doesn't have .d[0] or .0,
                    // only .d
                    if (rail[k][i][j] == null && j == 0) {
                        rail[k][i][j] =
                            DSim.get().findNode(fullname+".d");
                    }
                    if (rail[k][i][j] == null) {
                        System.err.println("Can't find "+fullname+ "." + j);
                        throw new NoSuchNodeException(fullname + "." + j);
                    }
                }
            }
        }
        instantiate();
    }
        
    protected abstract void instantiate();
    
    /**
     * @throws NoSuchNodeException
     **/
    public void bind() {
        if (!connect_status) {
            connect_status = true;
            instantiateNM();
        }
    }

    public String getName() {
        if (index >=0 ) return name+"["+index+"]";
        return name;
    }

    public String printIndex(int j, int i) {
        String ind = "";
        if (M >1) {
            ind += i + "]";
            if (index >=0) ind = "["+index+","+ind;
            else ind = "["+ind;
        } else if (index >=0) {
            ind = "["+index+"]";
        }
        if (rlist.getLength() > 1)
            ind = j+ind;
        return ind;
    }
    
    public void setName(final String newName) {
        name = newName;
    }

    public BigInteger getNumPossibleValues() {
        return numPossibleValues;
    }

    public long getCycleTime() {
        return bc.getCycleTime();
    }

    /*
     * Used to get rid of this channel.
     *
     * Stops watching nodes, changes state to invalid, etc.
     *
     * Should only be used when resetting the associated circuit.
     */
    public synchronized void destroy() {
        if (enableNodeWatchers != null) {
            for (int j=0;j< rlist.getLength();j++) {
                for (int i = 0; i < M; i++) {
                    if (enableNodeWatchers[j][i] != null)
                        enable[j][i].removeWatch(enableNodeWatchers[j][i]);
                }
            }
        }

        if (resetNodeWatcher != null)
            resetNode.removeWatch(resetNodeWatcher);

        // XXX: what about watches on data rails?

        cycle_status = DESTROYED;
    }

    public void printStatus() {
        bc.printStatus();
    }

    public void setOwner(final AbstractDevice owner) {
        this.owner = owner;
    }

    protected void setEnablingNode(final Node node) {
        if (owner != null) owner.setEnablingNode(node);
    }

    protected Node getEnablingNode() {
        return owner == null ? null : owner.getEnablingNode();
    }

    public int getCapacity() {
        return bc.getCapacity();
    }   

    public int getAvailable() {
        return bc.getAvailable();
    }   

    public String getVerboseStatus() {
        return bc.getVerboseStatus();
    }

    public void incCount() {
        bc.incCount();
    }

    public int getCount() {
        return bc.getCount();
    }
} // end of abstract class BufferedNodeChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
