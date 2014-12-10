/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
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
 * <p> Base class for <code>SlacklessNode*Channel</code>.  Slackless
 * <code>NodeChannels</code> don't have any time delay except for that imposed
 * by DSim. </p>
 *
 * @author Dan Daly
 * @version $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public abstract class SlacklessNodeChannel implements ChannelStatus {
    
    protected final Node [][][]  rail;
    protected final Node [][]    enable;
    protected final RailList   rlist;
    protected final int        M;
    protected final int        index;

    protected static final int NONE = 0, VALID = 1, NEUTRAL = 2, SELECT = 3,
                             DESTROYED = 4;

    protected Node resetNode;

    protected int sleep_status = NONE;

    protected boolean connect_status = false; //Not Connected
    
    protected WaiterInterface selector = null;

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

    /**
     * Class constructor.
     *
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
     **/
    public SlacklessNodeChannel(String name, RailList rlist, int M,
                                int index, boolean startNow) {
        connect_status = startNow;
        this.name = name;
        this.rlist = rlist;
        rlist.setNpowM(M);
        this.M = M;
        this.index = index;
        rail = new Node[rlist.getLength()][M][];
        enable = new Node[rlist.getLength()][M];
        if (startNow) instantiateNM();
        // review:  this doesn't look at all right to me -- jmr
        numPossibleValues = BigInteger.valueOf(rlist.getLength()*M);

        Debug.log("Warning: slackless node channels may have incorrect "
                  + "timing: (" + name + ")");
    }

    public BigInteger getNumPossibleValues() {
        return numPossibleValues;
    }

    /** These are slackless channels, so don't really have cycle times **/
    public long getCycleTime () {
        return 0;
    }
    
    /**
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
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
                    if (rail[k][i][j] == null) {
                        System.err.println("Can't find "+fullname+ "." + j);
                        throw new NoSuchNodeException(fullname+ "." + j);
                    }
                }
            }
        }
        instantiate();
    }
        
    protected abstract void instantiate();
    
    /**
     * @throws NoSuchNodeException  If a node in the channel could not be
     *   found.
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

        sleep_status = DESTROYED;
    }

    public int getCapacity() {
        return 0;
    }   

    public int getAvailable() {
        return 0;
    }   

    public String getVerboseStatus() {
        return "";
    }
} // end of abstract class SlacklessNodeChannel

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
