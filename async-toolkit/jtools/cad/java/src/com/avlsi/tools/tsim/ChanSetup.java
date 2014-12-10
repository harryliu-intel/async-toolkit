/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.util.debug.Debug;

/**
 * <p> Class for passing parameters around for <code>NodeChannel</code>s.  Also
 * used by <code>CastObjectFactory</code> to generate the needed CAST file
 * syntax. </p>
 * 
 * @author Dan Daly
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 29 July 2002.
 **/

public class ChanSetup {

    //Direction vars in DSim, WRITE is for a NodeWriteChannel
    public static final boolean WRITE = true;
    //Read is for a NodeReadChannel
    public static final boolean READ = false;
    
    //For timing, all generated channels receive this value if not 
    //specified
    public static int defaultSTAGES = 8;
    
    private final RailList rails;
    private final int M;
    private ChannelIO[] hchans;
    private boolean direction;
    private int[] STAGES = null;
    private String basename = "naming_error";
    private String name_prefix = "";

    public ChanSetup(int N, int M, ChannelIO hcio,
                     boolean direction) {
        this(N, M, new ChannelIO[] {hcio},direction );
    }

    public ChanSetup(int N, int M, ChannelIO[] hchans,
                     boolean direction) {
        this(new RailList(N), M, hchans, direction);
    }   

    public ChanSetup(int N, ChannelIO hcio, boolean direction) {
        this(N, 1,new ChannelIO[] {hcio},direction);
    }

    public ChanSetup(int N, ChannelIO[] hchan, boolean direction) {
        this(N,1, hchan,direction);
    }

    public ChanSetup( RailList rails,  ChannelIO[] hchans,
                      boolean direction){
        this(rails, 1, hchans, direction);
    }

    public ChanSetup( RailList rails, int M, ChannelIO[] hchans,
                      boolean direction){ 
        this.rails = rails;
        this.M = M;
        this.hchans = hchans;
        this.direction = direction;
        Debug.assertTrue(getLength() > 0);
        guessBaseName(hchans[0].getName());
    }

    private void guessBaseName(String name) {
        basename = name;
        for(int i=0;i<name.length();i++) {
            if (name.charAt(i) == '[') {
                basename = name.substring(0,i);
            }
        }
        if (getLength() > 1) basename = "multiChanWith_"+basename;
    }
    
    public void setBaseName(String name) {
        this.basename = name;
    }

    public void setNamePrefix(String name_prefix) {
        this.name_prefix = name_prefix;
    }

    public NodeChannel[] getNodeChannels(boolean startNow) {
        //Add # of channels equal to the product of all the terms in the
        //dim list.  Be sure to create the correct ChannelType
        String name = name_prefix+basename;
        NodeChannel[] channels = new NodeChannel[getLength()];
        for (int loop=0;loop<getLength();loop++) {
            int index = loop;
            if (getLength() ==1) index = -1;
            if (direction == WRITE)
                channels[loop] =
                    new NodeWriteChannel(
                            name, getRails(),getM(),index, startNow);
            else channels[loop] =
                    new NodeReadChannel(
                            name, getRails(),getM(), index, startNow);
            
        }
        return channels;
    }

    public ChannelIO[] CopyChannels() {
        ChannelIO[] hcio = new ChannelIO[getLength()];
        for (int loop=0;loop<getLength();loop++) {
            int stage = defaultSTAGES;
            if ((STAGES != null) && (STAGES.length > loop)) 
                stage = STAGES[loop];
            String chan_name = "cosim_"+name_prefix+basename;
            if (getLength() > 1) chan_name += "["+loop+"]";
            hcio[loop] = new BufferedChannel(chan_name,stage);
        }
        return hcio;
    }              
   
    public StringBuffer declare() {
        StringBuffer out = new StringBuffer();
        for(int index = 0;index< rails.getLength();index++) {
            int n = rails.get(index);
            String intString = "";
            if (rails.getLength() > 1) intString = index+"";
            out.append("e1of"+n+" "+name_prefix+basename+intString);
            if (getLength() > 1) {
                out.append("[0.."+(getLength()-1));
                if (M > 1) {
                out.append(",0.."+(M-1)+"]");
                } else out.append("]");
            } else if (M > 1) out.append("[0.."+(M-1)+"]");
            out.append(";\n");
        }
        return out;
    }

   public String passParameter() {
       //Iterate on the number of rails in railList
       int numrails = rails.getLength();
       if (numrails == 1) return name_prefix+basename;
       String params = new String();
       for (int i=0;i<rails.getLength();i++) {
           params += name_prefix+basename+i;
           if (i < (rails.getLength()-1)) params += ",";
       }
       return params;
   }

    public int getLength() {
        if (hchans != null) return hchans.length;
        return -1;
    }

    public String getFullName() {
        return name_prefix+basename;
    }

    public int getM() { return M;}
    public RailList getRails() { return rails;}
  
    public ChannelIO[] getChannels() {
        return hchans;
    }

    public ChannelIO getChannelAt(int index) {
        if (index < getLength()) {
            return hchans[index];
        }
        return null;
    }

} // end of class ChanSetup

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
