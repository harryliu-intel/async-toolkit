/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.tsim.verilog;

import com.avlsi.tools.dsim.Node;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.tsim.NodeWriteChannel;
import com.avlsi.tools.tsim.NodeReadChannel;
import com.avlsi.tools.sigscan.Sigscan;
import com.avlsi.tools.sigscan.DebugOpts;
/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class VerilogUtil {
    /**
     * This class should not be instantiated.
     **/
    private VerilogUtil() { }

    public static NodeWriteChannel connectNodeWriteChannel(
            String verilogScope, String name, 
            Sigscan sigscan, DebugOpts opts, int N) {
        String fullname = verilogScope+"."+name;
        for (int loop=0;loop<N;loop++) {
            Node utilNode = DSim.get().findOrAddNode(fullname+"."+loop);
            VerilogSharedBus vbus =
                new VerilogSharedBus(verilogScope,name+loop,
                                 sigscan, opts, N);            
            vbus.drivenByNode(utilNode, 0);
        }
        Node enode = 
            DSim.get().findOrAddNode(fullname+".e");
        VerilogSharedBus ebus = 
            new VerilogSharedBus(verilogScope,name+"e",
                             sigscan, opts, 1);
        ebus.driveNode(enode, 0);
        return new NodeWriteChannel(2, fullname, N,1);
    }

    public static NodeReadChannel connectNodeReadChannel(
            String verilogScope, String name, 
            Sigscan sigscan, DebugOpts opts, int N) {
        String fullname = verilogScope+"."+name;
        for (int loop=0;loop<N;loop++) {
            VerilogSharedBus vbus =
                new VerilogSharedBus(verilogScope,name+loop,
                                 sigscan, opts, N);            
            Node utilNode = DSim.get().findOrAddNode(fullname+"."+loop);
            vbus.driveNode(utilNode, 0);
        }
        Node enode = 
            DSim.get().findOrAddNode(fullname+".e");
        VerilogSharedBus ebus = 
            new VerilogSharedBus(verilogScope,name+"e",
                             sigscan, opts, 1);
        ebus.drivenByNode(enode, 0);
        return new NodeReadChannel(2, fullname, N,1);
    }

        
}

