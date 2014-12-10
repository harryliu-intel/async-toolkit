/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.fast;

import java.util.Collections;
import java.util.Map;

import com.avlsi.cell.CellInterface;
import com.avlsi.fast.BlockCommon;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.NetlistBlockFactory;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.cdl.parser.CanonicalizeCDL;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.netlist.AbstractNetlist;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.AliasedSet;

/**
 * Interface to a netlist block.
 **/
public class NetlistBlock extends BlockCommon {
    private AbstractNetlist netlist;
    private CellInterface parent;
    private Template templ = null;
    private Map params = Collections.EMPTY_MAP;
    private boolean empty;

    public NetlistBlock(final CellInterface parent) {
        this(null, parent);
        empty = true;
    }

    public NetlistBlock(final AbstractNetlist netlist,
                        final CellInterface parent) {
        setNetlist(netlist);
        this.parent = parent;
    }

    public String getType() {
        return BlockInterface.NETLIST;
    }

    /** Netlist block overrides. **/
    public void refineFrom(final BlockInterface o) {
        if (empty) {
            super.copyInternals(o);
            final NetlistBlock parent = (NetlistBlock) o;
            this.netlist = parent.netlist;
            this.templ = parent.templ;
            this.params = parent.params;
            this.empty = parent.empty;
        }
    }

    public BlockInterface merge(BlockInterface o) {
        Debug.assertTrue(false, "NetlistBlock doesn't support all BlockInterface functionality");
        return null;
    }

    public BlockInterface replace(BlockInterface o) {
        Debug.assertTrue(false, "NetlistBlock doesn't support all BlockInterface functionality");
        return null;
    }

    public void setCDLTemplate(final Template templ, final Map params) {
        this.empty = false;
        this.templ = templ;
        this.params = params;
    }

    public void setNetlist(final AbstractNetlist netlist) {
        this.empty = false;
        this.netlist = netlist;
    }

    public AbstractNetlist getNetlist() {
        if (netlist == null && templ != null) {
            /* If the template exist, then create the netlist from that, using
             * the default parameters, and cache it. */
            netlist = new NetlistBlockFactory();
            templ.execute(Collections.EMPTY_MAP, (CDLSimpleInterface) netlist);
        }
        return netlist;
    }

    public boolean isEmpty() {
        return empty;
    }

    public Template getCDLTemplate() {
        return templ;
    }

    public Template getCanonicalTemplate(final Cadencize c) {
        final CadenceInfo ci = c.convert(parent);
        final AliasedMap ports = ci.getPortNodes();
        final Template canonTempl = new Template(Collections.EMPTY_MAP);
        templ.execute(new CanonicalizeCDL(canonTempl, new AliasedSet(ports)));
        return canonTempl;
    }

    public Map getParams() {
        return params;
    }
}
