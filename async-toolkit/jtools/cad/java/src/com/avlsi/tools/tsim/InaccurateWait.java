/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import com.avlsi.tools.dsim.Node;

/**
 * <p> Subclass of <code>Wait</code> that does the same thing, but returns the
 * events in a random order.  Note that the regular <code>Wait</code> already
 * sort of does this, since its order depends on how Java schedules threads, but
 * this makes it more explicitly random. </p>
 *
 * <p> See <code>AccurateWait</code> for additional comments on using
 * <code>Wait</code> properly. </p>
 *
 * @author Patrick Pelletier
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 **/

public class InaccurateWait extends AccurateWait {

    public InaccurateWait(ChannelInput [] in, ChannelOutput [] out,
                          Node [] up, Node [] down) {
	super(in, out, up, down);
    }

    public InaccurateWait(ChannelInput [] in, ChannelOutput [] out) {
        this(in, out, null, null);
    }

    protected boolean wantRandomEvents() {
        return true;
    }

} // end of class InaccurateWait

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
