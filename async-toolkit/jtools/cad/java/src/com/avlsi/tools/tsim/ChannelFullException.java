/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

/**
 * <p> Indicates that the given output channel is full. </p>
 *
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @revision $Revision$
 * @since TSim version 2
 */

public class ChannelFullException extends RuntimeException
{
    /**
     * <p> The channel <code>out</code> is the full channel. </p>
     *
     * @param out the full channel.
     *
     * <pre><jml>
     * public normal_behavior
     *   requires out != null;
     * </jml></pre>
     */

    public ChannelFullException(final ChannelOutput out) {
        super(out.getName());
    }

} // end of class ChannelFullException

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
