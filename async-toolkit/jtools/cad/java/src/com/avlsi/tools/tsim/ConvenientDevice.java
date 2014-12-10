/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;

/**
 * <p> Subclass of <code>AbstractDevice</code> which handles arrays of channels
 * through convenience routines.  (This file was modified from
 * <code>com.avlsi.csp.csp2java.runtime.CspRuntimeAbstractDevice</code>.)
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/

public abstract class ConvenientDevice extends AbstractDevice {

    /**
     * Class constructor.
     **/
    public ConvenientDevice (String name, boolean suppressOutput) {
        super(name, suppressOutput);
    }

    /**
     * Uses AbstractDevice.send() to send a message on multiple channels of
     * a channel array.  This method takes a message of Java type 'int'.
     *
     * @param out Array of output channels
     * @param offset Index of least significant channel to use
     * @param length Number of channels to use
     * @param radix Number of symbols transmissible by each channel
     * @param message Message to send.
     **/
    protected void send(ChannelOutput[] out, int offset, int length, int radix,
            int message) throws InterruptedException {
        // Use the BigInteger send method listed below.
        send (out, offset, length, radix, BigInteger.valueOf(message));
    }

    /**
     * Uses AbstractDevice.send() to send a message on multiple channels of
     * a channel array.  This method takes a BigInteger message.
     *
     * @param out Array of output channels
     * @param offset Index of least significant channel to use
     * @param length Number of channels to use
     * @param radix Number of symbols transmissible by each channel
     * @param message Message to send.
     **/
    protected void send(ChannelOutput[] out, int offset, int length, int radix,
            BigInteger message) throws InterruptedException {

        for (int i = offset; i < offset + length; i++) {
            send(out[i], message.remainder(BigInteger.valueOf(radix)));
            message = message.divide(BigInteger.valueOf(radix));
        }
    }

    /**
     * Uses AbstractDevice.receive() to receive a message on multiple channels
     * of a channel array.
     *
     * @param in Array of input channels
     * @param offset Index of least significant channel to use
     * @param length Number of channels to use
     * @param radix Number of symbols transmissible by each channel
     **/
    protected Message receive(ChannelInput[] in, int offset, int length,
            int radix) throws InterruptedException {

        BigInteger messageValue = BigInteger.ZERO;
        long messageTime = 0;

        for (int i = offset + length - 1; i >= offset; i--) {
            Message m = receive(in[i]);
            messageValue = messageValue.multiply(
                    BigInteger.valueOf(radix)).add(m.getValue());
            messageTime = m.getTime();
        }

        return new Message (messageValue, messageTime);
    }

} // end of abstract class ConvenientDevice

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
