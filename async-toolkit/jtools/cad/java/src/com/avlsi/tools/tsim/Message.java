/*
 * Copyright 2000, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;

import com.avlsi.tools.sigscan.Transaction;

/**
 * <p> Class to represent a message on a channel. </p>
 *
 * @modify IMMUTABLE
 *
 * @author Aaron Denney
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 *
 * @bug Currently, tsim.Messages are immutable.  This means that every message
 * send creates a new instance that must eventually be garbage collected.  The
 * number of messages that are created and destroyed in a moderate TSim run
 * easily number in the millions.  If we can find a way to reuse messages this
 * would help out with both kinds of complexity issues.
 * @see <a href="http://internal/bugzilla/show_bug.cgi?id=1182">Bug#1182</a>
 **/

public class Message 
{
    // Attributes

    /**
     * Default values for the <var>line</var> and <var>instruction</var>
     * attributes.  
     * 
     * @review kiniry - Strange that they are not used!
     **/

    private static final int defaultLine = -1, defaultInstruction = -1;

    /**
     * The payload of the message.  For booleans, we just use the values 0 and
     * 1.  Every message must have a payload.
     * 
     * <pre><jml>
     * private invariant payload != null;
     * </jml></pre>
     **/

    private BigInteger payload;

    /** 
     * The local, relativistic, non-negative time of this message. 
     *
     * <pre><jml>
     * private invariant time >= 0;
     * </jml></pre>
     **/

    private long time = 0;

    /** 
     * Byte address of line from which this message originated.
     *
     * @rewiew kiniry - What does this mean?
     **/

    private final int line;

    /** 
     * Number within line. 
     *
     * @rewiew kiniry - What does this mean?
     **/

    private final int instruction;

    /** 
     * For linking transactions 
     *
     * @rewiew kiniry - What does this mean?
     **/

    private Transaction sender;


    // Constructors

    /**
     * Create a new message containing an integer value.
     *
     * @param payload the integer payload of this message.
     *
     * <pre><jml>
     * normal_behavior
     *   ensures intValue() == payload;
     * </jml></pre>
     **/

    public Message(int payload) {
        this(BigInteger.valueOf(payload), defaultLine, defaultInstruction);
    }

    /**
     * Create a new message containing a long value.
     *
     * @param payload the long payload of this message.
     *
     * <pre><jml>
     * normal_behavior
     *   ensures longValue() == payload;
     * </jml></pre>
     **/

    public Message(long payload) {
        this(BigInteger.valueOf(payload), defaultLine, defaultInstruction);
    }

    /**
     * Create a new message containing a boolean value.
     *
     * @param payload the boolean payload of this message.
     *
     * <pre><jml>
     * normal_behavior
     *   ensures booleanValue() == payload;
     * </jml></pre>
     **/

    public Message(boolean payload) {
        this(BigInteger.valueOf(payload ? 1:0), defaultLine, defaultInstruction);
    }

    /**
     * Create a new message containing an integer of arbitrary size.
     *
     * @param payload the <code>BigInteger</code> payload of this message.
     *
     * <pre><jml>
     * normal_behavior
     *   ensures getValue() == payload;
     * </jml></pre>
     **/

    public Message(BigInteger payload) {
        this(payload, defaultLine, defaultInstruction);
    }

    /**
     * Create a new message containing an integer of arbitrary size on a
     * specific line.
     *
     * @review kiniry - Make consistent.
     *
     * @param payload the <code>BigInteger</code> payload of this message.
     * @param line unknown.
     * @param instruction unknown.
     *
     * <pre><jml>
     * normal_behavior
     *   ensures getValue() == payload;
     *   ensures getLine() == line;
     *   ensures getInstruction() == instruction;
     * </jml></pre>
     **/

    public Message(BigInteger payload, int line, int instruction) { 
        this(payload, line, instruction, null); 
    }
    
    /**
     * Create a new message containing an integer of arbitrary size on a
     * specific line associated with a particular SignalScan transaction.
     *
     * @param payload the <code>BigInteger</code> payload of this message.
     * @param line unknown.
     * @param instruction unknown.
     * @param sender unknown.
     *
     * <pre><jml>
     * normal_behavior
     *   ensures getValue() == payload;
     *   ensures getLine() == line;
     *   ensures getInstruction() == instruction;
     *   ensures getSender() == sender;
     * </jml></pre>
     **/

    public Message(BigInteger payload,
                   int line, int instruction,
                   Transaction sender) {
        this.payload = payload;
        this.line = line;
        this.instruction = instruction;
        this.sender = sender;
    }

    // Public Methods

    /**
     * @return the local, relativistic time of this message.
     *
     * <pre><jml>
     * normal_behavior
     *   ensures \result >= 0;
     * </jml></pre>
     **/

    public /*@ pure @*/ long getTime() {
        return time;
    }

    /**
     * @return the byte address of line instruction started, or -1 if it was not
     * specified on construction.
     **/

    public /*@ pure @*/ int getLine() {
        return line;
    }

    /**
     * @return the instruction number within line.
     **/

    public /*@ pure @*/ int getInstruction() {
        return instruction;
    }

    /**
     * @return the payload of this message as a boolean.
     **/

    public /*@ pure @*/ boolean booleanValue() {
        long x = payload.longValue();
        if (x == 0) {
            return false;
        }
        return true;
    }
    
    /**
     * @return the payload of this message as an integer.
     **/

    public /*@ pure @*/ int intValue() {
        return payload.intValue();
    }

    /**
     * @return the payload of this message as a long (a 64 bit integer).
     **/

    public /*@ pure @*/ long longValue() {
        return payload.longValue();
    }

    /**
     * @return the payload of this message as a <code>BigInteger</code>.
     **/

    public /*@ pure @*/ BigInteger getValue() {
        return payload;
    }

    // Inherited from Object.

    public /*@ pure @*/ String toString() {
        // @note The parameter of BigInteger.toString(int) is a radix.
        return "0x" + payload.toString(16) + "@" + time;
    }

    /**
     * Works like <code>AbstractDevice.updateTime()</code>, but on a
     * <code>Message</code>.  Since <code>Message</code>s are immutable it returns
     * a new "updated" <code>Message</code>.
     *
     * @param time the time of the new "updated" message.
     * @ensures \result != this;
     * @ensures \result.getTime() == time;
     **/

    public Message updateMessageTime(long time) {
        Message result = null;

        if (time > this.getTime()) {
            result = this.clone(time);
        } else
            result = this;

        return result;
    }

    /**
     * @return the sender <code>Transaction</code>.
     **/

    public /*@ pure @*/ Transaction getSender() { return sender; }

    /**
     * @return a clone of this message with a new time.
     * @param time the new time for the message.
     *
     * <pre><jml>
     * normal_behavior
     *   requires time >= 0;
     *   ensures getTime() == time;
     * </jml></pre>
     **/

    public /*@ pure @*/ Message clone(long time) {
        Message result =
            new Message(this.payload, time, this.line, this.instruction, this.sender);

        return result;
    }

    /**
     * @return a clone of this message with a new value.
     * @param value the new value for the message.
     **/

    public /*@ pure @*/ Message clone(BigInteger value) {
        final Message result =
            new Message(value, this.time, this.line, this.instruction, this.sender);

        return result;
    }


    // Package Methods

    /**
     * Create a new message containing an integer value at a specific time.
     *
     * @param payload the integer payload of this message.
     * @param time the time at which this message was created.
     *
     * <pre><jml>
     * normal_behavior
     *   requires time >= 0;
     *   ensures getTime() == time;
     *   ensures intValue() == payload;
     * </jml></pre>
     **/

    Message(int payload, long time) {
        this(BigInteger.valueOf(payload), time, defaultLine, defaultInstruction);
    }

    /**
     * Create a new message containing a long value at a specific time.
     *
     * @param payload the long payload of this message.
     * @param time the time at which this message was created.
     *
     * <pre><jml>
     * normal_behavior
     *   requires time >= 0;
     *   ensures getTime() == time;
     *   ensures longValue() == payload;
     * </jml></pre>
     **/

    Message(long payload, long time) {
        this(BigInteger.valueOf(payload), time, defaultLine, defaultInstruction);
    }

    /**
     * Create a new message containing a boolean value at a specific time.
     *
     * @param payload the boolean payload of this message.
     * @param time the time at which this message was created.
     *
     * <pre><jml>
     * normal_behavior
     *   requires time >= 0;
     *   ensures getTime() == time;
     *   ensures booleanValue() == payload;
     * </jml></pre>
     **/

    Message(boolean payload, long time) {
        this(BigInteger.valueOf(payload ? 1:0), time, defaultLine, defaultInstruction);
    }

    /**
     * Create a new message containing an integer of arbitrary size at a
     * specific time.
     *
     * @param payload the <code>BigInteger</code> payload of this message.
     * @param time the time at which this message was created.
     *
     * <pre><jml>
     * normal_behavior
     *   requires time >= 0;
     *   ensures getTime() == time;
     *   ensures getValue() == payload;
     * </jml></pre>
     **/

    Message(BigInteger payload, long time) {
        this(payload, time, defaultLine, defaultInstruction);
    }

    /**
     * Create a new message containing an integer of arbitrary size on a
     * specific line at a specific time.
     *
     * @review kiniry - Make consistent.
     *
     * @param payload the <code>BigInteger</code> payload of this message.
     * @param time the time at which this message was created.
     * @param line unknown.
     * @param instruction unknown.
     *
     * <pre><jml>
     * normal_behavior
     *   requires time >= 0;
     *   ensures getTime() == time;
     *   ensures getValue() == payload;
     *   ensures getLine() == line;
     *   ensures getInstruction() == instruction;
     * </jml></pre>
     **/

    Message(BigInteger payload, long time, int line, int instruction) { 
        this(payload, time, line, instruction, null); 
    }
    
    /**
     * Create a new message containing an integer of arbitrary size on a
     * specific line associated with a particular SignalScan transaction at a
     * specific time.
     *
     * @param payload the <code>BigInteger</code> payload of this message.
     * @param time the time at which this message was created.
     * @param line unknown.
     * @param instruction unknown.
     * @param sender unknown.
     *
     * <pre><jml>
     * normal_behavior
     *   requires time >= 0;
     *   ensures getTime() == time;
     *   ensures getValue() == payload;
     *   ensures getLine() == line;
     *   ensures getInstruction() == instruction;
     *   ensures getSender() == sender;
     * </jml></pre>
     **/

    Message(BigInteger payload, long time,
                   int line, int instruction,
                   Transaction sender) {
        this.payload = payload;
        this.time = time;
        this.line = line;
        this.instruction = instruction;
        this.sender = sender;
    }

    /**
     * Set the time of this message.
     *
     * @param time the new time for the message.
     * @design Note the visibility of this method so that only TSim classes can
     * modify a message's time.
     *
     * <pre><jml>
     * normal_behavior
     *   requires time >= 0;
     *   ensures getTime() == time;
     *   assignable time;
     * </jml></pre>
     **/

    void setTime(long time) {
        this.time = time;
    }

    /**
     * Set the transaction of this message to 'sender'.
     *
     * @param sender the new transaction for this message.
     *
     * <pre><jml>
     * normal_behavior
     *   ensures getSender() == sender;
     *   assignable sender;
     * </jml></pre>
     **/

    void setSender(Transaction sender) {
        this.sender = sender;
    }

    /**
     * Set the value of this message to 'value'.
     *
     * @param value the new payload for this message.
     *
     * <pre><jml>
     * normal_behavior
     *   ensures getValue() == value;
     * also
     * private normal_behavior
     *   assignable payload;
     * </jml></pre>
     **/

    void setValue(BigInteger value) {
        this.payload = value;
    }

} // end of class Message

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
