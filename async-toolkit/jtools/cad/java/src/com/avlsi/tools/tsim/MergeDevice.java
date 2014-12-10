/*
 * Copyright 2001, 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.tsim;

import java.math.BigInteger;
import java.util.Vector;

import com.avlsi.util.debug.Debug;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.tsim.AbstractDevice;
import com.avlsi.tools.tsim.ChannelInput;
import com.avlsi.tools.tsim.ChannelOutput;
import com.avlsi.tools.tsim.Message;

/**
 * @todo Undocumented.
 *
 * @author Jesse Rosenstock
 * @author Kim Wallmar
 * @author jlm
 * @author Abe Ankumah
 * @author Dan Daly
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Revision$ $Date$
 *
 * @review kiniry - Under review beginning 23 July 2002.
 **/

public class MergeDevice extends AbstractDevice {

    protected static Vector conflicts = new Vector();

    protected ChannelInput[] inputs;
    protected ChannelOutput Out;
    //protected boolean rolloverCheck = false;
    //protected int bitNum = 0;
    protected MergeConflict conflict;
    protected int transactionNum = 0;

    public MergeDevice(String name, ChannelInput[] inputs,
                        ChannelOutput Out, boolean suppressOutput) {
        super(name, suppressOutput);
        this.conflict = null;
        this.inputs = inputs;
        this.Out = Out;
    }

    public void go() throws InterruptedException {
        final boolean suppressOutput = isOutputSuppressed();
        final int numInputs = inputs.length;
        for (;;) {
            setState("Waiting for input");
            transactionNum++;

            Message m0 = null;
            for (int i = 0; i < numInputs; ++i) {
                Message m = inputs[i].receive(time);
                if (!suppressOutput)
                    output("Received message " + i + ": " + m);
                if (i == 0)
                    m0 = m;
                else {
                    if (!numsEqual(m0.getValue(), m.getValue())) {
                        final String msg =
                            "Panic:  Discrepancy in MergeDevice input #" +
                            transactionNum + ": " +
                            inputs[0].getName() + " was 0x" +
                            m0.getValue().toString(16) +
                            " vs. " +
                            inputs[i].getName() + " was 0x" +
                            m.getValue().toString(16) +
                            "\nTime was " +
                            m0.getTime() + " on " + inputs[0].getName() +
                            " vs. " +
                            m.getTime() + " on " + inputs[i].getName();

                        System.err.println(msg);
                        conflicts.add(this);
                        conflict = new MergeConflict(m0.getValue(),
                                                     m0.getTime(),
                                                     m.getValue(),
                                                     m.getTime());

                        if (DSim.get().haltOnConflict)
                            DSim.get().interrupt();
                    }

                    if (!suppressOutput) {
                        if (m0.getTime() != m.getTime()) {
                            output("Time discrepancy " + inputs[0].getName() +
                                   ":" + m0.getTime() +
                                   " vs. " + inputs[i].getName() +
                                   ":" + m.getTime());
                        } else
                            output("No time discrepancy");
                    }
                }


            }

            send(Out, m0);
        }
    }

    protected void cleanup() {
        for (int i = 0; i < inputs.length; ++i) {
            inputs[i].destroy();
            inputs[i] = null;
        }
        Out.destroy(); Out=null;
        super.cleanup();
    }

    /**
     * NodeReadChannel doesn't always know how which way it should
     * interpret the bits, so this takes care of rollover (-10 =
     * 4294967286 on a 32-bit channel) issues.
     **/
    protected boolean numsEqual(BigInteger v1, BigInteger v2) {
        // quicker check first
        if (v1.equals(v2))
            return true;

        final BigInteger m = Out.getNumPossibleValues();
        Debug.assertTrue(m.signum() == 1);

        return v1.mod(m).equals(v2.mod(m));
    }

    /** Stub.  Return Vector of Merges which have experienced conflicts **/
    public static Vector conflictingMerges() {
        return conflicts;
    }

    /** Stub.  Return this MergeDevice's first conflict. **/
    public MergeConflict firstConflict() {
        return conflict;
    }

    /** clear conflicts, removes all devices from conflict vector **/
    public static void clearConflicts(){
        conflicts.clear();
    }

} // end of class MergeDevice

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
