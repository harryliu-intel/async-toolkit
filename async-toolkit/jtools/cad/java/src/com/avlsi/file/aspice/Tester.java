/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.file.aspice;

import com.avlsi.file.common.DeviceTypes;

/**
 * Class that runs device tests
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/

//GGG I heard that this should extend something...
public class Tester {
    public static void main (String[] args) {
        System.out.println("Reading model file");
        BSim3Model.readFile("/usr/local/cad/lib/bsim3/tsmc18.bsim3", 27);

        System.out.println("Starting Device tests");

        new ResistorTest("Source", "Drain", 1).test();
        new CapacitorTest("Source", "Drain", 1).test();

        double length = 1.8e-6;
        double width = 5 * length;
        new TransistorTest(DeviceTypes.N_TYPE,
                           "Source", "Drain", "Gate", "Bulk",
                           width, length).test();

        new DiodeTest(DeviceTypes.N_TYPE,
                      "Source", "Drain",
                      width, length, width*length, 2*(width+length)).test();
    }
}
