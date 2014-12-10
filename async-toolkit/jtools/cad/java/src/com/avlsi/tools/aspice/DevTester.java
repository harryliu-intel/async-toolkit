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

package com.avlsi.tools.aspice;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.util.recalc.IndependantVariable;
import com.avlsi.util.recalc.MinusOp;
import com.avlsi.util.recalc.DivOp;
import com.avlsi.util.recalc.MultOp;
import com.avlsi.util.recalc.Literal;
import java.util.HashMap;

/**
 * Class for ...
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class DevTester {
    /**
     * Constructor.
     **/
    DevTester() {
        
    }

    public static void main(String[] args) {
        //TraceFile tracer = new TraceFile("devtest");
        //tracer.startNewTrace();
        BSim3Model.readFile("/usr/local/cad/lib/bsim3/tsmc18.bsim3",27);
        Circuit circuit = new Circuit(1e-13d);
        GroundNode gnd = new GroundNode(HierName.makeHierName("GND!"));
        Node volt = new Node(HierName.makeHierName("volt"));
        Diode d = new Diode(DeviceTypes.N_TYPE,
                            volt,
                            gnd,
                            0.9e-06d, 0.18e-06d, //width, length
                            0.432e-12d, 2.76e-06d //area source, area perim
                            );
        circuit.addDevice(d);
        IndependantVariable x = new IndependantVariable(0);
        HashMap voltageMap = new HashMap();
        voltageMap.put(volt, x);
        circuit.addDevice(
                new com.avlsi.tools.aspice.CurrentSource(volt, 
                                                         x, voltageMap));
        double time = circuit.getTime();
        while ( time < 1e-9) {
            circuit.jump(1e-12);
            time = circuit.getTime();
        }
    }
}

