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

import java.util.HashMap;

import com.avlsi.file.common.DeviceTypes;
import com.avlsi.test.AbstractTestCase;

import com.avlsi.util.recalc.IndependantVariable;
import com.avlsi.util.recalc.MinusOp;
import com.avlsi.util.recalc.DivOp;
import com.avlsi.util.recalc.Literal;

/**
 * Class that tests circuit simulator
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public class TestCircuit extends AbstractTestCase {
//GGG Comment this class
    /** Length of NMOS Transistors. **/
    final double nLength = 0.18e-6;

    /** Width of NMOS Transistors. **/
    final double nWidth  = 5*nLength;

    /** Length of PMOS Transistors. **/
    final double pLength = 0.18e-6;

    /** Width of PMOS Transistors. **/
    final double pWidth  = 10*pLength;

    /** Ground voltage. **/
    final double low = 0;

    /** VDD Voltage. **/
    final double high = 1.8;

    /** Run all tests. **/
    public void test() throws Exception {
        // Case 1: Load BSim3 models
        // Note that all transistor and diode tests will fail if we don't
        // run this test
        testBsim3Models(false);

        // Case 2: Test Transistors
        testTransistor(false, DeviceTypes.N_TYPE);
        testTransistor(false, DeviceTypes.P_TYPE);

        // Case 3: Capacitance charging
        testCharging(false);

        // Case 4: Oscillator Ring
        testOscillatorRing(false);
    }

    /**
     * Test that BSim3 Models load.
     * This test implicitly loads the BSim3 model file that future tests use.
     * @param output true if test should print output
     **/
    public void testBsim3Models(boolean output) throws Exception {
        BSim3Model.readFile("/usr/local/cad/lib/bsim3/tsmc18.bsim3", 27);
        try {
            BSim3Model.findModel(DeviceTypes.N_TYPE, nWidth, nLength);
            BSim3Model.findModel(DeviceTypes.P_TYPE, pWidth, pLength);
        } catch (IllegalArgumentException e) {
            assertTrue(false);
        }
    }

    /**
     * Test that Transistors simulate correctly
     * @param output true if test should print output
     * @param type of transistor
     **/
    public void testTransistor(boolean output, final int type)
        throws Exception
    {
        // Case 2: Transistor Evaluation
        final double low;
        final double high;
        final double step;
        final double width;
        final double length;

        if (type == DeviceTypes.N_TYPE) {
            low = this.low;
            high = this.high;
            step = .01;
            width = nWidth;
            length = nLength;
        } else {
            low = this.high;
            high = this.low;
            step = -.01;
            width = pWidth;
            length = pLength;
        }

        Node source = new Node(low);
        Node drain  = new Node(low);
        Node gate   = new Node(low);
        Node bulk   = new Node(low);

        Transistor trans = new Transistor(source, drain, gate, bulk,
                                          type, width, length);

        for (double gateVolt = low; Math.abs(gateVolt - high) > 1e-6;
             gateVolt += 10*step)
        {
            gate.setVoltage(gateVolt);

            for (double drainVolt = low; Math.abs(drainVolt - high) > 1e-6;
                 drainVolt += step)
            {
                drain.setVoltage(drainVolt);

                source.initialize();
                drain.initialize();
                gate.initialize();
                bulk.initialize();

                trans.evalVoltage(0, 1, 0, 0,0); // Only track Current
//GGG We should do number compares here instead of printing to file

                if (output)
                    System.out.println(drainVolt + " " + source.getResult());
            }

            if (output)
                System.out.println();
        }
    }

    /**
     * Test a simple capacitor charging circuit
     * @param output true if test should print output
     **/
    public void testCharging(boolean output) throws Exception {
        final double timestep = 1e-10; 
        final double conductance = 1e-3;
        final double capacitance = 1e-9;
        final double endTime = 5*(capacitance / conductance);

        Circuit circuit = new Circuit(timestep);
        GroundNode ground = new GroundNode();
        Node vdd = new Node(high);
        Node x = new Node();

        circuit.addDevice(new Capacitor(vdd, ground, 1));
        circuit.addDevice(new Capacitor(ground, x, capacitance));
        circuit.addDevice(new Resistor(vdd, x, conductance));

        circuit.initializeSimulation();
        vdd.setVoltage(high);

        double voltage = x.getVoltage();
        double time = circuit.getTime();
        if (output)
            System.out.println(time + " " + voltage);

        while (time < endTime) {
            circuit.jump(1e-8);

            time = circuit.getTime();
            voltage = x.getVoltage();
//GGG Compare values to expected values...

            if (output)
                System.out.println(time + " " + voltage);
        }
    }

    /**
     * Test a simple transistor oscillation ring
     * @param output true if test should print output
     **/
    public void testOscillatorRing(boolean output) throws Exception {
        final double timestep = 1e-12;
        final double endTime = 1e-9;
        final int ringSize = 5; // Must be odd

        Circuit circuit = new Circuit(timestep);
        GroundNode ground = new GroundNode();
        Node vdd = new Node(high);
        Node firstNode = new Node(); // Save to close loop
        Node linkNode = firstNode; // Update as we create new nodes

        for (int i = 0; i < ringSize; i++) {
            Node sourceDrain;
            if (i == ringSize - 1)
                sourceDrain = firstNode;
            else
                sourceDrain = new Node();

            circuit.addDevice(new Transistor(sourceDrain, ground,
                                             linkNode, ground,
                                             DeviceTypes.N_TYPE,
                                             nWidth, nLength));
            circuit.addDevice(new Transistor(sourceDrain, vdd,
                                             linkNode, vdd,
                                             DeviceTypes.P_TYPE,
                                             pWidth, pLength));

            linkNode = sourceDrain; // Old source/drain feed into new linkNode
        }

        // Cheat on the power supply
        IndependantVariable x = new IndependantVariable(high);
        DivOp exp = new DivOp(new MinusOp(new Literal(high), x),
                                            new Literal(1e-10));
        HashMap voltageMap = new HashMap();
        voltageMap.put(vdd, x);

        circuit.addDevice(new CurrentSource(vdd, exp, voltageMap));

        double time = circuit.getTime();
        double voltage = firstNode.getVoltage();
        if (output)
            System.out.println(time + " " + voltage);

        do {
            circuit.jump(1e-11);

            time = circuit.getTime();
            voltage = firstNode.getVoltage();
//GGG Again, we should test these values, not print them to file.

            if (output)
                System.out.println(time + " " + voltage);
        } while (time < endTime && (low-1) <= voltage && voltage <= (high+1));
    }

    /**
     * Run all circuit test cases
     * @param args Command line arguments
     **/
    public static void main (String[] args) {
        AbstractTestCase.testOne(new TestCircuit());
    }
}
