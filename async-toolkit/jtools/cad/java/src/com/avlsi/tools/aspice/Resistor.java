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

import com.avlsi.file.common.HierName;

/**
 * Class to represent resistors in .aspice files.
 *
 * @author Ted Vessenes
 * @version $Name:  $ $Date$
 **/
public final class Resistor extends AbstractDevice 
                            implements java.io.Serializable {
    /** Conductance of resistor in 1/ohms */
    private double conductance;
    /** Device currents **/
    
    /** Drain node **/
    private double drainI;
    /** Source node **/
    private double sourceI;

    /**
     * Class constructor.
     * @param source node
     * @param drain node
     * @param conductance of resistor
     **/
    public Resistor(final HierName name,
                    final Node source,
                    final Node drain,
                    final double conductance)
    {
        super(new Node[] { source, drain });
        this.conductance = conductance;
        this.name = name;
        //System.out.println("New Resistor s="+source.getName()+
        //                   " d="+drain.getName()+
        //                   " cond= "+conductance);
    }

    public Resistor(final Node source,
                    final Node drain,
                    final double conductance) {
        this(null, source, drain, conductance);
    }
    /**
     * Get source node.
     * @return source node
     **/
    public Node getSource() {
        return nodes[0];
    }

    /**
     * Get drain node.
     * @return drain node
     **/
    public Node getDrain() {
        return nodes[1];
    }

    /**
     * Get conductance of resistor.
     * @return conductance of resistor
     **/
    public double getConductance() {
        return conductance;
    }

    /**
     * Set conductance of resistor.
     **/
    public void setConductance(double conductance) {
        this.conductance = conductance;
    }

    /** returns the device current **/
    public double getCurrent(int type) {
        switch (type) {
          case AbstractDevice.I1:      return drainI;
          case AbstractDevice.I2:      return sourceI;
        }
        return -1;
    }

    public String getCode() { return "R";}
    /**
     * Evaluates current, charge, and their derivatives for all ports
     * on a device and then informs its nodes of these values.
     * @param chargeScale scalar for charge calculation
     * @param currentScale scalar for current calculation
     * @param derivChargeScale scalar for charge derivative calculation
     * @param derivCurrentScale scalar for current derivative calculation
     **/
    public void evalVoltage(double chargeScale, double currentScale,
                            double derivChargeScale, double derivCurrentScale,
                            double time)
    {
        Node source = getSource();
        Node drain = getDrain();

        double current = (drain.getVoltage() - source.getVoltage())
                         * conductance;

        drainI = -current;
        sourceI = current;

        source.setResult(chargeScale, 0, currentScale, current);
        drain.setResult(chargeScale, 0, currentScale, -current);

        source.setMatrix(source,
                         derivChargeScale, 0, derivCurrentScale, conductance);
        source.setMatrix(drain,
                         derivChargeScale, 0, derivCurrentScale, -conductance);
        drain.setMatrix(source,
                        derivChargeScale, 0, derivCurrentScale, -conductance);
        drain.setMatrix(drain,
                        derivChargeScale, 0, derivCurrentScale, conductance);
    }

}
