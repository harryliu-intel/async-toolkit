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

package com.avlsi.circuit;
import com.avlsi.file.common.HierName;
/**
 * Class for Diodes used by AbstractCircuit and its subclasses 
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class Diode implements DiodeInterface{

    /** name of device (optional) **/
    protected final HierName name;
    
    /** Names of connecting nodes **/
    protected final HierName source, drain;

    /** Type of the Diode **/
    protected final int type;

    /** Width of the diode **/
    protected final double width;

    /** Length of the diode **/
    protected final double length;
    
    /** Area of the diode **/
    protected final double area;

    /** Perimeter of the diode **/
    protected final double perimeter;
    
    /**
     * Constructor.
     **/
    public Diode(HierName name,
                 int type,
                 HierName source,
                 HierName drain,
                 double width,
                 double length,
                 double area,
                 double perimeter) {
        this.name = name;
        this.source = source;
        this.drain = drain;
        this.type = type;
        this.width = width;
        this.length = length;
        this.area = area;
        this.perimeter = perimeter;
    }
    
    public HierName getName() { return name; }

    public int getType() { return type; }

    public double getWidth() { return width; }

    public double getLength() { return length; }

    public double getArea() { return area; }

    public double getPerimeter() { return perimeter; }
    
    public HierName getSource() { return source;}
    
    public HierName getDrain() { return drain;}
}

