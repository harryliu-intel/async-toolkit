/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

 /* Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.jauto;



/*
 * Class for Transistor Sizing Algorithms
 *
 * @author Qing Wu
 * @version $Date$
 */


public class Transistor
{
    // name of the transistor
    public String                name;

    // size of the transistor
    // note: this is not the real transistor size (width)
    // the real size is calculated as: size * size_of_the_halfoperator
    public double                size;



    public String setName(String s)
    {
        name = s;

        return name;
    }


    public String getName()
    {
        return name;
    }


    public double setSize(double d)
    {
        assert d >= 0.0 : "invalid size for transistor";

        size = d;

        return size;
    }


    public double getSize()
    {
        return size;
    }


    // constructor 1
    public Transistor()
    {
        name = "";
        size = 0.0;
    }

    // constructor 2
    public Transistor(Transistor tr)
    {
        name = tr.name;
        size = tr.size;
    }


    // returns current size
    public double getCurrentSize()
    {
        return size;
    }


    // dump out information for debug
    public void print()
    {
        System.out.println("name: " + name);
        System.out.println("size: " + size);
    }


    public String toString()
    {
        String s = "name: " + name + "\n";
        s += "size: " + size + "\n";

        return s;
    }

}
