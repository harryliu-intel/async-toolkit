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
import com.avlsi.file.common.HierName;

/**
 * Class for building custom sources
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class SourceHandler {

    /**
     * Constructor.
     **/
    public SourceHandler() { }

    public VoltageSource buildSource(HierName name,
        Node nplus, Node nminus, String[] args)
        throws SourceHandlerException { return null;}

    public static double[] parseDoubles(String[] args)
            throws NumberFormatException {
        double[] dargs = new double[args.length];
        for (int loop=0;loop<dargs.length;loop++) {
            dargs[loop] = Double.parseDouble(args[loop]);
        }
        return dargs;
    }
    
    public static double parseDouble(String arg)
            throws SourceHandlerException {
        try {
            return Double.parseDouble(arg);
        } catch (NumberFormatException e) {
            throw new SourceHandlerException("Number format error: "+
                                           e.getMessage(), e);
        }
    }
   
    public static int findEquals(String str) {
        for(int loop=0;loop<str.length();loop++) {
            if (str.charAt(loop) == '=') return loop;
        }
        return -1;
    }

    public static HashMap getDefaultTypes() {
        SourceHandler pulse = new SourceHandler() {
            public VoltageSource buildSource(HierName name,
                    Node nplus, Node nminus, String[] args) 
                    throws SourceHandlerException {
                if (args.length != 7)
                    throw new SourceHandlerException(
                        "PULSE voltage source"+
                        " usuage (incorrect # of arguments):\n"+
                        "Vxxx n+ n- PU[LSE] v1 v2 td tr tf pw per,"+
                        " where v1 .. per are doubles");
                double[] dargs = null;
                try {
                    dargs = parseDoubles(args);
                } catch (NumberFormatException e) {
                    throw new SourceHandlerException(
                        "PULSE voltage source"+
                        " usuage (argument format error):\n"+
                        "Vxxx n+ n- PU[LSE] v1 v2 td tr tf pw per,"+
                        " where v1 .. per are doubles", e);
                }
                System.out.println("PULSE Source built: v1="+dargs[0]+
                " v2="+ dargs[1]+" period="+ dargs[6]);
                return new PulseVoltageSource(name, nplus, nminus,
                        dargs[0],dargs[1], dargs[2], dargs[3],
                        dargs[4],dargs[5], dargs[6]);
            }
        };
        
        SourceHandler sin= new SourceHandler() {
            public VoltageSource buildSource(HierName name,
                    Node nplus, Node nminus, String[] args) 
                    throws SourceHandlerException {
                if (args.length != 6)
                    throw new SourceHandlerException("SIN voltage source"+
                            " usuage (incorrect # of arguments):\n"+
                            "Vxxx n+ n- SIN vo va freq td theta phi,"+
                            " where vo .. phi are doubles");
                double[] dargs = null;
                try {
                    dargs = parseDoubles(args);
                } catch(NumberFormatException e) {
                    throw new SourceHandlerException(
                            "SIN voltage source"+
                            " usuage (argument format error):\n"+
                            "Vxxx n+ n- EXP vo va freq td theta phi,"+
                            " where vo .. phi are doubles", e);
                }
                System.out.println("SIN Source built: vo="+dargs[0]+
                    " va="+dargs[1]+" freq="+dargs[2]);
                return new SinVoltageSource(name, nplus, nminus,
                        dargs[0],dargs[1], dargs[2], dargs[3],
                        dargs[4],dargs[5]);
            }
        };
        
        SourceHandler exp = new SourceHandler() {
            public VoltageSource buildSource(HierName name,
                    Node nplus, Node nminus, String[] args) 
                    throws SourceHandlerException {
                if (args.length != 6)
                    throw new SourceHandlerException("EXP voltage source"+
                            " usuage (incorrect # of arguments):\n"+
                            "Vxxx n+ n- EXP v1 v2 td1 tau1 td2 tau2,"+
                            " where v1 .. tau2 are doubles");
                double[] dargs = null;
                try {
                    dargs = parseDoubles(args);
                } catch(NumberFormatException e) {
                    throw new SourceHandlerException(
                            "EXP voltage source"+
                            " usuage (argument format error):\n"+
                            "Vxxx n+ n- EXP v1 v2 td1 tau1 td2 tau2,"+
                            " where v1 .. tau2 are doubles", e);
                }
                System.out.println("EXP Source built: v1="+dargs[0]+
                    " v2="+dargs[1]+" tau1="+dargs[3]+" tau2= "+dargs[5]);
                return new ExpVoltageSource(name, nplus, nminus,
                        dargs[0],dargs[1], dargs[2], dargs[3],
                        dargs[4],dargs[5]);
            }
        };
     
        SourceHandler pwl = new SourceHandler() {
            public VoltageSource buildSource(HierName name,
                    Node nplus, Node nminus, String[] args) 
                    throws SourceHandlerException {
                double rVal=-1, tdVal=0;
                String filename = null;
                int rIndex =-1,tdIndex=-1, subCount=0;
                if ((args == null) || (args.length == 0))
                    throw new SourceHandlerException("PWL: Not enough"+
                                                     " arguments.");
                int start = 0;
                try {
                    parseDouble(args[0]);
                } catch (SourceHandlerException e) {
                    //We have a filename
                    filename = args[0];
                    start = 1;
                }
                for(int loop=start;loop<args.length;loop++) {
                    args[loop] = args[loop].toUpperCase();
                    if (args[loop].startsWith("R")) {
                        //Repeat value
                        int eq = findEquals(args[loop]);
                        if (eq >=0) rVal = 
                            parseDouble(args[loop].substring(eq+1));
                        else rVal = 0;
                        rIndex =loop;
                        subCount++;
                    } else if (args[loop].startsWith("TD")) {
                        //TD value
                        int eq = findEquals(args[loop]);
                        if (eq >=0) tdVal = 
                            parseDouble(args[loop].substring(eq+1));
                        tdIndex=loop;
                        subCount++;
                    }
                }
                if (filename != null) {
                    try {
                        PWLVoltageSource src =
                            new PWLVoltageSource(name, nplus, nminus,
                                filename, rVal, tdVal);
                        System.out.println("PWL Source built: name="+name+
                        " across "+nplus.getName()+" and "+
                        nminus.getName()+" from file "+filename);
                        return src;
                    } catch (java.io.FileNotFoundException e) {
                        throw new SourceHandlerException("File named "+filename+
                                " not found, PWL not built", e);
                    } catch (java.io.IOException e) {
                        throw new SourceHandlerException(
                                "IOException : "+e.getMessage(), e);
                    }
                } 
                double[] vt = new double[args.length-subCount];
                int count = 0;
                for(int loop=0;loop<args.length;loop++) {
                    if ((loop != rIndex) && (loop != tdIndex)) {
                        vt[count++] = parseDouble(args[loop]);
                    }
                }
                System.out.println("PWL Source built: name="+name+
                    " across "+nplus.getName()+" and "+
                    nminus.getName());
                return new PWLVoltageSource(name, nplus, nminus,
                        vt, rVal, tdVal);
            }
        };
        HashMap types = new HashMap();
        types.put("PULSE",pulse);
        types.put("PU",pulse);
        types.put("SIN",sin);
        types.put("EXP",exp);
        types.put("PWL",pwl);
        return types;
    }
    
    /*****************************************************************
     * Class to represent exceptions in Source processing.
     *****************************************************************/
    public static final class SourceHandlerException 
            extends java.lang.Exception {
        public SourceHandlerException(final String message) {
            super(message);
        }
        public SourceHandlerException(final String message, Throwable cause) {
            super(message, cause);
        }
    }

}

