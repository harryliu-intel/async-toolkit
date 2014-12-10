package com.avlsi.tools.spice2spice;

import com.avlsi.circuit.AbstractCircuit;
import com.avlsi.circuit.CapacitorInterface;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.circuit.DiodeInterface;
import com.avlsi.file.common.HierName;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import com.avlsi.circuit.ResistorInterface;
import com.avlsi.file.spice.SpiceParser;
import com.avlsi.circuit.TransistorInterface;

public class Spice2Spice extends AbstractCircuit {
    private final String match_gate;
    private final int type;
    private final double multiplier;
    
    public static class MyRepository extends Repository {
        private final String match_gate;
        private final int type;
        private final double multiplier;

        public MyRepository(String match_gate, int type, double multiplier) {
            this.match_gate = match_gate;
            this.type = type;
            this.multiplier = multiplier;
        }

        protected AbstractCircuit newCircuitGenerator(final String cellType) {
            return new Spice2Spice(cellType, match_gate, type, multiplier);
        }
    }

    public Spice2Spice(final String cellType, String match_gate,
                       int type, double multiplier) {
        super(cellType);
        this.match_gate = match_gate;
        this.type = type;
        this.multiplier = multiplier;
    }

    private int numD = 0;
    private int numR = 0;
    private int numC = 0;
    private int numT = 0;

    private List params = new LinkedList();

    private int counter = 10000;

    private String inventName(String prefix, HierName h) {
        if (h == null) {
            return prefix + counter++;
        } else {
            return h.toString();
        }
    }

    private void begin() {
        if (params != null) {
            StringBuffer buf = new StringBuffer(".SUBCKT ");
            buf.append(getType());
            for (Iterator it = params.iterator(); it.hasNext(); ) {
                buf.append(' ');
                buf.append(it.next());
            }
            System.out.println(buf);
            params = null;
        }
    }

    /**
     * Adds a diode to the circuit (to be defined by subclass).
     **/
    public void addDiode(final DiodeInterface diode) {
        begin();
        System.out.println(inventName("D", diode.getName()) +
                           " " + diode.getSource() +
                           " " + diode.getDrain() +
                           (diode.getType() == DeviceTypes.N_TYPE ?
                            " n" : " p") +
                           " " + diode.getArea() +
                           " M=1 " + diode.getPerimeter());
        numD++;
    }

    /**
     * Adds a resistor to the circuit (to be defined by subclass).
     **/
    public void addResistor(final ResistorInterface resistor) {
        begin();
        System.out.println(inventName("R", resistor.getName()) +
                           " " + resistor.getSource() +
                           " " + resistor.getDrain() +
                           " " + (1/resistor.getConductance()));
        numR++;
    }

    /**
     * Adds a capacitor to the circuit (to be defined by subclass).
     **/
    public void addCapacitor(final CapacitorInterface capacitor) {
        begin();
        System.out.println(inventName("C", capacitor.getName()) +
                           " " + capacitor.getSource() +
                           " " + capacitor.getDrain() +
                           " " + capacitor.getCapacitance());
        numC++;
    }

    /**
     * Adds a transistor to the circuit (to be defined by subclass).
     **/
    public void addTransistor(final TransistorInterface transistor) {
        begin();
        double width = transistor.getWidth();
        String gate = transistor.getGate().toString();
        int theType = transistor.getType();

        if (type == theType && gate.matches(match_gate)) {
            width *= multiplier;
        }

        System.out.println(inventName("M", transistor.getName()) +
                           " " + transistor.getDrain() +
                           " " + gate +
                           " " + transistor.getSource() +
                           " " + transistor.getBulk() +
                           (theType == DeviceTypes.N_TYPE ? " n" : " p") +
                           " w=" + width +
                           " l=" + transistor.getLength());
        numT++;
    }

    /**
     * Returns the number of diodes in the circuit (to be defined by subclass).
     **/
    public int getDiodeCount() {
        return numD;
    }

    /**
     * Returns the number of resistors in the circuit
     * (to be defined by subclass).
     **/
    public int getResistorCount() {
        return numR;
    }

    /**
     * Returns the number of capacitors in the circuit
     * (to be defined by subclass).
     **/
    public int getCapacitorCount() {
        return numC;
    }

    /**
     * Returns the number of MOSFET transistors in the circuit
     * (to be defined by subclass).
     **/
    public int getTransistorCount() {
        return numT;
    }

    //
    // Namespace related methods
    //

    /**
     * Adds a cell or node name to the circuit
     **/
    public void addName(final HierName name) {
        params.add(name.toString());
        super.addName(name);
    }

    private static void usage() {
        System.err.println("Usage: spice2spice --task=scale --file=<input>");
        System.err.println("        --match-gate=<regexp> --type=[n|p] --multiplier=<float>");
        System.exit(1);
    }

    public static void main(String[] args) throws Throwable {
        // Command-line handling code stolen from Csp2TT
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
	final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles(parsedArgs); 
	final CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs(argsWithConfigs);
	final CommandLineArgs theArgs = cachedArgs;
	
        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    Spice2Spice.class));
        }
	
        if (theArgs.argExists("help") || !theArgs.argExists("file") ||
            theArgs.nonParsedArgumentsIterator().hasNext())
            usage();

        String file = theArgs.getArgValue("file", null);
        String match_gate = theArgs.getArgValue("match-gate", ".");
        String s_type = theArgs.getArgValue("type", "");
        String s_multiplier = theArgs.getArgValue("multiplier", "1");

        int type = DeviceTypes.NUM_TYPES;

        if (s_type.length() > 0) {
            switch (s_type.toLowerCase().charAt(0)) {
            case 'n':
                type = DeviceTypes.N_TYPE;
                break;
            case 'p':
                type = DeviceTypes.P_TYPE;
                break;
            default:
                usage();
            }
        }

        double multiplier = Double.parseDouble(s_multiplier);

        new SpiceParser().parseFile(file, new MyRepository(match_gate,
                                                           type, multiplier));
        System.out.println(".ENDS");
    }
}
