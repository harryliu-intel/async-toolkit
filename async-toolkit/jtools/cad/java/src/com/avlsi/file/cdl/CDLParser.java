/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */



package com.avlsi.file.cdl;

import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.StringTokenizer;

import com.avlsi.file.aspice.AspiceFile;
import com.avlsi.file.aspice.Diode;
import com.avlsi.file.aspice.Resistor;
import com.avlsi.file.aspice.Transistor;
import com.avlsi.file.common.Capacitor;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.ext.parse.ExtParser;
import com.avlsi.io.PositionStackReader;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.util.text.StringUtil;

/**
 * Parses cadence .cdl files into {@link AspiceFile} class.
 *
 * @see AspiceFile
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public final class CDLParser {
    private String line = null;
    private PositionStackReader reader = null;
    private final Map aspiceCellRepos = new HashMap();
    private final Map resistorMap;
    private final char subcellConnectionSeparatorChar;
    private boolean assura_rcx_extract;

    private int lineNum = 1;

    /**
     * Class constructor.
     *
     * @param subcellConnectionSeparatorChar  the character that
     *   should be used to separate the node name from the 
     *   cell name in arguments to 'X' lines.
     * @param resistorMap  (may be null) a Map from resistor type
     *   (String) to rho (Double).  When a subcircuit is encountered,
     *   the rho for the type is looked up in <code>resistorMap</code>,
     *   and a resistor is created with resistance rho * l / w.
     **/
    public CDLParser(final char subcellConnectionSeparatorChar,
            final Map resistorMap) {
        this.subcellConnectionSeparatorChar =
            subcellConnectionSeparatorChar;
        this.resistorMap = resistorMap;
    }

    /**
     * Class constructor.
     *
     * @param subcellConnectionSeparatorChar  the character that
     *   should be used to separate the node name from the 
     *   cell name in arguments to 'X' lines.
     **/
    public CDLParser(final char subcellConnectionSeparatorChar) {
        this(subcellConnectionSeparatorChar, null);
    }

    /**
     * Class constructor. Uses '/' for subcell connection separator.
     **/
    public CDLParser() {
        this('/', null);
    }

    /**
     * Map from cell name <code>String</code> to <code>String[]</code>
     * representing names of ports.  If a use has been seen, but no
     * definition, then the value will be an array of String with the
     * correct length seen in the uses, but nulls for the entries. If
     * a definition has been seen, then the values will also be filled in.
     **/
    private final Map cellPortMap = new TreeMap();

    private CDLFileFormatException cdlFileFormatException(
            final String message) {
        return new CDLFileFormatException(message);
    }

    /**
     * Call this to enable Assura RCX parsing compatibility.  Node
     * names of the form 'Xblah/' are converted to 'blah.'.
     **/
    public void setAssuraRCXParsing() {
        assura_rcx_extract = true;
    }

    public void parseFile(final String fileName)
        throws CDLFileFormatException, IOException {
        parseStream(new FileInputStream(fileName));
    }

    public void parseStream(final InputStream in)
        throws CDLFileFormatException, IOException {
        reader = new PositionStackReader(new InputStreamReader(in));

        reader.savePosition();
        while ((line = nextLine()) != null) {
            // skip empty lines
            if (line.length() == 0) {
                reader.discardPosition();
                reader.savePosition();
                continue;
            }

            // skip comment lines
            if (line.charAt(0) == '*') {
                reader.discardPosition();
                reader.savePosition();
                continue;
            }

            final String[] words = split(line);

            if (".SUBCKT".equalsIgnoreCase(words[0])) {
                reader.restorePosition();
                parseSubckt();
                reader.savePosition();
            } else if (".PARAM".equalsIgnoreCase(words[0]) ||
                       ".GLOBAL".equalsIgnoreCase(words[0])) {
                // ignore .PARAM & .GLOBAL
                reader.discardPosition();
                reader.savePosition();
            } else {
                throw cdlFileFormatException("Unknown directive " +
                        words[0] + " on line " + lineNum +
                        "; line is: " + line);
            }
        }
    }

    /**
     * Parses a .SUBCKT / .ENDS section into an AspiceFile, adding
     * the parsed AspiceFile to the map of cells.  It is an error if
     * a cell of the same name as already been parsed.  The stream
     * position must be before the .SUBCKT.
     **/
    private void parseSubckt()
        throws CDLFileFormatException, IOException {
        line = nextLine();
        String[] words = split(line);
        // .SUBCKT subname output_node ... / input_node ...
        // We treat input and output nodes the same

        Debug.assertTrue(".SUBCKT".equalsIgnoreCase(words[0]));

        // array[2] of Map from Pair of (source:HierName, drain:HierName)
        // to double[4], representing
        // diode info: {area, perim, width, length}
        final Map[] diodeInfoMap =
            new HashMap[]{new HashMap(), new HashMap()};

        // the name will look like lib-BUF_1of2, this will not
        // match the cast name which is just BUF_1of2, but in
        // the future should change to lib/BUF_1of2
        final String cellType = unmangleType(words[1]);

        // ignore out any "/" to merge the input and output ports
	String [] nodes = new String[words.length];
	int numDefPorts = 0;
	int numSlashes = 0;
        for (int i = 2; i < words.length; ++i) {
          if ("/".equals(words[i])) numSlashes++;
	  else  nodes[numDefPorts++] = words[i];
        }
	if (numSlashes>1)
	  throw cdlFileFormatException("More than one / in port list.");
	

        // the rest of the params are the exported nest
        String[] defPorts = (String[]) cellPortMap.get(cellType);

        if (defPorts != null) {
            throw cdlFileFormatException("Multiple definition of " +
                    cellType);
            /*
             * Don't allow use before definition because it complicates
             * uses that occur before definition.
            // check that the ports are compatable
            if (numDefPorts != defPorts.length)
                throw cdlFileFormatException("Found .SUBCKT " + cellType +
                        " with " + numDefPorts + " ports, but earlier use " +
                        "had " + ports.length + " ports.");

            for (int i = 0; i < ports.length; ++i)
                Debug.assertTrue(ports[i] == null);
            */
        } else {
            Debug.assertTrue(getCell(cellType) == null);
            // add the port list
            defPorts = new String[numDefPorts];
            cellPortMap.put(cellType, defPorts);
        }

        // fill in the right values for the port names
        System.arraycopy(nodes, 0, defPorts, 0, numDefPorts);

        final AspiceFile aspiceCell =
            new AspiceFile(cellType, aspiceCellRepos);

        // add names, also handling globals
        for (int i = 0; i < numDefPorts; ++i)
            aspiceCell.addPort(parseNodeName(defPorts[i]));

        while ((line = nextLine()) != null) {
            words = split(line);
            if (Character.toUpperCase(words[0].charAt(0)) == 'M') {
                // mosfet
                // Mxxx drain gate source bulk mname W=width L=length 
                // [ M=mag
                // | as=source_area ps=source_perim 
                //   ad=drain_area pd=draim_perim ]
                //  {a,p}{s,d} are floating point numbers x.yye-zz
                if (words.length != 8 && words.length != 9 && words.length != 12) {
                    for (int i=0; i<words.length; i++)
                        System.out.println("words["+i+"]: "+words[i]);

                    throw cdlFileFormatException("Wrong # of fields " +
                            "for mosfet.  Expected 8 or 9 or 12, got " +
                            words.length + ":\n" + line);
                }

                final HierName drain = parseNodeName(words[1]);
                final HierName gate = parseNodeName(words[2]);
                final HierName source = parseNodeName(words[3]);
                final HierName bulk = parseNodeName(words[4]);
                final int type = parseType(words[5]);

                if (findLastNonParm(words) > 5)
                    throw cdlFileFormatException("Wrong # of non-params " +
                                                 "for mosfet.");

                double width = -1.0;
                double length = -1.0;
                double[] area = new double[2];
                double[] perim = new double[2];
                area[0] = -1.0;
                area[1] = -1.0;
                perim[0] = -1.0;
                perim[1] = -1.0;

                for (int i=6; i<words.length; i++) {
                    String s = words[i].toLowerCase();
                    if (s.startsWith("w=")) {
                        width = parseParam(s, "w=", "width");
                    } 
                    else if (s.startsWith("l=")) {
                        length = parseParam(s, "l=", "length");
                    }
                    else if (s.startsWith("as=")) {
                        area[0] = parseParam(s, "as=", "source area");
                    }
                    else if (s.startsWith("ps=")) {
                        perim[0] = parseParam(s, "ps=", "source perim");
                    }
                    else if (s.startsWith("ad=")) {
                        area[1] = parseParam(s, "ad=", "drain area");
                    }
                    else if (s.startsWith("pd=")) {
                        perim[1] = parseParam(s, "pd=", "drain perim");
                    }
                    else if (s.startsWith("m=")) {
                        throw cdlFileFormatException("Can only support M=1, " +
                            "got " + s);
                    }
                    else
                        throw cdlFileFormatException("Unsupported MOSFET "+
                            "parameter specified at line " + lineNum);
                }

                if (length == -1.0 || width == -1.0) {
                    throw cdlFileFormatException("Length or width omitted in "+
                        "MOSFET definition at line " + lineNum);
                }

                if (area[0] != -1.0 || area[1] != -1.0 || 
                    perim[0] != -1.0 || perim[1] != -1.0) {
                    if (area[0] == -1.0 || area[1] == -1.0 || 
                        perim[0] == -1.0 || perim[1] == -1.0)
                        throw cdlFileFormatException("Incomplete AS/AD/PS/PD "+
                            "parameter set specified at line " + lineNum);

                    // XXX: note that these diodes will not be combined with
                    // those created with 'D'

                    final HierName[] ns = new HierName[2];
                    ns[0] = source;
                    ns[1] = drain;

                    for (int i = 0; i < 2; ++i) {
                        final Pair p = new Pair(ns[i], bulk);

                        double[] diodeInfo =
                            (double[]) diodeInfoMap[type].get(p);

                        if (diodeInfo == null) {
                            diodeInfo = new double[]{0.0, 0.0,
                                                     0.0, Double.MAX_VALUE};
                            diodeInfoMap[type].put(p, diodeInfo);
                        }

                        // diode info: {area, perim, width, length}
                        diodeInfo[0] += area[i];
                        diodeInfo[1] += perim[i];
                        diodeInfo[2] += width;
                        // use length here, even though it's really
                        // irrelevant.  It's just to get a ballpark
                        // figure for the model.
                        if (length < diodeInfo[3])
                            diodeInfo[3] = length;
                    }
                }

                // add the fet
                aspiceCell.addTransistor(new Transistor(type,
                            source, drain, gate, bulk, width, length));
            } else if (Character.toUpperCase(words[0].charAt(0)) == 'C') {
                // capacitor
                // Cxxx npositive nminus {cap} [{M=multiplier} 
                //      {$[mname] / $.MODEL = mname} {$SUB=substrate}]
                // 
                // Capacitance is optional in .cdl, but required here.
                // Multiplier, if specified, must be 1.
                // Mname and substrate are ignored here.

                if (words.length < 4) {
                    throw cdlFileFormatException("Too few fields for " +
                            "capacitor.  Expected 4, got " + words.length);
                }
                final HierName npositive = parseNodeName(words[1]);
                final HierName nminus = parseNodeName(words[2]);

                // ignore 'Inf' capacitance for now:

                if (!words[3].equals("Inf")) {
                    final double cap = parseDouble(words[3], "capacitance");

                    // Check for multiplier fifth argument

                    if (words.length > 4 && words[4].startsWith("M=") &&
                        !"M=1".equalsIgnoreCase(words[4]))
                        throw cdlFileFormatException("Can only support M=1, " +
                                " got " + words[4]);

                    aspiceCell.addCapacitor(new Capacitor(npositive, 
                                nminus, cap));
                }
                else {
                    System.err.println("Warning: Ignoring 'Inf' capacitance.");
                }
            } else if (Character.toUpperCase(words[0].charAt(0)) == 'D') {
                // diode
                // Dxxx npositive nminus mname {area} {M=multiplier}
                //      {periphery} {$SUB=substrate}
                //
                // Mname must contain exactly one of 'N' or 'P'.
                // Area and periphery are optional in .cdl, but required here.
                // Multiplier must be 1.
                // Substrate is ignored here.

                // XXX: note that these diodes will not be combined with
                // those created via as=, ... on 'M' lines.

                if (words.length < 7) {
                    throw cdlFileFormatException("Too few fields for " +
                            "diode.  Expected 7, got " + words.length);
                }
                final HierName npositive = parseNodeName(words[1]);
                final HierName nminus = parseNodeName(words[2]);
                final boolean isNType = (words[3].indexOf('N') >= 0);
                final boolean isPType = (words[3].indexOf('P') >= 0);
                final double area = parseDouble(words[4], "area");
                final String multiplier = words[5];
                final double periphery = parseDouble(words[6], "periphery");
                int type;

                if (isNType == isPType) {
                    throw cdlFileFormatException("Mname argument for diode "
                            + "must contain exactly one of 'N' or 'P'.  Got "
                            + words[3]);
                }

                type = isNType ? DeviceTypes.N_TYPE : DeviceTypes.P_TYPE;

                if (!"M=1".equalsIgnoreCase(multiplier))
                    throw cdlFileFormatException("Can only support M=1, " + 
                            " got " + multiplier);

                final double width = (periphery
                        + java.lang.Math.sqrt(periphery * periphery
                            - 4 * area)) / 2;
                final double length = area / width;

                aspiceCell.addDiode(new Diode(type, npositive, nminus,
                            width, length, area, periphery));
            } else if (Character.toUpperCase(words[0].charAt(0)) == 'R') {
                // resistor
                // Rxxx term1 term2 {res / r=res} {$SUB=substrate} {M=multiplier}
                //      {$[mname] / $.MODEL=mname} {w=width} {l=length}
                //
                // Res is optional in .cdl, but required here.
                // All arguments after res are ignored here.

                if (words.length < 4) {
                    throw cdlFileFormatException("Too few fields for "
                            + "resistor.  Expected 4, got " + words.length);
                }
                final HierName term1 = parseNodeName(words[1]);
                final HierName term2 = parseNodeName(words[2]);
		double res = 0;
		for (int i=3; i<words.length; i++) {
		    if (words[i].startsWith("w=")); // ignore width for now
		    else if (words[i].startsWith("l=")); // ignore length for now
		    else if (words[i].startsWith("r="))
			res = parseParam(words[i],"r=","resistance");
		    else if (words[i].startsWith("$")); // ignore model for now
		    else res = parseDouble(words[i], "resistance");
		}

		if (res<=0) throw cdlFileFormatException("Resistance <= 0");
                aspiceCell.addResistor(new Resistor(term1, term2, 1/res));
            } else if (Character.toUpperCase(words[0].charAt(0)) == 'X') {
                // subcircuit call
                // cdl: Xyyy node1 ... / subname
                // spice: Xyyy node1 node2 ... nodeN subname
                //   parm1=v2 parm2=v2 ...
                if (words.length < 2)
                    throw cdlFileFormatException("Too few arguments for " +
                            "subcircuit call " + words[0]);

                final String instanceNameString =
                    ExtParser.convertArrays(words[0].substring(1));
                final HierName instanceName;
                try {
                    instanceName =
                        HierName.makeHierName(instanceNameString, '.');
                } catch (InvalidHierNameException e) {
                    throw new AssertionFailure(e);
                }
                final String slash = words[words.length - 2];
                final boolean isCDL = "/".equals(slash);
                final int typeIdx = findLastNonParm(words);
                final String subcellType = unmangleType(words[typeIdx]);

                // param names are in words[1..typeIdx-2] if there is
                // a /, or words[1..typeIdx-1] if there is not
                final int numUsePorts =
                    isCDL ? typeIdx - 2 : typeIdx - 1;
                final Double rho = resistorMap == null ?
                    null : (Double) resistorMap.get(subcellType);
                if (rho != null) {
                    // resistor type
                    if (numUsePorts != 2) {
                        throw cdlFileFormatException("Found " +
                                numUsePorts + " for resistor, not 2:" +
                                line);
                    }

                    if (typeIdx != words.length - 3) {
                        throw cdlFileFormatException("Found " + numUsePorts +
                                " params for resistor, not 2." + line);
                    }

                    Debug.assertTrue(typeIdx == 3);

                    final HierName t1 = parseNodeName(words[1]);
                    final HierName t2 = parseNodeName(words[2]);
                    final double width =
                        parseParam(words[typeIdx + 1], "w=",
                                "resistor width");
                    final double length =
                        parseParam(words[typeIdx + 2], "l=",
                                "resistor length");
                    final double res = rho.doubleValue() * (length / width);

                    aspiceCell.addResistor(new Resistor(t1, t2, 1/res));
                } else {
                    // normal type
                    String[] ports = (String[]) cellPortMap.get(subcellType);
                    
                    if (ports != null) {
                        // check that number of ports is right
                        if (numUsePorts != ports.length)
                            throw cdlFileFormatException("Subcell use " +
                                    words[0] + " with " + numUsePorts +
                                    " ports, but definition had " +
                                    ports.length + " ports.");
                    } else {
                        throw cdlFileFormatException("Subcell use " +
                                words[0] + " of type " + subcellType + 
                                " without preceding .SUBCKT");
                        /*
                         * Don't bother with this, how would we know
                         * what to connect to?
                        // add dummy port list to record number of ports
                        cellPortMap.put(subcellType, ports);
                        */
                    }

                    // add subcell
                    aspiceCell.addSubcell(instanceNameString,
                            getCell(subcellType));

                    // process connections to ports
                    for (int i = 0; i < numUsePorts; ++i) {
                        final HierName port = parseNodeName(ports[i]);
                        final HierName param = parseNodeName(words[i + 1]);

                        try {
                            // use / or . as separator
                            aspiceCell.connectNames(param,
                                    HierName.makeHierName(
                                        instanceName.getAsString('.') +
                                        subcellConnectionSeparatorChar +
                                        port.getAsString('.'), '.'));
                        } catch (InvalidHierNameException e) {
                            throw (AssertionFailure)
                                new AssertionFailure("Invalid HierName " +
                                    "-- can't happen!").initCause(e);
                        }
                    }
                }
            } else if (".ENDS".equalsIgnoreCase(words[0])) {
                // now add the diodes that were collected from 'M' lines
                for (int itype = 0; itype < 2; ++itype) {
                    for (final Iterator iEntry =
                            diodeInfoMap[itype].entrySet().iterator();
                            iEntry.hasNext(); ) {
                        final Entry e = (Entry) iEntry.next();
                        final Pair p = (Pair) e.getKey();
                        final double[] diodeInfo = (double[]) e.getValue();
                        final HierName source = (HierName) p.getFirst();
                        final HierName drain = (HierName) p.getSecond();
                        final double width = diodeInfo[2];
                        final double length = diodeInfo[3];
                        final double area = diodeInfo[0];
                        final double perim = diodeInfo[1];

                        aspiceCell.addDiode(new Diode(itype, source, drain,
                                    width, length, area, perim));
                    }
                }

                aspiceCellRepos.put(cellType, aspiceCell);
                return;
            } else {
                throw cdlFileFormatException("Unknown line type: " + line);
            }
        }

        // we should not reach eof in a subcircuit
        throw cdlFileFormatException("reached EOF in .SUBCKT");
    }

    /**
     * Reads a line, returning it, or null if end of stream has been reached.  
     * The final '\n' is not returned.
     **/
    private String readLine() throws IOException {
        final StringBuffer sb = new StringBuffer();
        int ch;

        while ((ch = reader.read()) != -1) {
            if (ch == '\n') {
                lineNum++;
                break;
            }

            sb.append((char) ch);
        }

        if (ch == -1 && sb.length() == 0)
            return null;
        else
            return sb.toString();
    }

    /**
     * Reads a line, handling line concatenation via '+'.  Final '\n' is
     * not returned.
     **/
    private String readContinuedLine() throws IOException {
        String s = readLine();

        if (s == null)
            return null;

        final StringBuffer sb = new StringBuffer(s);
        int savedLineNum = lineNum;
        reader.savePosition();
        while ((s = readLine()) != null) {
            if (s.length() > 0 && s.charAt(0) == '+') {
                reader.discardPosition();
                reader.savePosition();
                savedLineNum = lineNum;
                // append a space instead of the +
                sb.append(' ').append(s.substring(1));
            } else {
                lineNum = savedLineNum;
                reader.restorePosition();
                break;
            }
        }

        if (s == null)
            reader.discardPosition();

        return sb.toString();
    }

    /**
     * Returns the next line, handling both continuation and comment lines.
     * Ignores empty lines.
     **/
    private String nextLine() throws IOException {
        String s;

        while ((s = readContinuedLine()) != null) {
            if (s.length() == 0)
                continue;
            else if (s.charAt(0) == '*')
                continue;
            else
                break;
        }

        return s;
    }

    private int findLastNonParm(final String[] words) {
        for (int i = words.length - 1; i >= 0; --i) {
            if (words[i].indexOf('=') == -1)
                return i;
        }

        return -1;
    }

    private int parseType(final String mname) throws CDLFileFormatException {
        final boolean isNType = mname.toLowerCase().indexOf('n') != -1;
        final boolean isPType = mname.toLowerCase().indexOf('p') != -1;

        if (isNType == isPType) {
            throw cdlFileFormatException("Bad modelname " + mname +
                    " must contain exactly one 'n' or 'p'.  Got " + mname);
        } else
            return isNType ? DeviceTypes.N_TYPE : DeviceTypes.P_TYPE;
    }

    private double parseParam(final String param, final String startsWith,
            final String message) throws CDLFileFormatException {
        if (!param.toUpperCase().startsWith(startsWith.toUpperCase()))
            throw cdlFileFormatException("Bad " + message + ": " + param +
                    " didn't start with " + startsWith);
        
        String d = param.substring(startsWith.length()).toUpperCase();
        if (d.endsWith("M")) d = d.substring(0,d.length()-1) + "E-3";
        else if (d.endsWith("U")) d = d.substring(0,d.length()-1) + "E-6";
        else if (d.endsWith("N")) d = d.substring(0,d.length()-1) + "E-9";
        else if (d.endsWith("P")) d = d.substring(0,d.length()-1) + "E-12";
        else if (d.endsWith("F")) d = d.substring(0,d.length()-1) + "E-15";

        return parseDouble(d, message);
    }

    private double parseDouble(final String d, final String message) 
            throws CDLFileFormatException {

         try {
             return Double.parseDouble(d);
         } catch (NumberFormatException e) {
             throw (CDLFileFormatException)
                 cdlFileFormatException("Bad double format " + d +
                     " for " + message).initCause(e);
         }
    }

    private boolean isInteger(final String s) {
        for (int i = 0; i < s.length(); ++i) {
            if (s.charAt(i) < '0' || s.charAt(i) > '9') {
                return false;
            }
        }

        return true;
    }

    private HierName parseNodeName(String s) {

        // convert arrays back
        s = StringUtil.replaceSubstring(s, "][", ",");

        // Convert 'Xblah/' to 'blah.' if this is an Assura RCX extract
        if (assura_rcx_extract) {
            StringTokenizer st = new StringTokenizer(s,"/");
            StringBuffer sb = new StringBuffer();
            while (st.hasMoreTokens()) {
                String x = st.nextToken();
                if (st.hasMoreTokens()) sb.append(x.substring(1)+".");
                else sb.append(x);
            }
            s = sb.toString();
        }
            

        try {
            if (s.startsWith("av")) {
                // spice uses av[A-Z][0-9]+(_[0-9]+)?
                // XXX: what if there is a real node called this?
                if (s.length() > 3 &&
                    s.charAt(2) >= 'A' && s.charAt(2) <= 'Z') {
                    final String t = s.substring(3);
                    if (isInteger(t))
                        s += '#';
                    else {
                        final int underIdx = t.indexOf('_');
                        if (underIdx != -1 &&
                            isInteger(t.substring(0, underIdx)) &&
                            isInteger(t.substring(underIdx + 1))) {
                            s += '#';
                        }
                    }
                }
            } else {
                // cdl uses integers for anonymous nodes: [0-9]+

                // add a hash on to the end if the name is an integer
                if (isInteger(s))
                    s += '#';
            }

            return HierName.makeHierName(s, '.');
        } catch (InvalidHierNameException e) {
            throw (AssertionFailure)
                new AssertionFailure("Invalid HierName -- can't happen!")
                    .initCause(e);
        }
    }

    private void go(String file) throws Throwable {
        reader = new PositionStackReader(new InputStreamReader(new
                    FileInputStream(file)));
        String s;

        while ((s = nextLine()) != null) {
            System.err.println(">" + s + "<");
        }
    }

    public AspiceFile getCell(final String cellType) {
        return (AspiceFile) aspiceCellRepos.get(cellType);
    }

    public Iterator getCellTypes() {
        return aspiceCellRepos.keySet().iterator();
    }

    private String unmangleType(final String typeName)
        throws CDLFileFormatException {
        final StringBuffer sb = new StringBuffer();

        // 1. convert any occurences of #xx to the appropriate character
        // x must be a hex digit
        // 2. map the first - to /

        boolean firstDash = true;
        for (int i = 0; i < typeName.length(); ++i) {
            final char ch = typeName.charAt(i);

            if (ch == '#') {
                try {
                    if (typeName.charAt(i + 1) != '#')
                        throw cdlFileFormatException("Single hash in " +
                                typeName);

                    final String hexDigits = "0123456789ABCDEF";
                    final int i1 = hexDigits.indexOf(
                            Character.toUpperCase(typeName.charAt(i + 2)));
                    final int i2 = hexDigits.indexOf(
                            Character.toUpperCase(typeName.charAt(i + 3)));

                    if (i1 == -1 || i2 == -1)
                        throw cdlFileFormatException("Bad hex escape in " +
                                typeName);

                    sb.append((char) (16 * i1 + i2));

                    i += 3;
                } catch (IndexOutOfBoundsException e) {
                    throw (CDLFileFormatException)
                        cdlFileFormatException("Bad hex escape in " +
                            typeName).initCause(e);
                }
            } else if (ch == '-' && firstDash) {
                sb.append('/');
                firstDash = false;
            } else
                sb.append(ch);
        }

        return sb.toString();
    }

    private static String[] split(final String line) {
        StringTokenizer st = new StringTokenizer(line);
        final String[] s = new String[st.countTokens()];

        for (int i = 0; i < s.length; ++i)
            s[i] = st.nextToken();

        return s;
    }

    public static void main(String[] args) throws Throwable {
        // new CDLParser().go(args[0]);
        new CDLParser().parseFile(args[0]);
    }
}
