/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.ext.parse;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;

import com.avlsi.file.common.Capacitor;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.file.ext.Environment;
import com.avlsi.file.ext.ExtCell;
import com.avlsi.file.ext.FET;
import com.avlsi.file.ext.Node;
import com.avlsi.file.ext.Terminal;
import com.avlsi.io.FileSearchPath;
import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.debug.Debug;
import com.avlsi.util.functions.UnaryAction;
import com.avlsi.util.text.StringUtil;

/**
 * ExtParser parses .ext files into {@link ExtCell}s.
 *
 * @see ExtCell
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class ExtParser {
    // Conversions from .ext units to SI units
    private static final double attoFarads = 1e-18;
    private static final double milliOhms = 1e-3;
    private static final double centiMicrons = 1e-2 * 1e-6;
    private static final double squareCentiMicrons
        = centiMicrons * centiMicrons;

    /**
     * The number of resistance classes that are important to us.
     * We only care about the first two, for ndiff and pdiff.
     **/
    private static final int NUM_IMPORTANT_RESIST_CLASSES = 2;

    private static final char PATH_SEPARATOR = '/';

    // environmental information
    String technology = null;
    Date timestamp = null;
    String version = null;
    String style = null;
    double rscale = 1.0, cscale = 1.0, lscale = 1.0;
    boolean scalesSet = false;
    double[] resistanceClasses = new double[0];
    int numResistClasses = -1;  // the number of resistance classes
                                // specified in the .ext file
                                // this will not equal
                                // resistanceClasses.length because
                                // resistanceClasses.length ==
                                // NUM_IMPORTANT_RESIST_CLASSES,
                                // the number we care to remember

    // parse structures
    final ExtCell ext;

    /**
     * The global repository for subcell definitions.
     **/
    final Map subcellNameToDefnMap;

    /**
     * search path to use to find .ext files.
     **/
    final FileSearchPath extPath;

    // parse state
    private boolean env = true;
    private int lineNumber = 0;
    private String line = null;

    private boolean m_RecurseOnUseStatements;

    /**
     * Class constructor.  Constructs a parser with a new
     * subcellNameToDefnMap
     **/
    public ExtParser(final String name, 
		     final FileSearchPath extPath,
		     boolean RecurseOnUseStatements ) {
        this(name, extPath, new HashMap(), RecurseOnUseStatements );
    }

    /**
     * Class constructor.  Constructs a parser with the given
     * subcellNameToDefnMap
     * @todo jmr XXX check that subcells are parsed with the same 
     * tech file (and other appropriate env info) as parent.
     **/
    public ExtParser(final String name,
                     final FileSearchPath extPath,
                     final Map subcellNameToDefnMap,
		     boolean RecurseOnUseStatements ) {
        // System.err.print("name in constructor:" );
        // System.err.println( name );
        ext = new ExtCell(name, subcellNameToDefnMap);
        this.extPath = extPath;
        this.subcellNameToDefnMap = subcellNameToDefnMap;
	m_RecurseOnUseStatements = RecurseOnUseStatements ;
    }

    /**
     * Constructs an ExtFileFormatException with the appropriate parameters.
     * When the constructor of ExtFileFormatException changes its mind
     * about what it wants, only this method needs to change.
     **/
    private ExtFileFormatException extFileFormatException(
            final String message)
    {
        return (ExtFileFormatException) new ExtFileFormatException(message,
                ext.getName(), lineNumber, line);
    }

    /**
     * Constructs an ExtFileFormatException with the appropriate parameters.
     * When the constructor of ExtFileFormatException changes its mind
     * about what it wants, only this method needs to change.
     **/
    private ExtFileFormatException extFileFormatException(
            final String message, final Exception cause)
    {
        return new ExtFileFormatException
            (message, ext.getName(), lineNumber, line, cause);
    }

    /**
     * Ensures that the words array has n words, throws an exception if this
     * is not the case.
     **/
    private void ensureNumWords(final String[] words,
                                final int n)
        throws ExtFileFormatException
    {
        if (words.length != n)
            throw extFileFormatException("Bad token count, " + n
                    + " tokens expected " + words.length + " found");
    }

    /**
     * Ensures that we are parsing the environment section of the .ext file,
     * throws an exception if this is not the case.  Should be called
     * whenever parsing an environmental directive.
     **/
    private void ensureEnv()
        throws ExtFileFormatException
    {
        if (!env)
            throw extFileFormatException("Environment directive found"
                    + " after non-environment directive");
    }

    /**
     * Ensures that the environment directives have been specified.
     * Should be called whenever parsing a non-environment directive.
     **/
    private void ensureEnvSpecified(final String technology,
                                    final Date timestamp,
                                    final String version,
                                    final String style,
                                    final double[] resistanceClasses)
        throws ExtFileFormatException
    {
        if (technology == null)
            throw extFileFormatException("Technology not specified");
        else if (timestamp == null)
            throw extFileFormatException("Timestamp not specified");
        else if (version == null)
            throw extFileFormatException("Version not specified");
        else if (style == null)
            throw extFileFormatException("Style not specified");
        else if (resistanceClasses.length == 0)
            throw extFileFormatException("Resistclass not specified");
    }

    // Generic datatype parsing ////////////////////////////////////////////

    /**
     * Parses a string into a double.
     **/
    private double parseDouble(final String s,
                               final String message)
        throws ExtFileFormatException
    {
        try {
            return Double.parseDouble(s);
        } catch (NumberFormatException e) {
            throw extFileFormatException(message, e);
        }
    }

    /**
     * Parses a string into an int.
     **/
    private int parseInt(final String s,
                         final String message)
        throws ExtFileFormatException
    {
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            throw extFileFormatException(message, e);
        }
    }

    /**
     * Parses a string into an Integer.
     **/
    private Integer parseInteger(final String s,
                                 final String message)
        throws ExtFileFormatException
    {
        try {
            return Integer.valueOf(s);
        } catch (NumberFormatException e) {
            throw extFileFormatException(message, e);
        }
    }

    /**
     * Parses n alternate doubles from the words array, starting at index 
     * startField.
     **/
    private double[] parseAltDoubles(final String[] words,
                                     final int n,
                                     final int startField,
                                     final double multiplier,
                                     final String errorMsg)
        throws ExtFileFormatException
    {
        // check for num fields
        if (words.length < startField + 2 * (n - 1))
            throw extFileFormatException("Not enough fields");

        final double[] ds = new double[n];

        for (int i = 0; i < n; ++i) {
            final int j = startField + 2 * i;
            ds[i] = parseDouble(words[j], "Field " + j + ": " + errorMsg)
                * multiplier;
        }

        return ds;
    }

    // Unit Parsing ////////////////////////////////////////////////////////

    // Capacitance

    /**
     * Parses a string with a capacitance in attofarads into a capacitance
     * in farads.  
     **/
    private double parseCapacitance(final String s, final String message)
        throws ExtFileFormatException
    {
        try {
            return parseDouble(s, message) * attoFarads * cscale;
        } catch (NumberFormatException e) {
            throw extFileFormatException(message, e);
        }
    }

    /**
     * Parses a string with a capacitance in attofarads into a capacitance
     * in farads.  
     **/
    private double parseCapacitance(final String s)
        throws ExtFileFormatException
    {
        return parseCapacitance(s, "Bad capacitance: " + s);
    }

    // Resistance

    /**
     * Parses a string with a resistance in milliohms into a resistance
     * in ohms.
     **/
    private double parseResistance(final String s, final String message)
        throws ExtFileFormatException
    {
        try {
            return parseDouble(s, message) * milliOhms * rscale;
        } catch (NumberFormatException e) {
            throw extFileFormatException(message, e);
        }
    }

    /**
     * Parses a string with a resistance in milliohms into a resistance
     * in ohms.
     **/
    private double parseResistance(final String s)
        throws ExtFileFormatException
    {
        return parseResistance(s, "Bad resistance: " + s);
    }

    // Length

    /**
     * Parses a string with a length in centimicrons into a length in
     * meters.
     **/
    private double parseLength(final String s, final String message)
        throws ExtFileFormatException
    {
        try {
            return parseDouble(s, message) * centiMicrons * lscale;
        } catch (NumberFormatException e) {
            throw extFileFormatException(message, e);
        }
    }

    /**
     * Parses a string with a length in centimicrons into a length in
     * meters.
     **/
    private double parseLength(final String s)
        throws ExtFileFormatException
    {
        return parseLength(s, "Bad length: " + s);
    }

    private double[] parseAltLengths(final String[] words,
                                     final int n,
                                     final int startField,
                                     final String errorMsg)
        throws ExtFileFormatException
    {
        return parseAltDoubles(words, n, startField, centiMicrons * lscale,
                errorMsg);
    }

    // Area

    /**
     * Parses a string with an area in square centimicrons into an area
     * in square meters.
     **/
    private double parseArea(final String s, final String message)
        throws ExtFileFormatException
    {
        try {
            return parseDouble(s, message) * squareCentiMicrons
                * lscale * lscale;
        } catch (NumberFormatException e) {
            throw extFileFormatException(message, e);
        }
    }

    /**
     * Parses a string with an area in square centimicrons into an area
     * in square meters.
     **/
    private double parseArea(final String s)
        throws ExtFileFormatException
    {
        return parseArea(s, "Bad area: " + s);
    }

    private double[] parseAltAreas(final String[] words,
                                   final int n,
                                   final int startField,
                                   final String errorMsg)
        throws ExtFileFormatException
    {
        return parseAltDoubles(words, n, startField,
                squareCentiMicrons * lscale * lscale, errorMsg);
    }

    /**
     * Strips quotes from the beginning and end of a  name, throwing an
     * exception if they are not there.  Ie maps "foo" -> foo.
     **/
    private String parseQuoted(final String s)
        throws ExtFileFormatException
    {
        if (s.length() < 3)
            throw extFileFormatException("name too short: " + s);
        else if (s.charAt(0) != '"')
            throw extFileFormatException("name does not start with \": "+ s);
        else if (s.charAt(s.length() - 1) != '"')
            throw extFileFormatException("name does not end with \": " + s);
        else // strip quotes
            return s.substring(1, s.length() - 1);
    }

    private HierName makeHierName(String name)
        throws ExtFileFormatException
    {
        try {
            // don't map / to .
            // name = StringUtil.replaceChars(name, PATH_SEPARATOR, '.');
            return HierName.makeHierName(name, '.');
        } catch (InvalidHierNameException e) {
            throw extFileFormatException("Invalid name: "
                    + e.getMessage(), e);
        }
    }

    /**
     * Strips quotes from the beginning and end of a  name, throwing an
     * exception if they are not there.  Ie maps "foo" -> foo.
     * @todo jmr XXX rename to to parseNodeName
     **/
    private HierName parseName(final String s)
        throws ExtFileFormatException
    {
        // strip quotes
        final String name = parseQuoted(s);

        // @design jmr move this into ExtCell, perhaps?
        return makeHierName(name);
    }

    /**
     * Parses a range specification for a use line of the form lo:hi:sep .
     **/
    private int[] parseUseRange(final String s)
        throws ExtFileFormatException
    {
        int[] a = new int[2];

        final int colon1Idx = s.indexOf(':');
        final int colon2Idx = s.indexOf(':', colon1Idx + 1);

        if (colon1Idx == -1 || colon2Idx == -1)
            throw extFileFormatException("Bad range spec: " + s);

        try {
            a[0] = Integer.parseInt(s.substring(0, colon1Idx));
            a[1] = Integer.parseInt(s.substring(colon1Idx + 1, colon2Idx));
            Integer.parseInt(s.substring(colon2Idx + 1));
        } catch (final NumberFormatException e) {
            throw extFileFormatException("Bad range spec, number format: "
                    + s, e);
        }

        return a;
    }

    /**
     * Parses the useID in a use line, returning an {@link ArraySpec}.  
     * useID may be of the form use or use[xlo:xhi:xsep][ylo:yhi:ysep].
     * Yes, lo:hi:sep, contrary to the lo,hi,sep indicated in ext(5).
     **/
    private ArraySpec parseUseSpec(final String useID)
        throws ExtFileFormatException
    {
        // find first [
        final int xOpen = useID.indexOf('[');

        if (xOpen == -1) {
            // there is no array
            return new ArraySpec(useID, "", 1, 1, false, 1, 1, false);
        } else {
            // find ]
            final int xClose = useID.indexOf(']', xOpen);

            try {
                // substring will throw IndexOutOfBoundsException
                // if an indexOf returns -1

                final int[] xRange
                    = parseUseRange(useID.substring(xOpen + 1, xClose));

                if (useID.charAt(xClose + 1) != '['
                    || useID.charAt(useID.length() - 1) != ']')
                    throw extFileFormatException("Bad useID: " + useID);

                final int[] yRange = parseUseRange(
                            useID.substring(xClose + 2, useID.length() - 1));

                return new ArraySpec(useID.substring(0, xOpen), "", 
                        xRange[0], xRange[1], xRange[0] != xRange[1],
                        yRange[0], yRange[1], yRange[0] != yRange[1]);
            } catch (final IndexOutOfBoundsException e) {
                throw extFileFormatException("Bad useID: " + useID, e);
            }
        }
    }

    /**
     * Parses the subcell array specifier component of a merge line,
     * returning an {@link ArraySpec}.
     * mergePath may be in one of the four forms:
     * <ul>
     *     <li> "" (the empty string), if it is a node in the cell
     *     <li> sub
     *     <li> sub[lo:hi]
     *     <li> sub[xlo:xhi,ylo:yhi]
     * </ul>
     **/
    private ArraySpec parseMergeSpec(final String mergePath)
        throws ExtFileFormatException
    {
        final int openBracket = mergePath.indexOf('[');

        if (openBracket == -1) {
            // no open bracket, make sure there is no close bracket
            if (mergePath.indexOf(']') != -1)
                throw extFileFormatException("Bad merge path: "
                        + mergePath);
            return new ArraySpec(mergePath, "", 1, 1, false, 1, 1, false);
        } else {
            // find ]
            final int closeBracket = mergePath.indexOf(']');

            // make sure ] is at end of string
            if (closeBracket != mergePath.length() - 1)
                throw extFileFormatException("Bad merge path: " + mergePath);

            try {
                // substring will throw IndexOutOfBoundsException
                // if an indexOf returns -1

                final String spec
                    = mergePath.substring(openBracket + 1, closeBracket);

                final int commaIdx = spec.indexOf(',');

                final boolean xIsArray;
                final int[] xRange;
                final int[] yRange;
                if (commaIdx == -1) {
                    // there is no comma, it is a 1-d spec
                    xIsArray = false;
                    xRange = new int[]{1, 1};
                    yRange = parseMergeRange(spec);
                } else {
                    // there is a comma, it is a 2-d spec
                    xIsArray = true;
                    xRange = parseMergeRange(spec.substring(0, commaIdx));
                    yRange = parseMergeRange(spec.substring(commaIdx + 1));
                }

                final String pre = mergePath.substring(0, openBracket);

                return new ArraySpec(pre, "",
                        xRange[0], xRange[1], xIsArray,
                        yRange[0], yRange[1], true);
            } catch (final IndexOutOfBoundsException e) {
                throw extFileFormatException("Bad merge path: " + mergePath, e);
            }
        }
    }

    /**
     * Parses ranges for array specifications.  The format is
     * <pre>
     *   range ::= number | number:number
     * </pre>
     **/
    private int[] parseMergeRange(final String s)
        throws ExtFileFormatException
    {
        // do we have a : ?
        final int colonIndex = s.indexOf(':');

        if (colonIndex == -1) {
            final int n = parseInt(s, "Bad single range: " + s);
            return new int[]{n,n};
        } else {
            final String startString = s.substring(0, colonIndex);
            final String endString = s.substring(colonIndex + 1);

            final int start = parseInt(startString,
                    "Bad range start: " + startString);
            final int end = parseInt(endString, "Bad range end: "
                    + endString);

            return new int[]{start, end};
        }
    }

    /**
     * Splits a path from a merge line into subcell and node path.
     * Splits a path of the form a/b/c/d into {a, /b/c/d}.
     * Splits a path of the form a into {"", a}
     **/
    public String[] splitMergePath(final String s) {
        final int slashIdx = s.indexOf('/');

        if (slashIdx == -1)
            return new String[]{"", s};
        else 
            return new String[]{
                s.substring(0, slashIdx),
                s.substring(slashIdx)};
    }

    /**
     * Parses a terminal for a fet line.
     **/
    private Terminal parseTerminal(final String[] words,
                                   final int startIndex,
                                   final String terminalName)
        throws ExtFileFormatException
    {
        final HierName name = parseName(words[startIndex]);
        final double length = parseLength(words[startIndex + 1],
                "bad gate length " + words[startIndex + 1] + " for "
                + terminalName);
        final String attributes = words[startIndex + 2];

        if (!"0".equals(attributes))
            throw extFileFormatException("Attributes unsupported: "
                    + words[startIndex + 2] + ", in " + terminalName);

        return new Terminal(name, length);
    }

    /**
     * Converts sparse array names and comma separated array names.
     * @review jmr XXX is this right?  what do we need to do for
     * node names vs. subcell names?
     **/
    public static String convertArrays(final String s) {

        // this name has array elements expanded into
        // foo[x][y].  We want this to be transformed 
        // into foo[x,y].  We also may have a sparse
        // array like foo(v)(w)[x,y].  This should
        // be transformed to foo[v,w,x,y]
        // So, we perform 
        // two steps:  
        // 1: change ( to [ and ) to ]
        // 2: change ][ to ,

        // @bug jmr XXX If we have parens in a non-sparse
        // array meaning, they will be mapped, too.
        // Is this ok?

        // step 1
        final StringBuffer noParens
            = StringUtil.replaceChars(new StringBuffer(s), "()", "[]");

        // step 2
        final String fixedUse
            = StringUtil.replaceSubstring(noParens, "][", ",").toString();

        return fixedUse;
    }

    /**
     * parses an ExtCell from the given file.  Searches using the extPath
     * search path.
     **/
    public void parseFile(final String fileName)
        throws ExtFileFormatException, IOException
    {
        // System.out.print( "File name in parseFile:" );
        // System.out.println( fileName );
        parseStream(extPath.openFileAsStream(fileName));
    }

    /**
     * Parses a .ext file.
     **/
    public void parseStream(final InputStream in)
        throws ExtFileFormatException, IOException
    {
        final BufferedReader
            br = new BufferedReader(new InputStreamReader(in));

        while ((line = br.readLine()) != null) {
            ++lineNumber;
            final String[] words = StringUtil.split(line, ' ');

            if (words.length == 0)
                throw extFileFormatException("Found empty line");

            if ("tech".equals(words[0])) {
                // tech techname
                ensureEnv();
                ensureNumWords(words, 2);

                technology = words[0];
            } else if ("timestamp".equals(words[0])) {
                // timestamp time
                ensureEnv();
                ensureNumWords(words, 2);

                if (timestamp != null)
                    throw extFileFormatException("Duplicate timestamp");

                // value in file is seconds since 1970, Date constructor
                // takes milliseconds
                timestamp = new Date(Long.parseLong(words[1]) * 1000);
            } else if ("version".equals(words[0])) {
                // version version
                ensureEnv();
                ensureNumWords(words, 2);

                if (version != null)
                    throw extFileFormatException("Duplicate versions");
                
                version = words[1];
            } else if ("style".equals(words[0])) {
                // style style
                ensureEnv();
                ensureNumWords(words, 2);

                if (style != null)
                    throw extFileFormatException("Duplicate styles");

                style = words[1];
            } else if ("scale".equals(words[0])) {
                // scale rscale cscale lscale
                ensureEnv();
                ensureNumWords(words, 4);

                if (scalesSet)
                    throw extFileFormatException("Duplicate scales");

                scalesSet = true;
                rscale = parseInt(words[1], "Bad rscale: " + words[1]);
                cscale = parseInt(words[2], "Bad cscale: " + words[2]);
                lscale = parseInt(words[3], "Bad lscale: " + words[3]);
            } else if ("resistclasses".equals(words[0])) {
                // resistclasses r1 r2 ...
                // however, we only care about the first two:
                // ndiff and pdiff
                ensureEnv();

                if (resistanceClasses.length != 0)
                    throw extFileFormatException("Duplicate resistclass");

                numResistClasses = words.length - 1;

                // there must be at least one resistclass
                if (numResistClasses < 1)
                    throw extFileFormatException(
                            "No resistance classes specified");

                resistanceClasses = new double[NUM_IMPORTANT_RESIST_CLASSES];

                for (int i = 0; i < resistanceClasses.length; ++i) {
                    resistanceClasses[i] = parseResistance(words[i + 1]);
                }
            } else if ("node".equals(words[0])) {
                // node name R C x y type a1 p1 a2 p2 ... aN pN
                //  0     1  2 3 4 5   6   7
                env = false;

                ensureNumWords(words, 7 + 2 * numResistClasses);

                final HierName name = parseName(words[1]);
                // lumped resistance is ignored, as it is useless
                final double r = parseResistance(words[2],
                        "Bad resistance: " + words[2]);
                final double c = parseCapacitance(words[3],
                        "Bad capacitance: " + words[3]);
                final double x = parseLength(words[4],
                        "Bad x: " + words[4]);
                final double y = parseLength(words[5],
                        "Bad y: " + words[5]);
                /* @todo jmr XXX replace this with a flag or something */
                final String type = words[6];

                final double[] rcas = parseAltAreas(words,
                        NUM_IMPORTANT_RESIST_CLASSES, 7, 
                        "Bad resistance class area");
                final double[] rcps = parseAltLengths(words,
                        NUM_IMPORTANT_RESIST_CLASSES, 8, 
                        "Bad resistance class perimeter");

                final Node protoNode = new Node(name, c, rcps, rcas);


                try {
                    ext.addData(name, protoNode);
                } catch (AliasedMap.MergeFailedException e) {
                    throw extFileFormatException("Internal error: "
                            + e.getMessage(), e);
                }

            } else if ("attr".equals(words[0])) {
                // attr name xl yl xh yh type text
                env = false;
                ensureNumWords(words, 8);
                ensureEnvSpecified(technology, timestamp, version, style,
                        resistanceClasses);

                throw extFileFormatException("attr unsupported");
            } else if ("equiv".equals(words[0])) {
                // equiv node1 node2
                env = false;
                ensureNumWords(words, 3);
                ensureEnvSpecified(technology, timestamp, version, style,
                        resistanceClasses);

                final HierName node1 = parseName(words[1]);
                final HierName node2 = parseName(words[2]);

                try {
                    ext.connectNames(node1, node2);
                } catch (AliasedMap.MergeFailedException e) {
                    throw extFileFormatException("Internal error: "
                            + e.getMessage(), e);
                }
            } else if ("fet".equals(words[0])) {
                // fet type xl yl xh yh area perim sub GATE   T1   T2
                //  0    1  2  3  4  5    6    7    8  9-11 12-14 15-17
                // or
                // fet type xl yl xh yh area perim sub GATE   T1
                //  0    1  2  3  4  5    6    7    8  9-11 12-14
                env = false;

                // fet lines will have 15 or 18 words, depending on whether
                // or not it is degenerate.
                // If it is degenerate, make source = drain.
                if (words.length != 15 && words.length != 18)
                    throw extFileFormatException(
                            "Bad token count, expected 15 or 18, got "
                            + words.length);

                ensureEnvSpecified(technology, timestamp, version, style,
                        resistanceClasses);

                final int type;
                if ("pfet".equals(words[1]))
                    type = DeviceTypes.P_TYPE;
                else if ("nfet".equals(words[1]))
                    type = DeviceTypes.N_TYPE;
                else
                    throw extFileFormatException("Bad fet type: "
                            + words[1]);

                // ignore xl, yl, xh, yh

                final double area = parseArea(words[6], "bad area: "
                        + words[6]);
                final double perim = parseLength(words[7], "bad perim: "
                        + words[7]);
                final HierName sub = parseName(words[8]);

                final Terminal gate = parseTerminal(words, 9, "gate");
                final Terminal source = parseTerminal(words, 12, "source");
                final Terminal drain;

                // for degenerate fets, make drain = source
                // otherwise, parse drain
                if (words.length == 15)
                    drain = source;
                else
                    drain = parseTerminal(words, 15, "drain");

                final FET fet = new FET(type,
                        area, perim, sub, gate, source, drain);

                ext.addFET(fet);
            } else if ("killnode".equals(words[0])) {
                // killnode node
                env = false;
                ensureNumWords(words, 2);
                ensureEnvSpecified(technology, timestamp, version, style,
                        resistanceClasses);

                // XXX support soon
                throw extFileFormatException("killnode unsupported");
            } else if ("resist".equals(words[0])) {
                // resist node1 node2 R
                env = false;
                ensureNumWords(words, 4);
                ensureEnvSpecified(technology, timestamp, version, style,
                        resistanceClasses);

                final String node1 = words[1];
                final String node2 = words[2];
                final double r = parseResistance(words[3],
                        "Bad resistance: " + words[3]);

                Debug.assertTrue(false);
                
                /** @bug XXX **/

                // pairToResistanceMap.put(new AliasPair(node1, node2),
                        // new Double(r));
            } else if ("distance".equals(words[0])) {
                // distance name1 name2 dmin dmax
                env = false;
                ensureNumWords(words, 5);
                ensureEnvSpecified(technology, timestamp, version, style,
                        resistanceClasses);

                throw extFileFormatException("distance unsupported");
            } else if ("use".equals(words[0])) {
                // use def use-id ta tb tc td te tf
                //  0   1     2    3 4  5  6  7  8
                env = false;
                ensureNumWords(words, 9);
                ensureEnvSpecified(technology, timestamp, version, style,
                        resistanceClasses);

                final String def = words[1];
                final String useID = words[2];

                // parse the useID, which may be of the form use or
                // use[xlo,xhi,xsep_centimicron][ylo,yhi,ysep_centimicron]
                final ArraySpec arraySpec = parseUseSpec(useID);

		if ( m_RecurseOnUseStatements ) {
		    // if not parsed, parse the subcell definition
		    ExtCell subExt = (ExtCell) subcellNameToDefnMap.get(def);
		    
		    if (subExt == null) {
			final ExtParser p
			    = new ExtParser(def, extPath, subcellNameToDefnMap, true);
			p.parseFile(def + ".ext");
			subExt = p.getExtCell();
			// parsing will add it to subcellNameToDefnMap
		    }

		    final ExtCell e = subExt;
		    arraySpec.mapNames(new UnaryAction() {
			    public void execute(final Object o) {
				final String use = convertArrays((String) o);
				
				// add the cell to the list of used cells
				ext.addSubcell(use, e);
			    }
			});
		}

            } else if ("merge".equals(words[0])) {
                // merge path1 path2 C a1 p1 a2 p2  ... aN pN
                //   0     1     2   3 
                // or
                // merge path1 path2
                //   0     1     2
                env = false;

                if (words.length != 3
                        && words.length != (4 + 2 * numResistClasses))
                    throw extFileFormatException("Bad token count for merge");
                ensureEnvSpecified(technology, timestamp, version, style,
                        resistanceClasses);

                // @bug jmr XXX need to handle global !
                final String path1 = parseQuoted(words[1]);
                final String path2 = parseQuoted(words[2]);

                final double c;
                final double[] rcas;
                final double[] rcps;

                if (words.length == 3) {
                    // If there are no merge info given, make all 0's

                    c = 0.0;

                    // share the same array
                    rcas = new double[resistanceClasses.length];
                    rcps = rcas;

                    for (int i = 0; i < rcas.length; ++i)
                        rcas[i] = 0.0;
                } else {
                    c = parseCapacitance(words[3], "Bad capacitance: "
                            + words[3]);

                    rcas = parseAltAreas(words,
                            NUM_IMPORTANT_RESIST_CLASSES, 4,
                            "Bad merge area");
                    rcps = parseAltLengths(words,
                            NUM_IMPORTANT_RESIST_CLASSES, 5,
                            "Bad merge perimeter");
                }

                // split paths into subcellSpec and path
                final String[] p1 = splitMergePath(path1);
                final String[] p2 = splitMergePath(path2);

                final ArraySpec path1Spec = parseMergeSpec(p1[0]);
                final ArraySpec path2Spec = parseMergeSpec(p2[0]);

                // verify that the arrays are of compatible sizes
                // this may not correctly handle merge of a 1-d array
                // and a 2-d array. What is the spec for that?
                final int nx1 = path1Spec.getXSize();
                final int nx2 = path2Spec.getXSize();
                final int ny1 = path1Spec.getYSize();
                final int ny2 = path2Spec.getYSize();

                if (nx1 != nx2 || ny1 != ny2) {
                    throw extFileFormatException(
                            "Array sizes do not agree");
                }

                // make a node for the merged nodes, give the node
                // two names for the connected labels, regardless
                // of whether they are sub/a b or sub1/a sub2/b

                for (int ix = 0; ix < nx1; ++ix)
                    for (int iy = 0; iy < ny1; ++iy) {
                        final HierName name1 = makeHierName(
                            convertArrays(path1Spec.genName(
                                path1Spec.getXLo() + ix,
                                path1Spec.getYLo() + iy)) + p1[1]);
                        final HierName name2 = makeHierName(
                            convertArrays(path2Spec.genName(
                                path2Spec.getXLo() + ix,
                                path2Spec.getYLo() + iy)) + p2[1]);

                        final Node node = new Node(name1, c, rcps, rcas);

                        try {
                            /*
                            if (c != 0.0
                                    || rcps[0] != 0.0 || rcps[1] != 0.0
                                    || rcas[0] != 0.0 || rcas[1] != 0.0)
                            */
                            ext.addData(name1, node);
                            ext.connectNames(name1, name2);
                        } catch (AliasedMap.MergeFailedException e) {
                            throw extFileFormatException(
                                    "Internal error: " + e.getMessage(), e);
                        }
                    }

            } else if ("cap".equals(words[0])) {
                // cap node1 node2 C
                env = false;
                ensureNumWords(words, 4);
                ensureEnvSpecified(technology, timestamp, version, style,
                        resistanceClasses);

                final HierName node1 = parseName(words[1]);
                final HierName node2 = parseName(words[2]);
                final double c = parseCapacitance(words[3],
                        "Bad capacitance: " + words[3]);

                ext.addCapacitor(new Capacitor(node1, node2, c));
            } else {
                throw extFileFormatException("Unrecognized directive: "
                        + words[0]);
            }

        }

        // XXX: check to make sure that hierarchical names referenced
        // are valid, ie that any subcells are actually there

        // XXX: many more validity checks

        // now that the file is parsed, add it to the global repository
        subcellNameToDefnMap.put(ext.getName(), ext);

    }

    /**
     * Retrieve the parsed ExtCell.
     **/
    public ExtCell getExtCell() {
        return ext;
    }

    /**
     * Parses the file cellName.ext into an ExtCell.
     **/
    public static ExtCell parse(final String cellName,
                                final FileSearchPath extPath,
				boolean RecurseOnUseStatements )
        throws ExtFileFormatException, IOException
    {
        final ExtParser p = new ExtParser(cellName, extPath, RecurseOnUseStatements);
	System.out.print( "Cell name in parse:" );
	System.out.println( cellName );
        p.parseFile(cellName + ".ext");
        return p.getExtCell();
    }

    public static ExtCell parse( final String cellName,
				 final String extFileName,
				 final FileSearchPath extPath,
				 boolean RecurseOnUseStatements )
	throws ExtFileFormatException, IOException
    {
	final ExtParser p = new ExtParser( cellName, extPath, RecurseOnUseStatements );
	p.parseFile( extFileName );
	return p.getExtCell();
    }
    
}
