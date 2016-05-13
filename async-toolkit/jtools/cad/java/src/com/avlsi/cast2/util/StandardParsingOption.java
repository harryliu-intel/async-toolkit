package com.avlsi.cast2.util;

import java.lang.ref.WeakReference;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cast2.impl.CastTwoParser;
import com.avlsi.cast2.impl.CastTwoTreeParser;
import com.avlsi.cell.CellInterface;
import com.avlsi.cell.CellUtils;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.text.StringUtil;

/**
 * Handle overriding of variables via --define arguments specified on the
 * commandline.
 **/
public class StandardParsingOption implements CastParsingOption {
    /**
     * The regular expression for the right hand-side of a valid --define
     * argument.
     **/
    private final static Pattern OVERRIDE =
        Pattern.compile("(.*)\\.([^\\.]+):(.*)");

    /**
     * Map from fully qualified cell name to a map from variable name to
     * value.
     **/
    private final Map/*<String,Map<String,String>>*/ vars;

    /**
     * A stack containing currently active maps from variable name to value.
     * The top of the stack contains the map that should be used to look for
     * overriden values.
     **/
    private final LinkedList/*<Map<String,String>>*/ mapStack;

    /**
     * A tree parser instance used to parse the value.  The instance is created
     * lazily, when needed.
     **/
    private CastTwoTreeParser castTreeParser;

    /**
     * Enable hack for compatibility for bug 3771?
     **/
    private final boolean hackBug3771;

    /**
     * Enable hack for compatibility for bug 7068?
     **/
    private final boolean hackBug7068;

    /**
     * Enable hack for compatibility for bug 16459?  If set, do not require
     * strict, linear subtyping.
     **/
    private final boolean hackBug16459;

    /**
     * Enable to not refine standard.channel.bdc from asynchronous_channel
     **/
    private final boolean hackOrphanBdc;

    /**
     * Whether to check ports are connected properly between subcells.
     **/
    private final boolean checkConnections;
    private boolean checkE1ofN = false;

    /**
     * Weak reference to a Cadencize object used to check connections between
     * subcells.
     **/
    private WeakReference<Cadencize> cadRef;

    public StandardParsingOption(final CommandLineArgs args) {
        vars = new HashMap();
        mapStack = new LinkedList();
        // handle variables not declared inside a cell, e.g., file scope
        // variables
        mapStack.add(Collections.EMPTY_MAP);

        for (CommandLineArgsIterator i = args.iterator(); i.hasNext(); ) {
            final CommandLineArg arg = (CommandLineArg) i.next();
            if (arg.getName().equals("define")) {
                final String val = arg.getValue();
                final Matcher m = OVERRIDE.matcher(val);
                if (m.matches()) {
                    final String cell = m.group(1);
                    Map cellMap = (Map) vars.get(cell);
                    if (cellMap == null) {
                        cellMap = new HashMap();
                        vars.put(cell, cellMap);
                    }
                    cellMap.put(m.group(2), m.group(3));
                } else {
                    System.err.println("WARNING: invalid --define argument: " +
                                       val);
                }
            }
        }

        hackBug3771 = args.argExists("enable-bug3771-compatibility");
        hackBug7068 = args.argExists("enable-bug7068-compatibility");
        hackBug16459 = args.argExists("enable-bug16459-compatibility");
        hackOrphanBdc = args.argExists("enable-orphan-bdc");
        checkConnections = args.argExists("check-connections");
        if (checkConnections) {
            final String s = args.getArgValue("check-connections", "");
            final Set<String> types =
                new HashSet<String>(Arrays.asList(StringUtil.split(s, ',')));
            checkE1ofN = types.contains("e1ofN");
        }
    }

    public boolean processInline(final CellInterface cell) {
        return true;
    }

    public void beginCellConstruction(final CellInterface cell) {
        final Map currentMap = (Map) vars.get(cell.getFullyQualifiedType());
        mapStack.addFirst(currentMap == null ? Collections.EMPTY_MAP
                                             : currentMap);
    }

    public Value getOverrideValue(final CellInterface cell, final Symbol var,
                                  final Value init, final Environment env) {
        final Map currentMap = (Map) mapStack.getFirst();
        final String val = (String) currentMap.remove(var.getString());
        if (val == null) return null;
        try {
            final CastTwoParser castParser =
                CastTwoParser.getParser(val, 0, 0, "<variable override>");
            castParser.startExpression();

            if (castTreeParser == null) {
                castTreeParser = new CastTwoTreeParser();
            }
            final Value v = castTreeParser.expression(castParser.getAST(), env,
                                                      false);

            return v;
        } catch (Exception e) {
            System.err.println("WARNING: Cannot parse overriden value " +
                               cell.getFullyQualifiedType() + "." +
                               var.getString() + ":" + val);
            return null;
        }
    }

    public void endCellConstruction(final CellInterface cell) {
        final Map currentMap = (Map) mapStack.removeFirst();
        if (!currentMap.isEmpty()) {
            System.err.print("WARNING: " + cell.getFullyQualifiedType() +
                             " does not define the following variables to be" +
                             " overridden:");
            for (Iterator i = currentMap.keySet().iterator(); i.hasNext(); ) {
                System.err.print(" " + i.next());
            }
            System.err.println();
        }
        if (checkConnections) {
            Cadencize cad = cadRef == null ? null : cadRef.get();
            if (cad == null) {
                cad = new Cadencize(false);
                cadRef = new WeakReference<Cadencize>(cad);
            }
            CellUtils.verifySubcells(cell, cad, checkE1ofN);
        }
    }

    public boolean bug3771HackEnabled() {
        return hackBug3771;
    }

    public boolean bug7068HackEnabled() {
        return hackBug7068;
    }

    public boolean bug16459HackEnabled() {
        return hackBug16459;
    }

    public boolean orphanBdcHackEnabled() {
        return hackOrphanBdc;
    }
}
