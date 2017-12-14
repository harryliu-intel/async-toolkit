/*
 *      PropertyReader.java - load and check configuration properties
 *
 *      Copyright 2002 Fulcrum Microsystems, Inc.  All rights reserved.
 *
 *      $Id$
 */

package com.fulcrummicro.util.properties;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.nfunk.jep.JEP;

import com.avlsi.util.container.CollectionUtils;
import com.fulcrummicro.util.cmdline.PropertyArg;
import com.fulcrummicro.util.misc.JepMathExpressionParser;
import com.fulcrummicro.util.misc.PropValueList;
import com.fulcrummicro.util.xml.XmlPropFileHandler;
import com.fulcrummicro.util.xml.XmlUtils;

/**
 * PropReader - class to convert an XML option description
 *                  file to /namerarchical map of maps
 *
 * Based off of PropertyManager, but designed to be generic
 *
 * allowed in names: a-zA-Z0-9_.
 * currently used special characters: $(){}[]%:
 *   loop              [A-B]
 *   subst             $(S)
 *   math              ${M} or $%M%
 *   nested variants   a:b
 * all others are reserved and should not be used
 *
 * @author Naru Sundar
 */
public class PropReader {
    private boolean includeDefaults = true;

    /* list of prefixes we are allowed to add that don't have defaults */
    private final ArrayList<String> allowedPrefixes = new ArrayList<String>();

    /* values entered on the commandline and via config files */
    private Map<String,String> cmdlineValues =
        new LinkedHashMap<String,String>();

    /* contains all the properties before they are hiearchified */
    private Map<String, PropConfig> configs = new HashMap<String, PropConfig>();
    private Map<String, Property> defaults = new HashMap<String, Property>();
    private PropValueList values = new PropValueList();
    private Map<String, String> finalValues = new HashMap<String, String>();

    /* the un-hiearchified property space (fully expanded) */
    private Map<String,Object> expandedValues =
        new LinkedHashMap<String,Object>();

    /* the regular expression defining expansion */
    private final Pattern loopPat = Pattern.compile("\\[(\\d+)-(\\d+)\\]");  // [start-end]
    // ${MATH} or $%MATH%
    private final Pattern mathPat = Pattern.compile("(?:" + "\\$\\{([^{}]+)\\}" + ")|(?:" + "\\$%(.+?)%" + ")");

    public static final String VARIANT_SEPERATOR = ":";
    public static final String TEST_CONFIG = "__TEST_CONFIG__";

    private boolean debug = false;
    private boolean debugVerbose = debug && true;
    private boolean suppressErrors = false;

    /* basic constructor to initialize stuff */
    public PropReader() {
        PropConfig pc = new PropConfig();
        pc.addOpt("regress.qsub_mem", new Property("Integer", "0", true, ','));
        pc.addOpt("models.qsub_mem", new Property("Integer", "0", true, ','));
        pc.addOpt("regress.test_feature", new Property("String", "", true, ','));
        pc.addOpt("testbench.tchk.scope", new Property("String", "", true, ','));
        configs.put(TEST_CONFIG, pc);
    }

    protected PropReader clone() {
        PropReader clone = new PropReader();
        clone.includeDefaults = includeDefaults;
        clone.allowedPrefixes.addAll(allowedPrefixes);
        clone.cmdlineValues.putAll(cmdlineValues);
        for (String config : configs.keySet()) {
            clone.configs.put(config, configs.get(config).clone());
        }
        clone.defaults.putAll(defaults);
        for (String key : values.keySetOrdered()) {
            clone.values.put(key, values.get(key));
        }
        clone.finalValues.putAll(finalValues);
        clone.debug = debug;
        clone.debugVerbose = false;
        clone.suppressErrors = true;
        return clone;
    }

    protected void errorPrint(String str) {
        if(!suppressErrors) {
            if(debug) System.err.println("ERROR: " + str);
            System.out.println("ERROR: " + str.replace('\n',' '));
        }
    }

    protected void debugPrint(String str) {
        if(debug) {
            System.err.println(str);
            if(debugVerbose) {
                for(String key : cmdlineValues.keySet()) {
                    System.err.println("  c " + key + " = " + cmdlineValues.get(key));
                }
                System.err.println("Configs");
                for(String config : configs.keySet()) {
                    System.err.println("  config: " + config);
                    PropConfig pc = configs.get(config);
                    for (String p : pc.getUses()) {
                        System.err.println("    uses: " + p);
                    }
                    System.err.println("    opts:");
                    Map<String, Property> opts = pc.getOpts();
                    for (String key : opts.keySet()) {
                        Property prop = opts.get(key);
                        System.err.println("      type: " + prop.type + ", name: " + key + ", value: " + prop.value);
                    }
                    System.err.println("    sets:");
                    for (PropSetVal set : pc.getSets()) {
                        System.err.println("      name: " + set.name + ", value: " + set.value);
                    }

                }
                for(String key : expandedValues.keySet()) {
                    System.err.println("  e " + key + " = " + expandedValues.get(key));
                }
            }
        }
    }

    /**
     * Reads defaults from a given XML file, appends values
     * from the commandline (and any commandline referenced
     * config files) and converts the resultant maps to one
     * hierarchic property map
     *
     * @param  args                 command line arguments
     * @param  includeDefaults      true means include the defaults
     *
     * @return ArrayList            list of arguments that were not
     *                              parsed (caller handles this)
     * @throws BadPropertyException with a human-readable error message
     *                              if anything goes wrong in parsing
     */
    public ArrayList<String> loadProperties(String[] args,
                                       boolean includeDefaults) {
        this.includeDefaults = includeDefaults;

        boolean defaultsLoaded = false;

        /* override values with commandline/config files */
        PropCommandLineParser clp = new PropCommandLineParser(configs,
                allowedPrefixes, args);

        // Grab additional xml files to preprocesses as bad arguments
        ArrayList<String> badArgs = clp.parseCommandLine();
        // Pre-apply all special properties necessary for including XML files.
        for (String property : new String[]{"topdir", "platform"}) {
            configs.get(TEST_CONFIG).addOpt(property, new Property("String", ""));
            String value = clp.searchPropList(property);
            if (value != null) {
                cmdlineValues.put(property, value);
            } else {
                System.out.println("warning: \"" + property + "\" not found");
            }
        }
        // Load a default xml file specified on the command line of the form --(file).xml
        for(int i = 0; i < badArgs.size(); i++) {
            String arg = badArgs.get(i);
            if(!arg.startsWith("--") && arg.endsWith(".xml")) {
                loadDefaults(arg);
                defaultsLoaded = true;
            }
        }
        if(!defaultsLoaded)
            throw new ExpansionErrorException("no defaults xml file found on commandline");
        // Reapply cmd line
        for (String key : cmdlineValues.keySet()) {
            configs.get(TEST_CONFIG).addSet(key, cmdlineValues.get(key));
        }
        clp.evaluateConfigFiles();

        preProcessing(clp);
        processProperties();
        postProcessProperties();
        return badArgs;
    }

    protected void preProcessing(PropCommandLineParser clp) {
        // Add all defaults and sets from configs
        preProcessDefaults(null, TEST_CONFIG);
        // Add set imperatives to the defaults
        preProcessProperties(null, TEST_CONFIG);
        // Add variant configurations from command line
        preProcessCmdLine(clp, true);
        // Add variable modifications from command line
        preProcessCmdLine(clp, false);
    }

    protected void processProperties() {
        processVarValueSubs();
        processSpecialCases();
        processMathExpressions();
        processVarNameSubs();
        processExpansions();
    }

    protected void preProcessDefaults(String parent, String config) {
        String full;
        if (parent != null) {
            full = parent + ":" + config;
        } else {
            full = config;
        }
        if (debug) {
            System.err.println("preProcessDefaults(" + full + ")");
        }
        // Check if this is multi-level and process top first, since inside can override outside
        int splitPoint = config.lastIndexOf(VARIANT_SEPERATOR);
        if (splitPoint != -1) {
            String start = config.substring(0, splitPoint);
            String end = config.substring(splitPoint+1);
            preProcessDefaults(null, start);
            preProcessDefaults(start, end);
            return;
        }
        if (!configs.containsKey(full)) {
            System.out.println("ERROR: Config/Variant block does not exist: " + full);
        } else {
            debugPrint("preProcessDefaults(" + config + ") starting processing");
            for (String uses : configs.get(full).getUses()) {
                preProcessDefaults(null, uses);
            }
            Map<String, Property> opts = configs.get(full).getOpts();
            defaults.putAll(opts);
            for (String set : opts.keySet()) {
                values.put(set, opts.get(set).value);
            }
            debugPrint("preProcessDefaults(" + full + ") complete");
        }
    }

    protected void preProcessProperties(String parent, String config) {
        String full;
        if (parent != null) {
            full = parent + ":" + config;
        } else {
            full = config;
        }
        if (debug) {
            System.err.println("preProcessProperties(" + full + ")");
        }
        // Check if this is multi-level and process top first, since inside can override outside
        int splitPoint = config.lastIndexOf(VARIANT_SEPERATOR);
        if (splitPoint != -1) {
            String start = config.substring(0, splitPoint);
            String end = config.substring(splitPoint+1);
            preProcessProperties(null, start);
            preProcessProperties(start, end);
            return;
        }
        if (configs.containsKey(full)) {
            debugPrint("preProcessProperites(" + full + ") starting processing");
            for (String uses : configs.get(full).getUses()) {
                preProcessProperties(null, uses);
            }
            for (PropSetVal set : configs.get(full).getSets()) {
                handleProperty(set);
            }
            debugPrint("preProcessProperties(" + full + ") complete");
        }
    }

    protected void handleProperty(PropSetVal set) {
        if (defaults.containsKey(set.name)) {
            if (defaults.get(set.name).type.equals("String")) {
                handleStringProp(set);
            } else if (defaults.get(set.name).type.equals("Integer")) {
                handleIntegerProp(set);
            } else {
                values.put(set.name, set.value);
            }
        } else {
            values.put(set.name, set.value);
        }
    }

    protected void handleStringProp(PropSetVal set) {
        Property prop = defaults.get(set.name);
        if (prop.concat && values.get(set.name).length() > 0) {
            String newVal = values.get(set.name);
            // Find any earlier instances an remove the duplicate
            if (!set.name.equals("cosim.exceptions")) {
                String [] vals = set.value.split(Pattern.quote(Character.toString(prop.concat_op)));
                if (vals.length == 1) {
                    newVal = newVal.replace(vals[0], "");
                } else {
                    for (String val : vals) {
                        newVal = newVal.replace(val + prop.concat_op, "");
                    }
                }
            }
            values.put(set.name, newVal + prop.concat_op + set.value);
        } else {
            values.put(set.name, set.value);
        }
    }

    protected void handleIntegerProp(PropSetVal set) {
        if (defaults.get(set.name).concat) {
            values.put(set.name, Integer.toString(
                    Integer.parseInt(values.get(set.name)) + Integer.parseInt(set.value)));
        } else {
            values.put(set.name, set.value);
        }
    }

    protected void preProcessCmdLine(PropCommandLineParser clp, boolean variant) {
        if (debug) {
            System.err.println("preProcessCmdLineVariants");
        }
        debugPrint("preProcessCmdLine(" + variant + ") starting processing");
        for (PropertyArg arg : clp.getPlist()) {
            String name = arg.getName();
            String value = arg.getValue();
            if (variant) {
                if (name.equalsIgnoreCase("variant")) {
                    for (String val : value.split(",")) {
                        preProcessDefaults(null, val);
                        preProcessProperties(null, val);
                    }
                }
            } else {
                if (!name.equalsIgnoreCase("variant")) {
                    handleProperty(new PropSetVal(name, value));
                }
            }
        }
        debugPrint("preProcessCmdLine(" + variant + ") complete");
    }

    protected void processVarNameSubs() {
        PropReader clone = this.clone();
        String substPat = "\\$\\([\\w._]+\\)";
        Pattern pat = Pattern.compile(String.format("(%s)", substPat));
        // Substitute variables in key space
        for (String key : clone.values.keySetOrdered()) {
            String newKey = new String(key);
            String value = clone.values.get(key);
            boolean done;
            boolean update = false;
            do {
                done = true;
                Matcher m = pat.matcher(newKey);
                if (m.find()) {
                    done = false;
                    update = true;
                    if(debug) {
                        System.err.println("  processSubstitutions: found " + newKey + " : " + m.group(1));
                    }
                    SubstExpr s = new SubstExpr(m.group(1));
                    s.evaluateSubst();
                    String result = clone.values.get(s.match());
                    if (result == null) {
                        System.err.println("Cannot find key " + s.match() + " in keys");
                    }
                    newKey = newKey.substring(0, m.start(1)) + result + newKey.substring(m.end(1));
                }
            } while (!done);
            if (update) {
                values.put(key, newKey, value);
                if (defaults.containsKey(key)) {
                    defaults.remove(key);
                    defaults.put(newKey, new Property(clone.defaults.get(key).type, value));
                }
            }
        }
    }

    protected void processVarValueSubs() {
        PropReader clone = this.clone();
        String substPat = "\\$\\([\\w\\-._]+\\)";
        Pattern pat = Pattern.compile(String.format("(%s)", substPat));
        // Substitute variables in value space
        for (String key : clone.values.keySetOrdered()) {
            String value = new String(clone.values.get(key));
            boolean done;
            boolean update = false;
            do {
                done = true;
                Matcher m = pat.matcher(value);
                if (m.find()) {
                    done = false;
                    update = true;
                    if(debug) {
                        System.err.println("  processSubstitutions: found " + key + " : " + m.group(1));
                    }
                    SubstExpr s = new SubstExpr(m.group(1));
                    s.evaluateSubst();
                    String result = clone.values.get(s.match());
                    if (result == null) {
                        System.err.println("Cannot find key " + s.match() + " in keys");
                    }
                    value = value.substring(0, m.start(1)) + result + value.substring(m.end(1));
                }
            } while (!done);
            if (update) {
                values.put(key, value);
            }
        }
    }

    protected void processMathExpressions() {
        PropReader clone = this.clone();
        JEP parser = new JepMathExpressionParser();

        for (String key : clone.values.keySetOrdered()) {
            String value = new String(clone.values.get(key));
            boolean update = false;
            boolean done;
            do {
                done = true;
                Matcher m = mathPat.matcher(value);
                if(m.find()) {
                    done = false;
                    String mathExp = (m.group(1) != null) ? m.group(1) : m.group(2);
                    if(debugVerbose) System.err.println("    Found math exp " + mathExp + " in " + value);
                    if(parser.parseExpression(mathExp) != null) {
                        update = true;
                        Double result = parser.getValue();
                        String resultStr;
                        if(result.isNaN()) {
                            resultStr = parser.getValueAsObject().toString();
                        } else {
                            if(Math.floor(result) == result.doubleValue()) {
                                resultStr = Integer.toString(result.intValue());
                            } else {
                                resultStr = result.toString();
                            }
                        }
                        value = value.substring(0, m.start(0)) + resultStr + value.substring(m.end(0));

                        if(debugVerbose) System.err.println("    Replaced " + key + " with " + value);
                    } else {
                        errorPrint("Failure to parse math expression " + value + " in key " + key + ": " + parser.getErrorInfo());
                        break;
                    }
                }
            } while(!done);
            if (update) {
                values.put(key, value);
            }
        }
    }

    boolean expandRange(String key, String value, boolean vars) {
        boolean subst = false;
        Matcher m = loopPat.matcher(key);

        if(m.find()) {
            subst = true;
            int lb, ub;
            try {
                lb = Integer.parseInt(m.group(1));
                ub = Integer.parseInt(m.group(2));
            } catch(NumberFormatException e) {
                /* get rid of bad key */
                values.remove(key);
                throw new ExpansionErrorException("invalid indices " +
                        m.group(1) + "-" +
                        m.group(2) +
                        " for key " + key);
            }
            if(lb > ub) {
                throw new ExpansionErrorException("invalid indices " +
                        m.group(1) + "-" +
                        m.group(2) +
                        " for key " + key);
            }
            if (vars) {
                for(int i = lb; i <= ub; i++) {
                    String newKey = key.substring(0, m.start(1) - 1) + i +
                            key.substring(m.end(2) + 1);
                    if (!expandRange(newKey, value, true)) {
                        if (defaults.containsKey(newKey)) {
                            finalValues.put(newKey, value);
                        } else {
                            System.err.println("ERROR: Could not find expanded key in defaults: " + newKey);
                        }
                    }
                }
                finalValues.remove(key);
            } else {
                /* remove old key */
                String type = defaults.get(key).type;
                // values.remove(key);
                if (defaults.containsKey(key)) {
                    defaults.remove(key);
                }
                /* add new keys and expand them as we add them */
                for(int i = lb; i <= ub; i++) {
                    String newKey = key.substring(0, m.start(1) - 1) + i +
                            key.substring(m.end(2) + 1);
                    finalValues.put(newKey, value);
                    if (defaults.containsKey(newKey)) {
                        System.err.println("ERROR: Duplicate opt declarations found for " + newKey);
                    } else {
                        defaults.put(newKey, new Property(type, value));
                        expandRange(newKey, value, false);
                    }
                }
            }
        }
        return subst;
    }

    protected void processExpansions() {
        PropReader clone = this.clone();
        // Remove it all so that we can refill it with the expanded list
        //values.clear();
        // Expand lists from defaults
        for (String key : values.keySetOrdered()) {
            if (defaults.containsKey(key)) {
                if (!expandRange(key, clone.values.get(key), false)) {
                    finalValues.put(key, clone.values.get(key));
                }
            } else {
                finalValues.put(key, clone.values.get(key));
            }
        }
        // Expand the rest of the sets as all of the defaults should exist
        for (String key : clone.values.keySetOrdered()) {
            if (!expandRange(key, clone.values.get(key), true)) {
                finalValues.put(key, clone.values.get(key));
            }
        }
    }

    protected void addDefaultValues() {
        Set<String> keys = new HashSet<String>();
        Map<String, Property> defs = configs.get(TEST_CONFIG).getOpts();
        keys.addAll(defs.keySet()); // make a copy since expandLoopsInKeys will modify defaults
        Iterator<String> iter = keys.iterator();

        for (String key : defs.keySet()) {
            Property prop = defs.get(key);
            if(debug) System.err.println("    addDefaultValues " + key + ", " + prop.value);

            if(!values.containsKey(key)) {
                values.put(key, prop.value);
                defaults.put(key, prop);
            }
        }
    }

    protected void loadDefaults(String defaultOptFile) {
        ArrayList<String> nfiles = new ArrayList<String>();

        /* start with first file */
        nfiles.add(defaultOptFile);

        do {
            String nextFile = null;
            InputStream istr = null;

            /* continue while there are files to parse */
            if(nfiles.size() > 0) {
                String s = nfiles.remove(0);

                /* do any preliminary parsing */
                if(canExpandString(s)) {
                    try {
                        /* first check the commandline values */
                        nextFile = expandValue(s, cmdlineValues);
                    } catch(ExpansionErrorException e1) {
                        try {
                            /* try evaluating from the defaults */
                            nextFile = configs.get(TEST_CONFIG).expandValue(s);
                        } catch(ExpansionErrorException e2) {
                            System.out.println("ERROR: " + e2.getMessage());
                            return;
                        }
                    }
                } else {
                    nextFile = s;
                }
            } else break;

            try {
                istr = new FileInputStream(nextFile);
            } catch(IOException e) {
                System.out.println("ERROR: " + e.getMessage());

                return;
            }

            try {
                XmlUtils.parseXml(istr,
                                  new XmlPropFileHandler(configs,
                                                         nfiles,
                                                         allowedPrefixes));
            } catch(BadPropertyException e) {
                System.out.println("ERROR: unable to parse " +
                                   nextFile + ": " + e.getMessage());
            }
        } while(nfiles.size() > 0);
    }

    protected boolean canExpandString(String s) {
        return new SubstExpr(s).canSubst();
    }

    protected boolean canExpandLoops(String s) {
        return loopPat.matcher(s).find();
    }

    public String expandKey(String key, Map<String,String> values)
        throws ExpansionErrorException, MalformedSubstException {

        String evaluatedKey = new String(key);
        SubstExpr m = new SubstExpr(evaluatedKey);

        while(m.canSubst()) {
            String newKey = m.evaluateSubst();
            String expandedKey = expandKey(newKey, values);
            String expandedValue = values.get(expandedKey);

            if(expandedValue == null)
                throw new ExpansionErrorException("Unable to resolve key " + expandedKey);

            /* expand the subkey and evaluate it */
            evaluatedKey = evaluatedKey.substring(0, m.start()) +
                           expandedValue +
                           evaluatedKey.substring(m.end());

            m = new SubstExpr(evaluatedKey);
        }

        return evaluatedKey;
    }

    public String expandValue(String value, Map<String,String> values)
        throws ExpansionErrorException, MalformedSubstException {

        String expandedValue = new String(value);
        SubstExpr m = new SubstExpr(expandedValue);

        while(m.canSubst()) {
            String key = m.evaluateSubst();

            String expandedKey = expandKey(key, values);

            if(values.get(expandedKey) == null)
                throw new ExpansionErrorException("Unable to resolve key " + expandedKey);

            if(values.containsKey(expandedKey)) {
                /* expand the subkey and evaluate it */
                expandedValue = expandedValue.substring(0, m.start()) +
                                values.get(expandedKey) +
                                expandedValue.substring(m.end());
            } else {
                /* don't throw an error if the key can be expanded further */
                if(!(new SubstExpr(expandedKey)).canSubst())
                    throw new ExpansionErrorException("cannot evaluate key " +
                                                      expandedKey);
            }
            m = new SubstExpr(expandedValue);
        }
        return expandedValue;
    }

    protected boolean prefixIsAllowed(String key) {
        for(int i = 0; i < allowedPrefixes.size(); i++) {
            if(key.indexOf(allowedPrefixes.get(i)) == 0) {
                return true;
            }
        }
        return false;
    }

    protected Object guessTypedValue(String value) {
        Object cvt = null;
        boolean error = false;

        try {
            cvt = Long.decode(value);
        } catch(NumberFormatException e) {
            error = true;
        }

        if(!error)
            return cvt;

        if(value.equalsIgnoreCase("true"))
            return Boolean.TRUE;
        else if(value.equalsIgnoreCase("false"))
            return Boolean.FALSE;

        return value;
    }

    protected void fullyConvertTypes()
        throws TypeErrorException {

        Set<String> keys = finalValues.keySet();
        Iterator<String> iter = keys.iterator();

        while(iter.hasNext()) {
            String key = iter.next();
            String value = finalValues.get(key);
            Property prop;

            prop = defaults.get(key);

            if(prop == null) {
                if(includeDefaults) {
                    if(prefixIsAllowed(key))
                        expandedValues.put(key, guessTypedValue(value));
                    else
                        System.out.println("ERROR: null type for key " + key);
                }

                continue;
            }
            String type = prop.type;
            if(type.equals("String")) {
                expandedValues.put(key, value);
            } else if(type.equals("Integer") || type.equals("Long")) {
                Long cvt = null;
                if(value.length() > 0) {
                    try {
                        cvt = Long.decode(value);
                    } catch(NumberFormatException e) {
                        throw new TypeErrorException("key " + key + ": unable to convert " +
                                                     value + " to an integer");
                    }
                } else {
                    cvt = new Long(0);
                }

                expandedValues.put(key, cvt);
            } else if(type.equals("Float")) {
                Float cvt = null;
                if(value.length() > 0) {
                    try {
                        cvt = Float.valueOf(value);
                    } catch(NumberFormatException e) {
                        throw new TypeErrorException("key " + key +
                                                     ": unable to convert " +
                                                     value + " to a float");
                    }
                } else {
                    cvt = new Float(Float.NaN);
                }
                expandedValues.put(key, cvt);
            } else if(type.equals("Boolean")) {
                if(value.equalsIgnoreCase("true"))
                    expandedValues.put(key, Boolean.TRUE);
                else if(value.equalsIgnoreCase("false"))
                    expandedValues.put(key, Boolean.FALSE);
                else if(value.equals(""))
                    expandedValues.put(key, Boolean.TRUE);
                else
                    throw new TypeErrorException("key " + key + ": unable to convert " +
                                                 value + " to a boolean");
            } else {
                throw new TypeErrorException("key " + key + ": bad type " + type);
            }
        }
    }

    protected void processSpecialCases() {
        if (values.containsKey("cosim.base")) {
            String value = values.get("cosim.base");
            values.put("cosim.base", value.replaceAll("\\s+", new String()));
        }
        if (values.containsKey("cosim.exceptions")) {
            String value = values.get("cosim.exceptions");
            String[] cosims = value.split(",");
            ArrayList<String> newCosims = new ArrayList<String>();
            Pattern co = Pattern.compile("(.*)\\{.*\\}");
            for (int i = 0; i < cosims.length; i++) {
                boolean dup = false;
                Matcher set = co.matcher(cosims[i]);
                if (set.matches()) {
                    String setVal = set.group(1);
                    for (int j = i+1; j < cosims.length; j++) {
                        Matcher sav = co.matcher(cosims[j]);
                        if (sav.matches()) {
                            if (sav.group(1).equals(setVal)) {
                                dup = true;
                                break;
                            }
                        }
                    }
                    if (!dup) {
                        newCosims.add(cosims[i]);
                    }
                } else {
                    newCosims.add(cosims[i]);
                }
            }
            StringBuilder newExceptions = new StringBuilder();
            for (int i = 0; i < newCosims.size(); i++) {
                if (i > 0) {
                    newExceptions.append(",");
                }
                newExceptions.append(newCosims.get(i));
            }
            values.put("cosim.exceptions", newExceptions.toString());
        }
    }
    protected void postProcessProperties() {
        PropReader clone = this.clone();
        // Check for unknown keys
        for (String key : clone.finalValues.keySet()) {
            if (!defaults.containsKey(key)) {
                boolean excluded = false;
                for (String exclude : allowedPrefixes) {
                    if (key.indexOf(exclude) != -1) {
                        excluded = true;
                        break;
                    }
                }
                if (!excluded) {
                    System.out.println("ERROR: " + key + " does not exist in the XML defaults");
                    finalValues.remove(key);
                }
            }
        }
        /* do type conversion */
        fullyConvertTypes();
    }

    /* returns the property space as a hierarchical namespace */
    @SuppressWarnings("unchecked")
    public Map<String, Object> getProperties() {
        return CollectionUtils.makeHierarchicalMap(expandedValues, '.');
    }

    /* utility methods */

    public void printValues() {
        for (String key : finalValues.keySet()) {
            String value = finalValues.get(key);
            System.out.println(key + "=" + value);
        }
    }

    public static void main(String[] args) {
        boolean includeDefaults = true;
        PropReader pr = new PropReader();

        /* examine command line arguments */
        if(args.length == 0) {
            System.out.println("Usage: PropReader [ no-defaults ] <defaults file>.xml");
            System.out.println("");
            System.exit(-1);
        }

        if(args[0].equals("no-defaults")) {
            includeDefaults = false;
        }

        try {
            pr.loadProperties(args, includeDefaults);
        } catch(ExpansionErrorException e) {
            System.out.println("ERROR: " + e.getMessage());

            System.exit(-1);
        } catch(TypeErrorException e) {
            System.out.println("ERROR: " + e.getMessage());

            System.exit(-1);
        }
        pr.printValues();
    }
}
