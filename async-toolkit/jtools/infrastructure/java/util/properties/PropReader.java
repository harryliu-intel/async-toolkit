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
import com.fulcrummicro.util.misc.JepMathExpressionParser;
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
public class PropReader implements PropReaderInterface {
    private boolean includeDefaults = true;

    /* list of prefixes we are allowed to add that don't have defaults */
    private final ArrayList<String> allowedPrefixes = new ArrayList<String>();

    /* list of properties whose assignments are aggregated */
    private final Map<String,Object> aggrProps = new HashMap<String,Object>();
    /* list of properties whose assignments are aggregated but not collapsed (duplicates allowed) */
    private final Map<String,Object> aggrPropsNonCollapsed = new HashMap<String,Object>();

    /* values entered on the commandline and via config files */
    private Map<String,String> cmdlineValues =
        new LinkedHashMap<String,String>();

    /* contains all the properties before they are hiearchified */
    private Map<String,String> defaults = new LinkedHashMap<String,String>();
    private Map<String,String> values = new LinkedHashMap<String,String>();
    private Map<String,String> types = new LinkedHashMap<String,String>();

    /* the un-hiearchified property space (fully expanded) */
    private Map<String,Object> expandedValues =
        new LinkedHashMap<String,Object>();

    /* the regular expression defining expansion */
    private final Pattern loopPat = Pattern.compile("\\[(\\d+)-(\\d+)\\]");  // [start-end]
    // ${MATH} or $%MATH%
    private final Pattern mathPat = Pattern.compile("(?:" + "\\$\\{([^{}]+)\\}" + ")|(?:" + "\\$%(.+?)%" + ")");

    public static final String VARIANT_SEPERATOR = ":";

    private boolean debug = false;
    private boolean debugVerbose = debug && true;
    private boolean suppressErrors = false;

    /* basic constructor to initialize stuff */
    public PropReader() {
        aggrProps.put("variant", Boolean.TRUE);
        aggrProps.put("log.scope", Boolean.TRUE);
        aggrProps.put("regress.test_feature", Boolean.TRUE);
        aggrProps.put("testbench.tchk.scope", Boolean.TRUE);
        aggrProps.put("verilog.ncoption.ncsdf_cmd_file", Boolean.TRUE);
        aggrProps.put("verilog.ncoption.nctfile", Boolean.TRUE);
        aggrPropsNonCollapsed.put("cosim.exceptions", Boolean.TRUE);
    }

    protected PropReader clone() {
        PropReader clone = new PropReader();
        clone.includeDefaults = includeDefaults;
        clone.allowedPrefixes.addAll(allowedPrefixes);
        clone.aggrProps.putAll(aggrProps);
        clone.aggrPropsNonCollapsed.putAll(aggrPropsNonCollapsed);
        clone.cmdlineValues.putAll(cmdlineValues);
        clone.defaults.putAll(defaults);
        clone.values.putAll(values);
        clone.types.putAll(types);
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
                for(String key : values.keySet()) {
                    System.err.println("  v " + key + " = " + values.get(key));
                }
                for(String key : defaults.keySet()) {
                    System.err.println("  d " + key + " = " + defaults.get(key));
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
        PropCommandLineParser clp = new PropCommandLineParser(defaults, types,
                                                              allowedPrefixes, args, this);

        /* grab xml files as bad arguments */
        ArrayList<String> badArgs = clp.parseCommandLine();

        /* preapply topdir, because it is special*/
        String topdir = clp.searchPropList("topdir");

        /* this is necessary so that we can include xml files */
        if(topdir != null) {
            cmdlineValues.put("topdir", topdir);
        } else {
            System.out.println("! no topdir found");
        }

        for(int i = 0; i < badArgs.size(); i++) {
            String arg = badArgs.get(i);

            if(!arg.startsWith("--") && arg.endsWith(".xml")) {
                /* load defaults from XML files */
                loadDefaults(arg);

                defaultsLoaded = true;
            }
        }

        if(!defaultsLoaded)
            throw new ExpansionErrorException("no defaults xml file found on commandline");

        if(includeDefaults) {
            /* add default values to value table */
            addDefaultValues();
        }

        /* called first to provide any properties used in include tags */
        clp.evaluateCommandLineProperties(false);

        /* evaluates the first config file and returns any nested includes */
        clp.evaluateConfigFiles();

        /* do this again because the config file may have defaults to add */
        if(includeDefaults) {
            addDefaultValues();
        }

        /* Find any loops that have substitutions like [1-$(N)] */
        debugPrint("* about to expandSubstitutionLoops");
        expandSubstitutionLoops(clp);

        /* remaining processing steps */
        processProperties(clp);

        /* do type conversion */
        fullyConvertTypes();

        return badArgs;
    }

    /** second part of property parsing
     *  split out so it can be done on cloned version too*/
    protected void processProperties(PropCommandLineParser clp) {
        /* get config blocks that didn't have a name field */
        /* XmlPropFileHandler gave them the name _UNNAMED_ */
        debugPrint("* about to add _UNNAMED_");
        addPropertiesFromNamedBlock("_UNNAMED_", true);

        /* handle named config blocks */
        debugPrint("* about to evaluateNamedBlocks");
        evaluateNamedBlocks();

        /* since this is called last, commandline always overrides */
        clp.evaluateCommandLineProperties(true);  // first time, expand loops overwriting
        clp.evaluateCommandLineProperties(false); // then reload non-loop to overwrite loops

        /* remove duplicates in aggrProps */
        debugPrint("* about to collapseAggrProps");
        collapseAggrProps();

        /* write input values into table */
        debugPrint("* about to mergeCommandLineValues");
        mergeCommandLineValues();

        /* perform expansion on all keys/values */
        debugPrint("* about to fullyExpandSubstitutions");
        fullyExpandSubstitutions();

        /* perform math expansion on all values */
        debugPrint("* about to expandMathExpressions");
        expandMathExpressions();
    }

    protected void expandSubstitutionLoops(PropCommandLineParser clp) {
        PropReader clone = this.clone();
        clone.processProperties(clp);

        String substPat = "\\$\\([\\w._]+\\)";
        String digitPat = "\\d+";
        Pattern firstPat = Pattern.compile(String.format("\\[(%s)-%s\\]", substPat, digitPat));
        Pattern lastPat = Pattern.compile(String.format("\\[%s-(%s)\\]", digitPat, substPat));
        Pattern bothPat = Pattern.compile(String.format("\\[(%s)-(%s)\\]", substPat, substPat));
        Pattern[] patterns = {bothPat, firstPat, lastPat}; // do 'both' first, since it will degenerate into 'last'

        values = doExpandSubstitutionLoops(values, clone, patterns);
        cmdlineValues = doExpandSubstitutionLoops(cmdlineValues, clone, patterns);
    }

    private Map<String,String> doExpandSubstitutionLoops(Map<String,String> valueMap, PropReader clone, Pattern[] patterns) {
        Map<String,String> newValues = new LinkedHashMap<String,String>();
        Set<String> keys = new HashSet<String>(valueMap.keySet());
        Iterator<String> iter = keys.iterator();

        while(iter.hasNext()) {
            String key = iter.next();
            String value = valueMap.get(key);
            String type = types.get(key);
            boolean found = false;

            for (Pattern pattern : patterns) {
                Matcher m = pattern.matcher(key);
                if(m.find()) {
                    found = true;
                    if(debug) { System.err.println("  expandSubstitutionLoops found " + key + " : " + m.group(1)); }
                    SubstExpr s = new SubstExpr(m.group(1));
                    s.evaluateSubst();
                    String result = (String)clone.expandedValues.get(s.match());
                    key = key.substring(0, m.start(1)) + result + key.substring(m.end(1));
                }
            }
            newValues.put(key, value);
            if(found) {
                if(!types.containsKey(key)) types.put(key, type);
                if(!defaults.containsKey(key)) defaults.put(key, value);
                expandLoopsInKeys(key, newValues, defaults, types, false);
            }
        }
        return newValues;
    }

    protected void addDefaultValues() {
        Set<String> keys = new HashSet<String>();
        keys.addAll(defaults.keySet()); // make a copy since expandLoopsInKeys will modify defaults
        Iterator<String> iter = keys.iterator();

        while(iter.hasNext()) {
            String key = iter.next();
            String value = defaults.get(key);
            if(debug) System.err.println("    addDefaultValues " + key + ", " + value);

            if(values.get(key) == null) {
                values.put(key, value);
                expandLoopsInKeys(key, values, defaults, types, false);
            }
        }
    }

    protected void mergeCommandLineValues() {
        Set<String> keys = cmdlineValues.keySet();
        Iterator<String> iter = keys.iterator();

        while(iter.hasNext()) {
            String key = iter.next();
            String value = cmdlineValues.get(key);

            if(defaults.containsKey(key) ||
               defaults.containsKey(stripNamedBlockFromKey(key)) ||
               prefixIsAllowed(key) ||
               (isNamedBlockKey(key) && prefixIsAllowed(stripNamedBlockFromKey(key)))) {
                if(values.containsKey(key)) {
                    values.remove(key);
                }
                values.put(key, value);
            } else {
                if(isNamedBlockKey(key))
                    errorPrint("in named block \"" +
                               getNamedBlockFromKey(key) +
                               "\", " + key + " does not exist in the XML defaults");
                else
                    errorPrint(key + " does not exist in the XML defaults");
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
                            nextFile = expandValue(s, defaults);
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

            /* parse the xml */
            try {
                XmlUtils.parseXml(istr,
                                  new XmlPropFileHandler(defaults,
                                                         types,
                                                         new ArrayList<String>(),
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

    public void expandLoopsInKeys(String key,
                                  Map<String,String> values,
                                  Map<String,String> defaults,
                                  Map<String,String> types,
                                  boolean overrideExisting)
        throws ExpansionErrorException {


        String def = defaults.get(key);
        String type = types.get(key);
        String value = values.get(key);

        Matcher m = loopPat.matcher(key);

        if(m.find()) {
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

            /* remove old key */
            values.remove(key);
            types.remove(key);
            defaults.remove(key);

            /* add new keys and expand them as we add them */
            for(int i = lb; i <= ub; i++) {
                String newKey = key.substring(0, m.start(1) - 1) + i +
                                key.substring(m.end(2) + 1);

                /**
                 * if a value exists then someone has overridden
                 * it on the commandline and we should not override
                 * it again
                 */
                if(!values.containsKey(newKey) || overrideExisting)
                    values.put(newKey, value);

                /**
                 * if a type already exists, then some other key in
                 * defaults describes this particular subset of the
                 * range already, so don't override the type
                 */
                if(!types.containsKey(newKey) || (types.get(newKey) == null))
                    types.put(newKey, type);

                if(def != null) defaults.put(newKey, def);

                expandLoopsInKeys(newKey, values, defaults, types, overrideExisting);
            }
        }
    }

    protected ArrayList<String> splitCommaSeparatedString(String s) {
        ArrayList<String> x = new ArrayList<String>();

        int index = s.indexOf(",");
        while(index != -1) {
            x.add(s.substring(0, index));
            s = s.substring(index + 1);

            index = s.indexOf(",");
        }
        x.add(s);

        return x;
    }

    protected String getNamedBlockFromKey(String key) {
        if(key.startsWith("_NAMED_")) {
            return key.substring(7, key.indexOf('.'));
        }

        return key;
    }

    protected String stripNamedBlockFromKey(String key) {
        if(key.startsWith("_NAMED_")) {
            return key.substring(key.indexOf('.') + 1);
        }

        return key;
    }

    protected boolean isNamedBlockKey(String key) {
        if(key.startsWith("_NAMED_")) {
            return true;
        }

        return false;
    }

    protected void evalUseForNamedBlock(String name) {
        if(debug) System.err.println("evalUseForNamedBlock(" + name + ")");

        Set<String> keys = cmdlineValues.keySet();

        /* pre-iterate to find _USE_ statement */
        for(String key : keys) {
            if(isNamedBlockKey(key)) {
                /* strip off _NAMED_<blockname>. prefix */
                String actualKey = stripNamedBlockFromKey(key);
                String namedBlock = getNamedBlockFromKey(key);

                /* is it the one we want? */
                if(namedBlock.equals(name) && actualKey.equals("_USE_")) {
                    evalUseBlock(name, namedBlock);
                    break;
                }
            }
        }
    }

    protected void evalUseBlock(String name, String block) {
        if(debug) System.err.println(" evalUseBlock(" + name + ", " + block + ")");
        boolean found = false;

        /* now iterate through the use blocks to find any recursive uses */
        String useKey = "_NAMED_" + block + "._USE_";
        String use = cmdlineValues.get(useKey);

        if(use != null) {
            found = true;
            ArrayList<String> useList
                = splitCommaSeparatedString(use);

            for(String u : useList) {
                evalUseBlock(name, u);
            }
        }


        Set<String> keys = cmdlineValues.keySet();

        /* we need to add keys later */
        Map<String, String> keysToAdd = new HashMap<String, String>();

        if(debug) System.err.println("  adding contents of use block " + block);
        for(String key : keys) {
            if(isNamedBlockKey(key) &&
               getNamedBlockFromKey(key).equals(block) &&
               !key.endsWith("_USE_")) {
                found = true;
                String newKey = //"_NAMED_" + name + "." +
                    stripNamedBlockFromKey(key);
                /* Do not add the new value if it is already in KeysToAdd
                 * namedBlock and higherlevel usesBlocks have precedence */
                if (!keysToAdd.containsKey(newKey)) {
                    /* add this under the namedblock name */
                    if(debug) System.err.println("    adding " + newKey);
                    handleProperty(keysToAdd, newKey, cmdlineValues.get(key));
                } else {
                    if(debug) System.err.println("    skipping " + newKey);
                }
            }
        }

        /* add new keys */
        for(String key : keysToAdd.keySet()) {
            handleProperty(cmdlineValues, key, keysToAdd.get(key));
        }

        if(!found) {
            String err = "WARNING: no properties or uses for use block named " + block + " in " + name + " were found";
            System.out.println(err);
            debugPrint(err);
        }
    }

    /** add all the properties from the block named 'name' to cmdlineValues.
     * @param name NAMED variant/config name
     * @param allowEmpty if false, fire an error if no properties are added
     */
    protected void addPropertiesFromNamedBlock(String name, boolean allowEmpty) {
        if(debug) System.err.println("addPropertiesFromNamedBlock(" + name + ", " + allowEmpty + ") starting");

        // first check if this is multi-level and process top first, since inside can override outside
        int splitPoint = name.lastIndexOf(VARIANT_SEPERATOR);
        if(splitPoint != -1) {
            String start = name.substring(0, splitPoint);
            String end = name.substring(splitPoint+1);
            addPropertiesFromNamedBlock(start, true); // will do addition splits
            addPropertiesFromNamedBlock(end, true); // inner should override outer
        }

        debugPrint("addPropertiesFromNamedBlock(" + name + ") starting processing");

        Set<String> keys = cmdlineValues.keySet();
        int numProps = 0;
        Map<String,String> newValues = new LinkedHashMap<String,String>();

        /* handle use statement first */
        evalUseForNamedBlock(name);

        for(Iterator<String> iter = keys.iterator(); iter.hasNext();) {
            String key = iter.next();
            String value = cmdlineValues.get(key);

            /* all named blocks have this prefix */
            if(isNamedBlockKey(key) &&
               getNamedBlockFromKey(key).equals(name)) {

                /* strip off _NAMED_<blockname>. prefix */
                String actualKey = stripNamedBlockFromKey(key);
                String newValue = cmdlineValues.get(key);

                if(!actualKey.equals("_USE_")) {
                    /* named block always overrides existing values */
                    if(debug) System.err.println("  " + key + " -> " + actualKey + " = " + value + " -> " + newValue);
                    handleProperty(newValues, actualKey, newValue);
                }
                numProps++;
                newValues.put(key, value); // need to keep them here in case they are included again
            } else {
                /* do not override values already added */
                if(newValues.get(key) == null) {
                    newValues.put(key, value);
                } else {
                    if(debug) System.err.println("  excluding " + key);
                    // We want to use the old value, but still be able to merge aggrProps
                    String old = newValues.put(key, value);
                    handleProperty(newValues, key, old);
                }
            }
        }

        if((numProps == 0) && !suppressErrors && !allowEmpty) {
            System.out.println("WARNING: no properties from block named " + name + " were found");
            debugPrint("WARNING: no properties from block named " + name + " were found");
        }
        cmdlineValues = newValues;

        debugPrint("addPropertiesFromNamedBlock(" + name + ") complete");
    }

    protected void removeUnusedVariantBlocks() {
        Set<String> keys = cmdlineValues.keySet();
        Iterator<String> iter = keys.iterator();
        ArrayList<String> keysToRemove = new ArrayList<String>();


        while(iter.hasNext()) {
            String key = iter.next();
            Object value = cmdlineValues.get(key);

            /* assume at this time that all used named blocks
             * have been evaluated
             */
            if(isNamedBlockKey(key))
                keysToRemove.add(key);
        }

        while(keysToRemove.size() > 0)
            cmdlineValues.remove(keysToRemove.remove(0));

    }

    protected void evaluateNamedBlocks() {
        /* grab list of variants */
        String variant = cmdlineValues.get("variant");

        if(variant != null) {
            ArrayList<String> variantList = splitCommaSeparatedString(variant);

            while(variantList.size() > 0) {
                String name = variantList.remove(0);

                /* remove fragments and duplicates */
                if(!name.equals("") && !variantList.contains(name))
                    addPropertiesFromNamedBlock(name, false);
            }
        }

        debugPrint("about to evaluateNamedBlocks.removeUnusedVariantBlocks");
        removeUnusedVariantBlocks();
    }

    protected void expandMathExpressions() {
        JEP parser = new JepMathExpressionParser();

        Set<String> keys = expandedValues.keySet();
        Iterator<String> iter = keys.iterator();

        /* perform substitution expansion */
        while(iter.hasNext()) {
            String key = iter.next();
            String value = (String)expandedValues.get(key);

            boolean found;
            do {
                found = false;
                Matcher m = mathPat.matcher(value);

                if(m.find()) {
                    String mathExp = (m.group(1) != null) ? m.group(1) : m.group(2);
                    if(debugVerbose) System.err.println("    Found math exp " + mathExp + " in " + value);
                    if(parser.parseExpression(mathExp) != null) {
                        found=true;
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

                        Object replaced = expandedValues.put(key, value);
                        if(debugVerbose) System.err.println("    Replaced " + replaced + " with " + value);
                    } else {
                        errorPrint("Failure to parse math expression " + value + " in key " + key + ": " + parser.getErrorInfo());
                    }
                }
            } while(found);
        }
    }

    public void fullyExpandSubstitutions()
        throws ExpansionErrorException {

        Set<String> keys = values.keySet();
        Iterator<String> iter = keys.iterator();

        /* perform substitution expansion */
        while(iter.hasNext()) {
            String key = iter.next();
            String value = values.get(key);
            if(value == null) {
                throw new ExpansionErrorException("value for " + key + " is null");
            }

            boolean canExpandKey = canExpandString(key);
            String expandedKey = canExpandKey ?
                                 expandKey(key, values) :
                                 key;

            String expandedValue = canExpandString(value) ?
                                   expandValue(value, values) :
                                   value;

            //if(debugVerbose) System.err.println(String.format("      %s = %s => () => %s = %s",
            //                                                  key,value, expandedKey,expandedValue));

            // expanded values take precedence
            if(!expandedValues.containsKey(expandedKey) || canExpandKey) {
                expandedValues.put(expandedKey, expandedValue);
            } else if(debugVerbose) {
                System.err.println(String.format("      fullyExpandSubstitutions: Skipping insert of %s = %s",
                                                 expandedKey,expandedValue));
            }
        }
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

        Set<String> keys = expandedValues.keySet();
        Iterator<String> iter = keys.iterator();

        while(iter.hasNext()) {
            String key = iter.next();
            String value = (String) expandedValues.get(key);
            String type = types.get(key);

            if(type == null) {
                if(includeDefaults) {
                    if(prefixIsAllowed(key))
                        expandedValues.put(key, guessTypedValue(value));
                    else
                        System.out.println("ERROR: null type for key " + key);
                }

                continue;
            }

            if(type.equals("String")) {
                /* do nothing */
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

    /** Remove duplicate options in aggrProps properties.
     *
     *  The remaining item should be the last possible, since later options
     *  override earlier options.
     */
    protected void collapseAggrProps() {
        for (String aggrProp : aggrProps.keySet()) {
            String curr = cmdlineValues.get(aggrProp);

            if(curr != null) {
                String[] opts = curr.split(",");
                ArrayList<String> goodOpts = new ArrayList<String>();

                for (String string : opts) {
                    // remove if already there
                    goodOpts.remove(string);

                    // always add to end
                    goodOpts.add(string);
                }

                String value = "";
                for (String string : goodOpts) {
                    if(value != "") value += ",";
                    value += string;
                }

                // replace the old with the new
                cmdlineValues.put(aggrProp, value);
            }
        }
    }

    public void handleProperty(Map<String,String> valuesMap, String key, String value) {
        handleProperty(valuesMap, key, value, false);
    }
    public void handleProperty(Map<String,String> valuesMap, String key, String value, boolean override) {
        if((aggrProps.get(stripNamedBlockFromKey(key)) != null) ||
            (aggrPropsNonCollapsed.get(stripNamedBlockFromKey(key)) != null) ||
            (key.contains("_USE_"))) {
            String curr = valuesMap.get(key);

            /* if curr is empty skip and do the usual thing */
            if(curr != null) {
                /* merge */
                if(debug) System.err.println("handleProperty merge (" + key + ")");
                if (key.equals("cosim.exceptions")) {
                    String[] cosims = curr.split(",");
                    Pattern co = Pattern.compile("(.*)\\{.*\\}");
                    boolean subst = false;
                    Matcher set = co.matcher(value);
                    if (set.matches()) {
                        String setVal = set.group(1);
                        for (int i = 0; i < cosims.length; i++) {
                            Matcher sav = co.matcher(cosims[i]);
                            if (sav.matches()) {
                                if (sav.group(1).equals(setVal)) {
                                    cosims[i] = value;
                                    subst = true;
                                }
                            }
                        }
                    }
                    if (subst) {
                        value = cosims[0];
                        for (int i = 1; i < cosims.length; i++) {
                            value = cosims[i] + "," + value;
                        }
                    } else {
                        value = curr + "," + value;
                    }
                } else {
                    value = curr + "," + value;
                }
            }
        } else if(valuesMap.containsKey(key)) {
            if(debug) System.err.println("handleProperty valuesMap.remove(" + key + ")");
            valuesMap.remove(key);
        }

        valuesMap.put(key, value);
        expandLoopsInKeys(key, valuesMap, defaults, types, override);
    }

    /* methods from the PropReaderInterface */

    public void handleProperty(String key, String value, boolean override) {
        handleProperty(cmdlineValues, key, value, override);
    }
    public void handleProperty(String key, String value) {
        handleProperty(cmdlineValues, key, value);
    }

    public String evaluateProperty(String value)
        throws ExpansionErrorException {

        /* do any preliminary parsing */
        if(canExpandString(value)) {
            /* check commandline values only */
            return expandValue(value, cmdlineValues);
        }

        return value;
    }

    public void handlePropertyDefault(String key, String value, String type) {
    }

    /* returns the property space as a hierarchical namespace */
    @SuppressWarnings("unchecked")
    public Map<String, Object> getProperties() {
        return CollectionUtils.makeHierarchicalMap(expandedValues, '.');
    }

    /* utility methods */

    public void printValues() {
        Set<String> keys = expandedValues.keySet();
        Iterator<String> iter = keys.iterator();

        while(iter.hasNext()) {
            String key = iter.next();
            Object value = expandedValues.get(key);
            String type = types.get(key);

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
