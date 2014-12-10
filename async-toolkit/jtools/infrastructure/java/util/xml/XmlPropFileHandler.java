package com.fulcrummicro.util.xml;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.Reader;

import java.util.HashMap;
import java.util.Map;
import java.util.ArrayList;
import java.util.Stack;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.XMLReader;
import org.xml.sax.InputSource;
import javax.xml.parsers.SAXParserFactory;

import com.avlsi.util.exception.ExceptionUtils;

import com.fulcrummicro.util.properties.BadPropertyException;
import com.fulcrummicro.util.properties.PropReader;
import com.fulcrummicro.util.xml.XmlUtils;

import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * XML parser handler for reading config files, supported
 * XML tags includ:
 *      - "config" for defining properties
 *
 * @author Naru Sundar
 */
public class XmlPropFileHandler extends DefaultHandler {
    private final boolean debug = false;

    /* maps to hold type information */
    protected final Map<String,String> defaults;
    protected final Map<String,String> types;

    /* list of any prefixes that should be ignored */
    protected final ArrayList<String> userPrefixes;

    /* holds lines grabbed from inside config block */
    protected final ArrayList<String> lines;

    /* holds a list of any include statements we find */
    protected final ArrayList<String> newFiles;

    /**
     * this list holds the tag stack; a list is used
     * instead of a stack so that we can peek at the entire
     * history 
     */
    protected final ArrayList<Object[]> tagStack = new ArrayList<Object[]>(); 
    protected final int TAGSTACK_TAG = 0;
    protected final int TAGSTACK_NAME = 1;
    protected final int TAGSTACK_USES = 2;
    
    protected String currentLine;

    /*
     * When test is set, we will only parse the contents of that test's block. We
     * will also add named config blocks that are outside of any other test block.
     */
    /** Name of test to parsein testplan. "" for simple test file. */
    protected final String test;
    /** True until named test is found. */
    protected boolean testUnfound;
    /** True when parsing test or named config block outside of any test. */
    protected boolean parsing;
    /** True when we are parsing within named test. */ 
    protected boolean inNamedTest;
    
    protected final Pattern repPat = Pattern.compile(".*&(.*);.*");
    
    protected final HashMap<String, String> tagMapping;

    public XmlPropFileHandler(Map<String,String> defaults, 
        Map<String,String> types,
        ArrayList<String> lines, 
        ArrayList<String> newFiles,
        ArrayList<String> prefixes) {
        
        this(defaults, types, lines, newFiles, prefixes, "");
    }
    
    public XmlPropFileHandler(Map<String,String> defaults, 
                              Map<String,String> types,
                              ArrayList<String> lines, 
                              ArrayList<String> newFiles,
                              ArrayList<String> prefixes,
                              String test) {
        this.lines = lines;
        this.newFiles = newFiles;
        this.defaults = defaults;
        this.types = types;
        this.userPrefixes = prefixes;
        if(test == null) test = "";
        this.test = test;
        
        if(test.equals("")) {
            parsing = true;
            testUnfound = false;
        } else {
            parsing = false;
            testUnfound = true;
            inNamedTest = false;
        }
        
        // List of special tags that are converted into options
        tagMapping = new HashMap<String, String>();
        tagMapping.put("owner", "regress.test_owner");
        tagMapping.put("author", "regress.test_author");
        tagMapping.put("feature", "regress.test_feature");
        
        if(debug) System.err.println("*** XmlPropFileHandler test: " + test + " (" + parsing + ")");
            
        currentLine = null;
    }
    
    public void endDocument() {
        if(testUnfound) {
            System.out.println("ERROR: No test '" + test + "' was found in given XML file");
        }
    }
    
    private boolean inTest() {
        for (Object[] tag : tagStack) {
            if(tag[TAGSTACK_TAG].equals("test")) return true;
        }
        
        return false;
    }

    public void startElement (String uri, String unused,
                              String qName, Attributes atts) {
        /* push the "name" and "use" properties, only for config blocks */
        tagStack.add(0, new Object[] { qName, 
                                       atts.getValue("name"),
                                       atts.getValue("uses") });
        if(debug) System.err.println("*** startElement " + tagStack.get(0)[TAGSTACK_TAG] + ", " + tagStack.get(0)[TAGSTACK_NAME] + " (" + parsing + ")");
        if(!parsing) {
            if((qName.equals("test")) && 
               (atts.getValue("name") != null) && 
               (atts.getValue("name").equals(test))) {
                testUnfound = false;
                parsing = true;
                inNamedTest = true;
            }
        }
        
        if(!parsing) {
            if(qName.equals("config") && !inTest()) {
                parsing = true;
                if(debug) System.err.println("       startElement: config parsing = true " +  atts.getValue("name"));
            }
        }
        
        if(parsing) {
            if(qName.equals("include")) {
                String file = atts.getValue("file");
    
                if(file != null)
                    newFiles.add(file);
                else
                    System.out.println("ERROR: in property file, " +
                            "include tag without file property");
            } else if (qName.equals("opt")) {
                String name = atts.getValue("name");
                String type = atts.getValue("type");
                String def = atts.getValue("default");
    
                if(name == null) {
                    System.out.println("ERROR: in XML defaults file, " +
                            "opt tag without name property"); 
                } else if(type == null) {
                    System.out.println("ERROR: in XML defaults file, opt " + 
                            name + " has no type"); 
                } else if(def == null) {
                    System.out.println("ERROR: in XML defaults file, opt " + 
                            name + " has no default"); 
                } else {
                    defaults.put(name, def);
                    types.put(name, type);
                }
            } else if(qName.equals("ignore")) {
                if(atts.getValue("prefix") != null) {
                    String prefix = atts.getValue("prefix");
    
                    if(!prefix.endsWith("."))
                        userPrefixes.add(prefix);
                }
            } else if(qName.equals("test")) {
                if(atts.getValue("name") != null) {
                    lines.add("regress.test_name="+atts.getValue("name"));
                }
            }
        }
    }


    public void endElement (String uri, String name,
                            String qName) {
        if(parsing) {
            /* terminate the previous token if any */
            if((currentLine != null) && (currentLine.length() > 0))
                endOfToken(true);
    
            String configName = null;
            String configUse = null;
            /* these get parsed out later */
            if(isCurrentBlockValid("config", TAGSTACK_TAG)) {
                configName = getCurrentBlockArg("config", TAGSTACK_NAME);
                configUse = getCurrentBlockArg("config", TAGSTACK_USES);
            }
            if(isCurrentBlockValid("variant", TAGSTACK_TAG)) {
                configName = getCombinedVariantName();
                configUse = getCurrentBlockArg("variant", TAGSTACK_USES);
            }
            if(configUse != null) {
                if(configName != null) {
                    lines.add("_NAMED_" + configName + "._USE_=" + configUse);
                    if(debug) System.err.println("                      _NAMED_" + configName + "._USE_=" + configUse);
                } else {
                    lines.add("_NAMED__UNNAMED_._USE_=" + configUse);
                    if(debug) System.err.println("                      _NAMED__UNNAMED_._USE_=" + configUse);   
                }
            }
        }
        Object[] tag = tagStack.remove(0);
        
        if(debug) System.err.println("                                                *** endElement " + 
                                     tag[TAGSTACK_TAG] + ", " + tag[TAGSTACK_NAME] + " (" + parsing + ")");
        
        if((!test.equals("")) && (inNamedTest == false) &&
            (tag[TAGSTACK_TAG] != null) && (tag[TAGSTACK_TAG].equals("config"))) {
            parsing = false;
            if(debug) System.err.println("       endElement: config parsing = false ");
        }
        
        if((!test.equals("")) &&
           (tag[TAGSTACK_TAG] != null) && (tag[TAGSTACK_TAG].equals("test")) && 
           (tag[TAGSTACK_NAME] != null) && (tag[TAGSTACK_NAME].equals(test))) {
            parsing = false;
            inNamedTest = false;
        }
    }

    public void characters (char ch[], int start, int length) {
        if(parsing) {
            for(int i = start; i < (start + length); i++) {
                if(ch[i] != '\n') {
                    if(currentLine == null) {
                        currentLine = new String();
                    }
    
                    currentLine = currentLine + ch[i];
                } else {
                    endOfToken(false);
                }
            }
        }
    }

    protected void endOfToken(boolean endElement) {
        if(currentLine == null) return;
        
        boolean isConfig = isCurrentBlockValid("config", TAGSTACK_TAG);
        boolean isVariant = isCurrentBlockValid("variant", TAGSTACK_TAG);
        boolean isSet = isCurrentBlockValid("set", TAGSTACK_TAG);

        /* either an immediate config or an immediate set */
        if(isConfig || isVariant || isSet) { 
            /* unnamed blocks are implicitly named "" */
            String configName = getCurrentBlockArg("config", TAGSTACK_NAME);
            if(configName == null) { // not config, try variant
                configName = getCombinedVariantName();
            }

            if(isSet) {
                if(!endElement) return; // concatenate lines within set tag
                String paramName = getCurrentBlockArg("set", TAGSTACK_NAME);

                currentLine = paramName + "=" + currentLine;
            } 

            /* try to not add any extra lines */
            if(configName == null)
                configName = "_UNNAMED_"; // will be loaded later with PropReader
            
            lines.add("_NAMED_" + configName + "." + currentLine);   
            if(debug) System.err.println("                      _NAMED_" + configName + "." + currentLine);   
        } else {
            // Handle special tags that are converted into options
            for (String tag : tagMapping.keySet()) {
                if(isCurrentBlockValid(tag, TAGSTACK_TAG)) {
                    lines.add(tagMapping.get(tag) + "=" + currentLine);
                    break;
                }
            }            
        }

        currentLine = null;
    }

    protected String getCurrentBlockArg(String type, int arg) {
        for(int i = 0; i < tagStack.size(); i++) {
            Object[] blockTuple = tagStack.get(i);
            String blockType = (String) blockTuple[TAGSTACK_TAG];

            if(blockType.equalsIgnoreCase(type)) {
                return (String) blockTuple[arg];
            }
        }
        
        return null;
    }

    protected boolean isCurrentBlockValid(String type, int depth) {
        for(int i = 0; i < tagStack.size(); i++) {
            Object[] blockTuple = tagStack.get(i);
            String blockType = (String) blockTuple[TAGSTACK_TAG];

            if(blockType.equalsIgnoreCase(type)) {
                return i <= depth;
            }
        }
        
        return false;
    }
    
    /** Gets the combined nested (a:b:c) variant name 
     * Use in place of getCurrentBlockArg("variant", TAGSTACK_NAME) */
    protected String getCombinedVariantName() {
        String result = null;
        String type = "variant";

        for(int i = 0; i < tagStack.size(); i++) {
            Object[] blockTuple = tagStack.get(i);
            String blockType = (String) blockTuple[TAGSTACK_TAG];

            if(blockType.equalsIgnoreCase(type)) {
                if(result == null) {
                    result = ((String)blockTuple[TAGSTACK_NAME]);
                } else {
                    result = ((String)blockTuple[TAGSTACK_NAME]) + PropReader.VARIANT_SEPERATOR + result;
                }
            }
        }
        
        return result;
    }
}
