package com.fulcrummicro.util.xml;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

import com.fulcrummicro.util.properties.PropConfig;
import com.fulcrummicro.util.properties.PropConfigParserException;
import com.fulcrummicro.util.properties.PropReader;
import com.fulcrummicro.util.properties.Property;

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
    protected final Map<String, PropConfig> configs;

    /* list of any prefixes that should be ignored */
    protected final ArrayList<String> userPrefixes;

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

    ArrayList<String> currentLines = new ArrayList<String>();
    protected String currentLine = new String();

    /*
     * When test is set, we will only parse the contents of that test's block. We
     * will also add named config blocks that are outside of any other test block.
     */
    /** Name of test to parse in testplan. "" for simple test file. */
    protected final String test;
    /** True until named test is found. */
    protected boolean testUnfound;
    /** True when parsing test or named config block outside of any test. */
    protected boolean parsing;
    /** True when we are parsing within named test. */ 
    protected boolean inNamedTest;
    
    protected final Pattern repPat = Pattern.compile(".*&(.*);.*");
    
    protected final HashMap<String, String> tagMapping;
    
    public XmlPropFileHandler(Map<String, PropConfig> configs,
            ArrayList<String> newFiles,
            ArrayList<String> prefixes) {
        
        this(configs, newFiles, prefixes, "");
    }

    public XmlPropFileHandler(Map<String, PropConfig> configs,
            ArrayList<String> newFiles,
            ArrayList<String> prefixes,
            String test) {
        this.configs = configs;
        this.newFiles = newFiles;
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
    
    public void startElement(String uri, String unused, 
            String qName, Attributes atts) {
        
        preProcessElement(false);
        
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
            if (qName.equals("config") && !inTest()) {
                parsing = true;
                if(debug) System.err.println("       startElement: config parsing = true " +  atts.getValue("name"));
            }
        }
        
        if (inNamedTest || (!inTest() || test.equals(""))) {
            if (qName.equals("config") || qName.equals("variant")) {
            String name;
            if (qName.equals("variant")) {
                name = getCombinedVariantName();
            } else {
                name = atts.getValue("name");
            }
            if (name != null) {
                if (!configs.containsKey(name)) {
                    configs.put(name, new PropConfig());
                } else {
                    throw new PropConfigParserException("Duplicate config/variant name detected (" + name + ")");
                }
            }
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
                String def = atts.getValue("default").toString();
                String concat = atts.getValue("concat");
                String concat_op = atts.getValue("concat_op");
                // Make sure the opts are only declared once
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
                    // Make sure we are adding for current test only
                    if (inNamedTest || (!inTest() || test.equals(""))) {
                        Property prop;
                        if (concat != null) {
                            if (concat.equalsIgnoreCase("true")) {
                                prop = new Property(type,def, true, 
                                                    concat_op == null ? ',': concat_op.charAt(0));
                            } else {
                                prop = new Property(type,def);
                            }
                        } else {
                            prop = new Property(type,def);
                        }
                        this.configs.get(getCurrentConfig()).addOpt(name, prop);
                    }
                }
            } else if(qName.equals("ignore")) {
                if(atts.getValue("prefix") != null) {
                    String prefix = atts.getValue("prefix");
    
                    if(!prefix.endsWith("."))
                        userPrefixes.add(prefix);
                }
            } else if(qName.equals("test")) {
                if(atts.getValue("name") != null) {
                    configs.get(PropReader.TEST_CONFIG).addOpt("regress.test_name", new Property("String", atts.getValue("name")));
                }
            }
        }
    }
        
    public void characters (char ch[], int start, int length) {
        for (int i = start; i < (start + length); i++) {
            if (ch[i] != '\n') {
                currentLine = currentLine + ch[i];
            } else {
                if (currentLine.length() > 0) {
                    scrubWhiteSpaces();
                    currentLines.add(currentLine + " ");
                    currentLine = new String();
                }
            }
        }
    }

    void scrubWhiteSpaces() {
        currentLine = currentLine.replaceAll("^\\s+", "");
        currentLine = currentLine.replaceAll("\\s+$", "");
    }
    
    String getLines() {
        String value = new String();
        for (String line : currentLines) {
            value += line;
        }
        value = value.replaceAll("^\\s+", "");
        value = value.replaceAll("\\s+$", "");
        scrubWhiteSpaces();
        if (currentLine.length() > 0) {
            value += currentLine;
            currentLine = new String();
        }
        currentLines.clear();
        return value;
    }
    
    void preProcessElement(boolean end) {
        boolean isConfig = isCurrentBlockValid("config", TAGSTACK_TAG);
        boolean isVariant = isCurrentBlockValid("variant", TAGSTACK_TAG);
        boolean isSet = isCurrentBlockValid("set", TAGSTACK_TAG);

        if (parsing) {
            if (isConfig || isVariant || isSet) {
                String paramName = getCurrentBlockArg("set", TAGSTACK_NAME);
                String configName = getCurrentConfig();
                if (isConfig || isVariant) { 
                    if (currentLines.size() > 0) {
                        if (currentLine.length() > 0) {
                            scrubWhiteSpaces();
                            currentLines.add(currentLine);
                            currentLine = new String();
                        }
                        for (String line: currentLines) {
                            line = line.replaceAll("\\s+$", "");
                            int index = line.indexOf("=");
                            if (index != -1) {
                                String prop = line.substring(0, index);
                                String val = line.substring(index+1);
                                configs.get(configName).addSet(prop, val);
                            }
                        }
                        currentLines.clear();
                    }
                }
                if (paramName != null) {
                    if (test.equals("") || inNamedTest || !inTest()) {
                        configs.get(configName).addSet(paramName, getLines());
                    }
                }
            } else {
                // Handle special tags that are converted into options
                for (String tag : tagMapping.keySet()) {
                    if (isCurrentBlockValid(tag, TAGSTACK_TAG)) {
                        configs.get(PropReader.TEST_CONFIG).addOpt(tagMapping.get(tag), new Property("String", getLines()));
                        break;
                    }
                }            
            }
        }
        if (end) {
            currentLine = new String();
            currentLines.clear();
        }
    }
    
    public void endElement(String uri, String name, String qName) {
        preProcessElement(true);
        boolean isConfig = isCurrentBlockValid("config", TAGSTACK_TAG);
        boolean isVariant = isCurrentBlockValid("variant", TAGSTACK_TAG);
        if(parsing) {
            String configUse = null;
            if(isVariant) {
                configUse = getCurrentBlockArg("variant", TAGSTACK_USES);
            } else if (isConfig) {
                configUse = getCurrentBlockArg("config", TAGSTACK_USES);
            }
            
            if (configUse != null && (isConfig || isVariant)) {
                String configName = getCurrentConfig();
                PropConfig pc = null;
                if (inNamedTest && configName == null) {
                    pc = configs.get(PropReader.TEST_CONFIG);
                } else {
                    pc = configs.get(configName);
                }
                for (String config : configUse.split(",")) {
                    pc.addUses(config);
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

    protected String getCurrentConfig() {
        String configName = getCombinedVariantName();
        if (configName == null) {
            configName = getCurrentBlockArg("config", TAGSTACK_NAME);
        }
        if (configName == null) {
            return PropReader.TEST_CONFIG;
        } else {
            return configName;
        }
    } 
    
    protected String getCurrentBlockArg(String type, int arg) {
        for (Object[] blockTuple : tagStack) {
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
