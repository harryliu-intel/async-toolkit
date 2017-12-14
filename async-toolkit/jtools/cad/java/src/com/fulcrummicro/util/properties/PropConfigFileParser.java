package com.fulcrummicro.util.properties;

import java.util.ArrayList;
import java.util.Map;

import java.io.FileInputStream;
import java.io.IOException;

import com.fulcrummicro.util.xml.XmlUtils;
import com.fulcrummicro.util.xml.XmlPropFileHandler;
import com.fulcrummicro.util.properties.PropReader;
/**
 * Parses config files containing key=value pairs
 *
 * @author Naru Sundar
 */
public class PropConfigFileParser {

    protected final ArrayList<String> lines = new ArrayList<String>();
    protected final ArrayList<String> nfiles = new ArrayList<String>();
    protected int lineNo = 0;
    protected int startingLine = 0;

    protected static final int READ_AHEAD_LIMIT = 4096;

    protected final ArrayList<String> prefixes;
   
    public PropConfigFileParser(Map<String, PropConfig> configs,
            ArrayList<String> prefixes,
            String filename ) {
        this(configs, prefixes, filename, "");
    }
    
    public PropConfigFileParser(Map<String, PropConfig> configs,
            ArrayList<String> prefixes,
            String filename,
            String test ) {
        this.prefixes = prefixes;

        FileInputStream istr = null;

        filename = configs.get(PropReader.TEST_CONFIG).expandValue(filename);

        try {
            istr = new FileInputStream(filename);
        } catch(IOException e) {
            System.out.println("ERROR: " + e.getMessage());
        }

        /* parse the xml to just the properties */
        try {
            XmlUtils.parseXml(istr,
                              new XmlPropFileHandler(configs,
                                      nfiles,
                                      prefixes,
                                      test));
        } catch(BadPropertyException e) {
            System.out.println("ERROR: unable to parse " + 
                               filename + ": " + e.getMessage());
        }

        parseConfigFile(configs);

        try {
            istr.close();
        } catch(IOException e) {
            System.out.println("ERROR: " + e.getMessage());
        }
    }


    protected String readFilteredLine() throws
        PropConfigParserException {

        String block = new String("");
        String line = null, currLine = new String("");
        boolean inComment = false;

        while(true) {
            lineNo++;

            /* pull next line out of array */
            if(lines.size() == 0)
                if(currLine.equals("")) {
                   return null;
                } else {
                    if(block.equals(""))
                        return currLine;
                    else
                        return "_NAMED_" + block + "." + currLine.trim();
                }
            else {
                line = lines.remove(0).trim();
            }

            /* pull off block name first */
            if(line.startsWith("_NAMED_")) {
                block = line.substring(7, line.indexOf('.'));
                line = line.substring(line.indexOf('.') + 1);
            }

            /* ignore blank lines */
            if(line.length() == 0) {
                continue;
            }

            /* ignore full end of line comments */
            if(line.startsWith("#") ||
               line.startsWith("//"))
                continue;

            /* check for single line comment */
            if(line.indexOf("/*") != -1) {
                if(inComment) {
                    throw new PropConfigParserException("line " + lineNo + 
                                                        ": nested comments are not supported");
                } else {
                    currLine = currLine + 
                               line.substring(0, line.indexOf("/*"));

                    if(line.indexOf("*/") == -1) {
                        inComment = true;
                    } else {
                        inComment = false;

                        currLine = currLine + 
                                   line.substring(line.indexOf("*/") + 2);
                    }
                }

                continue;
            } else if((line.indexOf("*/") != -1) && inComment) {
                inComment = false;

                currLine = currLine +
                           line.substring(line.indexOf("*/") + 2);

                continue;
            } else if(inComment) {
                continue;
            }

            /* check for end of line continuation */
            if(line.endsWith("\\")) {
                currLine = currLine + line.substring(0, line.indexOf("\\"));
            } else {
                if(line.length() > 0) {
                    currLine = currLine + line;

                    if(block.equals(""))
                        return currLine;
                    else
                        return "_NAMED_" + block + "." + currLine.trim();
                }
            }
        }
    }

    protected void parseConfigFile(Map<String, PropConfig> configs)
        throws PropConfigParserException {

        /**
         * in order to get the precedence to be sane, we evaluate
         * from the inside out, so evaluate all files included by
         * this one recursively first before we apply properties
         * in this file, that way, each file can override any that
         * they include
         */

        while(nfiles.size() > 0) {
            String filename = nfiles.remove(0);
            new PropConfigFileParser(configs, prefixes, filename);
        }
    }
}
