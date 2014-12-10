package com.fulcrummicro.util.properties;

import java.util.ArrayList;
import java.util.Map;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import com.fulcrummicro.util.xml.XmlUtils;
import com.fulcrummicro.util.xml.XmlPropFileHandler;
import com.fulcrummicro.util.properties.PropReaderInterface;

/**
 * Parses config files containing key=value pairs
 *
 * @author Naru Sundar
 */
public class PropConfigFileParser {

    protected PropReaderInterface pri;
    protected final ArrayList<String> lines = new ArrayList<String>();
    protected final ArrayList<String> nfiles = new ArrayList<String>();
    protected int lineNo = 0;
    protected int startingLine = 0;

    protected static final int READ_AHEAD_LIMIT = 4096;

    protected final Map<String,String> defaults;
    protected final Map<String,String> types;
    protected final ArrayList<String> prefixes;

    public PropConfigFileParser(Map<String,String> defaults,
        Map<String,String> types,
        ArrayList<String> prefixes,
        String filename,
        PropReaderInterface pri) {
        
        this(defaults, types, prefixes, filename, "", pri);
    }
    
    public PropConfigFileParser(Map<String,String> defaults,
                                Map<String,String> types,
                                ArrayList<String> prefixes,
                                String filename,
                                String test,
                                PropReaderInterface pri) {
        this.pri = pri;
        this.defaults = defaults;
        this.types = types;
        this.prefixes = prefixes;

        FileInputStream istr = null;

        try {
            istr = new FileInputStream(filename);
        } catch(IOException e) {
            System.out.println("ERROR: " + e.getMessage());
        }

        /* parse the xml to just the properties */
        try {
            XmlUtils.parseXml(istr,
                              new XmlPropFileHandler(defaults,
                                                     types,
                                                     lines, 
                                                     nfiles,
                                                     prefixes,
                                                     test));
        } catch(BadPropertyException e) {
            System.out.println("ERROR: unable to parse " + 
                               filename + ": " + e.getMessage());
        }

        parseConfigFile();

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
        boolean continueReading = true;

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
                line = ((String) lines.remove(0)).trim();
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

    protected void parseConfigFile()
        throws PropConfigParserException {
        String line = null;

        /**
         * in order to get the precedence to be sane, we evaluate
         * from the inside out, so evaluate all files included by
         * this one recursively first before we apply properties
         * in this file, that way, each file can override any that
         * they include
         */

        while(nfiles.size() > 0) {
            String filename = (String) nfiles.remove(0);
            String evaluateFilename = pri.evaluateProperty(filename);

            new PropConfigFileParser(defaults, types, prefixes, evaluateFilename, pri);
        }

        while(true) {
            /* updated starting line number */
            startingLine = lineNo;

            line = readFilteredLine();

            /* quit on EOF */
            if(line == null)
                break;

            if(line.indexOf("=") != -1) {
                String key = line.substring(0, line.indexOf("=")).trim();
                String value = line.substring(line.indexOf("=") + 1).trim();

                if(key.length() == 0) {
                    throw new PropConfigParserException("line " + startingLine +
                                                        ": empty key in \"" + line + "\"");
                }

                pri.handleProperty(key, value);
            } else {
                pri.handleProperty(line, "");
            }
        } 
    }
}
