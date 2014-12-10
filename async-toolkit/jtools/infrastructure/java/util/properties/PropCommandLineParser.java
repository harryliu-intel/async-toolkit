package com.fulcrummicro.util.properties;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Map;

import com.fulcrummicro.util.cmdline.CommandLineIterator;
import com.fulcrummicro.util.cmdline.PropCommandLineIterator;
import com.fulcrummicro.util.cmdline.PropertyArg;

import com.fulcrummicro.util.properties.PropReaderInterface;

import com.fulcrummicro.util.misc.Utility;

/**
 * Parses command line arguments and returns 
 * them using the PropReaderInterface
 *
 * @author Naru Sundar
 */
public class PropCommandLineParser {

    protected PropReaderInterface pri;
    protected PropCommandLineIterator cli;

    protected final ArrayList<String> flist = new ArrayList<String>();
    protected final ArrayList<PropertyArg> plist = new ArrayList<PropertyArg>();

    protected final Map<String,String> defaults;
    protected final Map<String,String> types;
    protected final ArrayList<String> prefixes;

    public PropCommandLineParser(Map<String,String> defaults,
                                 Map<String,String> types,
                                 ArrayList<String> prefixes,
                                 String[] args,
                                 PropReaderInterface pri) {
        this.pri = pri;
        this.cli = new PropCommandLineIterator(Utility.toArrayList(args));

        this.defaults = defaults;
        this.types = types;
        this.prefixes = prefixes;
    }

    public ArrayList<String> parseCommandLine() {
        ArrayList<String> badArgList = new ArrayList<String>();

        while(cli.hasNext()) {
            PropertyArg arg = null;

            try {
                /* get next argument object */
                arg = (PropertyArg) cli.next();
            } catch(IllegalArgumentException e) {
                /* exception message is bad argument */
                badArgList.add(e.getMessage());
            }

            /* bad argument? continue */
            if(arg == null) {
                continue;
            }

            if(arg.getName().equals("config")) {
                /**
                 * we simply add the list of any commandline files
                 * to this list and evaluate them in evaluteFiles
                 * so that the PropReader can add the defaults and
                 * provide a basis for evaluating any substitution
                 * properties used in further nested includes
                 */
                flist.add(arg.getValue());
            } else {
                plist.add(arg);
            }
        }

        return badArgList;
    }

    public void evaluateConfigFiles() {
        for(int i = 0; i < flist.size(); i++) {
            String configFile = flist.get(i);

            try {
                String evaluatedConfigFile = pri.evaluateProperty(configFile);

                if(evaluatedConfigFile.contains(":")) {
                    String testplan = evaluatedConfigFile.substring(0,evaluatedConfigFile.indexOf(":"));
                    String test     = evaluatedConfigFile.substring(evaluatedConfigFile.indexOf(":")+1);

                    //System.err.println("*** testplan: " + testplan + " test: " + test);
                    new PropConfigFileParser(defaults, types, prefixes, 
                        testplan, test, pri);
                } else {
                    new PropConfigFileParser(defaults, types, prefixes, 
                        evaluatedConfigFile, pri);
                }
            } catch(ExpansionErrorException e) {
                System.out.println("ERROR: " + e.getMessage());
            }
        }
    }

    public void evaluateCommandLineProperties(boolean override) {
        for(int i = 0; i < plist.size(); i++) {
            PropertyArg arg = plist.get(i);

            pri.handleProperty(arg.getName(), arg.getValue(), override);
        }
    }

    public String searchPropList(String key) { 
        for(int i = 0; i < plist.size(); i++) {
            PropertyArg arg = plist.get(i);
            
            if(arg.getName().equals(key))
               return arg.getValue(); 
        }

        return null;
    }
}
