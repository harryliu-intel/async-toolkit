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

    protected PropCommandLineIterator cli;

    protected final ArrayList<String> flist = new ArrayList<String>();
    protected final ArrayList<PropertyArg> plist = new ArrayList<PropertyArg>();

    protected final Map<String, PropConfig> configs;
    protected final ArrayList<String> prefixes;

    public PropCommandLineParser(Map<String, PropConfig> configs,
                ArrayList<String> prefixes,                                 
                String[] args) {
        this.cli = new PropCommandLineIterator(Utility.toArrayList(args));
        this.configs = configs;
        this.prefixes = prefixes;
    }

    public ArrayList<String> parseCommandLine() {
        ArrayList<String> badArgList = new ArrayList<String>();

        while(cli.hasNext()) {
            PropertyArg arg = null;

            try {
                /* get next argument object */
                arg = cli.next();
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
                for (String cfile : arg.getValue().split(",")) {
                    flist.add(cfile);
                }
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
                if(configFile.contains(":")) {
                    int index = configFile.indexOf(":");
                    String testplan = configFile.substring(0, index);
                    String test = configFile.substring(index+1);

                    new PropConfigFileParser(configs, prefixes, 
                        testplan, test);
                } else {
                    new PropConfigFileParser(configs, prefixes, 
                        configFile);
                }
            } catch(ExpansionErrorException e) {
                System.out.println("ERROR: " + e.getMessage());
            }
        }
    }
    
    public ArrayList<PropertyArg> getPlist() {
        return plist;
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
