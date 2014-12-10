/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4

package com.avlsi.util.cmdline;

import java.io.FileReader;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.File;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Iterator;
import java.util.Vector;
import com.avlsi.util.container.TriMap;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.ArrayList;
import java.util.LinkedList;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.Node;
import com.avlsi.util.text.StringUtil;
import com.avlsi.util.readline.Readline;

import com.avlsi.util.classloader.ConfigurableClassLoader;

import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.CommandLineArg;

import com.avlsi.io.SearchPath;
import com.avlsi.io.FileSearchPath;

public class CmdLine implements CmdModule {

    /** Sample RSS at 20-second intervals. */
    private static final RssSampler rss_sampler = new RssSampler(20 * 1000);

    ///Loader for loading modules
    ConfigurableClassLoader  m_ModuleClassLoader;

    //Hashtable modules = new Hashtable();
    /** Mapping of command modules. **/
    TriMap modules = new TriMap();
    /** Module to route non-absolute commands to. **/
    CmdModule defaultModule;
    /** Whether or not to allow loading of other modules into this one. **/
    boolean allowLoad = false;
    /** Signal handler to catch interrupts and such. **/
    static Signal sig = Signal.get();
    /** List of handlers to be notified on interrupt. **/
    static Vector intHandlers = new Vector();
    /** Our interrupt handler that routes signals to all the other registered handlers. **/
    static Signal.Handler intHandler = new Signal.Handler() {
        public void execute() {
            for (int i=0; i<intHandlers.size(); i++) {
                Signal.Handler cur = (Signal.Handler) intHandlers.elementAt(i);
                cur.execute();
            }
        }
    };
    static {
        sig.setHandler(Signal.SIGINT, intHandler);
    }
    /** Registers a handler to be called on interrupt. **/
    public static void addIntHandler(Signal.Handler h) {
        intHandlers.add(h);
    }
    /** Removes an interrupt handler. **/
    public static void removeIntHandler(Signal.Handler h) {
        intHandlers.remove(h);
    }
    /** Basic commands for help, sourcing files, etc. **/
    CmdCommand basicCommands[] = {
        new CmdCommand("lm", "lm", "list loaded modules") {
            public void execute(String ags[]) {
                System.out.println("Loaded Commands/Modules:");
                listModuleHelp();
            }
        },
        new CmdCommand("source", "source <file>", "Take input from a file") {
            public void execute(String args[]) {
                if (!validateNumArgs(args, 1)) { return; }
                try {
                    BufferedReader in = new BufferedReader(new FileReader(args[0]));
                    String cur;
                    while((cur = in.readLine()) != null) {
                        cur = cur.trim();
                        routeCommand(cur);
                    }
                } catch(FileNotFoundException filenotfoundexception) {
                    System.err.println("Source File Not Found: " + args[0]);
                } catch(IOException ioexception) {
                    System.err.println("Error Reading File: " + args[0]);
                }
            }
        },

        new  CmdCommand("help", "help [command]", "Prints help information") {
            public void execute(String args[]) {
                if (args == null || args.length == 0) {
                    System.out.println("\nDefined commands:");
                    System.out.println("\t# comment\t\tIgnored (single line comment)");
                    System.out.println("\t[/module/]command ...\tIssua a command to the specified module");
                    listModuleHelp();
                    System.out.println("");
                }
                else {
                    if (args.length > 1) {
                        System.out.println("Usage: help [command]");
                        return;
                    }
                    String cmd = args[0].trim();
                    CmdModule mod = (CmdModule)modules.get(cmd);
                    if(mod != null) { 
                        System.out.println(mod.getExtendedHelpMsg());
                    }
                    else {
                        System.out.println("Module/command "+cmd+" not found.");
                    }
                }
            }
        },

        new CmdCommand("quit", "quit", "Exits the program") {
            public void execute(String args[]) {
                long javamem = 1+Runtime.getRuntime().totalMemory()/1048576;
                long kb = rss_sampler.getHWM();
                long osmem = 1 + kb / 1024;
                System.out.print("Recommend --max-heap-size at least ");
                System.out.print(javamem);
                System.out.print('M');
                if (kb != 0) {
                    System.out.print(", qsub mem at least ");
                    System.out.print(osmem);
                    System.out.print('M');
                }
                System.out.println();
                System.exit(0);
            }
        }
    };

    /** Commands for loading and removing modules, etc. **/
    CmdCommand modCommands[] = {
        new  CmdCommand("drop", "drop <module>", "Detaches a module by name") {
            public void execute(String args[]) { 
                if (validateNumArgs(args, 1)) { dropModule(args[0]); }
            }
        },
        new CmdCommand("load", "load <module>", "Loads a module by name") {
            public void execute(String args[]) {
                if (validateNumArgs(args, 1)) { loadModule(args[0]); }
            }
        },
        new CmdCommand("cm", "cm <module>", "Changes the default tool") {
            public void execute(String args[]) { 
                if (validateNumArgs(args, 1)) { 
                    if (args[0].equals("/")) { setDefault((CmdModule)null); }
                    else { setDefault(args[0]); }
                }
            }
        }
    };

    public CmdLine( boolean allowLoad ) {
	this( allowLoad, new FileSearchPath( "." ) );
    }

    /** Constructor: <code>allowLoad</code> determines whether the user can load/drop modules. **/
    public CmdLine(boolean allowLoad, SearchPath modulePath ) {
        //modules = new Hashtable();
        defaultModule = this;
	m_ModuleClassLoader = new ConfigurableClassLoader( CmdLine.class.getClassLoader(), modulePath ) ;
        CmdCommand b;
        this.allowLoad = allowLoad;
        if (allowLoad) {
             addCommands(modCommands);
        }
        addCommands(basicCommands);
    }
    /** Adds a list of commands to our command table. **/
    public void addCommands(CmdCommand list[]) {
        for (int i=0; i<list.length; i++) { addCommand(list[i]); }
    }
    /** Adds a command to our command table. **/
    public void addCommand(CmdCommand c) { modules.put(c.getName(), c); }

    /** Adds a module to our command table. **/
    public void addModule(CmdModule mod) {
        modules.put(mod.getName(), mod);
    }
    /** Removes a module from our command table. **/
    public void dropModule(CmdModule mod) {
        modules.remove(mod.getName());
    }
    /** Removes a module from our command table. **/
    public void dropModule(String name) {
        modules.remove(name);
    }
    /** Routes non-absolute commands to the given module. **/
    public void setDefault(CmdModule mod) {
        if (mod == null) { defaultModule = this; }
        else { defaultModule = mod; }
    }
    /** Routes non-absolute commands to the given module. **/
    public void setDefault(String modName) {
        if (modName == null) { defaultModule = this; }
        else { 
            CmdModule mod = (CmdModule)modules.get(modName);
            if (allowLoad && modName!=null && mod==null) { 
                // if we're allowed, try to load it...
                mod = loadModule(modName); 
                //mod = (CmdModule)modules.get(modName);
            }
            if (mod!=null && modName!=null) { System.out.println("Setting default module to "+modName); }
            setDefault(mod); 
        }
    }
    /** Returns the name of the module that commands are routed to by default, or 'none' **/
    public String getDefault() {
        return defaultModule == null ? "none" : defaultModule.getName();
    }
    /** Expands non-dotted names to look in this package with the suffix "Module". **/
    public String expandModuleName(String name) {
        return "com.avlsi.util.cmdline." + name + "Module";
    }
    /** Expands a module name (as nec.) and tries to load and install it. **/
    public CmdModule loadModule(String name) {
        String exp = expandModuleName(name);
	String error = "";
        try {
            CmdModule mod = (CmdModule)Class.forName(name, true, m_ModuleClassLoader ).newInstance();
            addModule(mod);
            System.out.println("Loaded module " + name);
            return mod;
        } 
	catch(ClassNotFoundException classnotfoundexception) {
	    error = classnotfoundexception.getMessage();
	}
	catch(Exception exception) {
            System.err.println("Load failed: Module " + name + " could not be instantiated.");
            exception.printStackTrace();
            return null;
        }
        try {
            CmdModule mod = (CmdModule)Class.forName(exp, true, m_ModuleClassLoader ).newInstance();
            addModule(mod);
            System.out.println("Loaded module " + exp);
            return mod;
        } 
	catch(ClassNotFoundException classnotfoundexception) {
        
	} 
	catch(Exception exception) {
            System.err.println("Load failed: Module " + exp + " could not be instantiated.");
            exception.printStackTrace();
            return null;
        }
        System.err.println("Load failed: neither Modules " + name + " nor " + exp + " found.\n" + error + "\n" );
        return null;
    }
    /** Print out help messages for all commands in table. **/
    public void listModuleHelp() {
        CmdModule c;
        Iterator i = modules.iterator();
        while (i.hasNext()) {
            c = (CmdModule)i.next();
            System.out.println(c.getHelpMsg());
        }
    }
    /** Sends absolute commands to top level, otherwise to default module. **/
    public void routeCommand(String cmd) {
        cmd = cmd.trim();
        if (cmd != null && cmd.length() > 0 && !cmd.startsWith("#")) {
            if(cmd.indexOf('/') == 0) {
                String args[] = CmdCommand.splitArgs(cmd.substring(1));
                execute(args);
            } else {
                if(defaultModule != null) { 
                    String args[] = CmdCommand.splitArgs(cmd);
                    defaultModule.execute(args);
                }
                else { System.err.println("No default module. Type '/help' for help"); }
            }
        }
    }

    public String getName() { return ""; }

    public String getHelpMsg() { return "/ top level module"; }
    public String getExtendedHelpMsg() { 
        return "\nModule /\n\n"+
               "    The top-level command-line module.  All other modules are loaded\n"+
               "    from this one.\n\n";
    }

    /** Returns a list of partial matches from the command table. **/
    public String[] getCompletion(String cmd, int start, int end) {
        // TODO implement this using the TriMap

        ArrayList list = new ArrayList();
        String[] ret = null;
        String prefix = "";
        // if starts with / start with this, otherwise defaultModule
        boolean root = cmd!=null && cmd.startsWith("/");
        boolean defaultValid = (defaultModule!=this && defaultModule!=null);
        if (cmd==null || (root && cmd.length()==1)) {
            // just return list of our commands
            boolean absolute = defaultValid || root;  
            CmdModule c;
            Iterator it = modules.iterator();
            while (it.hasNext()) {
                c = (CmdModule)it.next();
                if (absolute) { list.add("/"+c.getName()); }
                else { list.add(c.getName()); }
            }
            if (defaultValid && !root) {
                String[] def = defaultModule.getCompletion(null, start, end);
                for(int i=0; i<def.length; i++) { list.add(def[i]); }
            }
        } else if (defaultValid && !root) {
            // FIXME if the completion is before the args, don't pass on
            ret = defaultModule.getCompletion(cmd, start, end);
        } else {
            if (root) {
                cmd = cmd.substring(1);
                prefix +="/";
            }
            int len = cmd.length();
            int bar = cmd.indexOf('/'), spc = cmd.indexOf(' '), tab = cmd.indexOf('\t');
            int dlm = Math.min(bar<=0?bar:len, Math.min(spc>=0?spc:len, tab>=0?tab:len));
            String name = null;
            if (dlm>=0) { 
                name = cmd.substring(0, dlm); 
                prefix += cmd.substring(0, dlm+1);
            }
            if (name != null) {
                CmdModule mod = null; 
                mod = (CmdModule)modules.get(name);
                // step down hierarchy letting modules do completion
                if(mod != null) { 
                    // FIXME if the completion is before the args, don't pass on
                    ret = mod.getCompletion(cmd.substring(dlm+1), start-(dlm+1), end-(dlm+1)); 
                } else { return null; }
            } else {
                // only partial match
                CmdModule c;
                Iterator it = modules.iterator();
                while (it.hasNext()) {
                    String cname = ((CmdModule)it.next()).getName();
                    if (cname.startsWith(cmd)) { list.add(cname); }
                }
            }
        }
        int sz = list.size();
        if (sz>0) { ret = (String[])list.toArray(new String[sz]); }
        if ((prefix.length()>0) && (ret!=null))  {
            for (int i=ret.length-1; i>=0; i--) { ret[i] = prefix + ret[i]; }
        }
        return ret;
    }
    /** Completer that searches for partial matches in the command table. **/
    class CmdCompleter implements Readline.CompletionFunction {
        CmdModule module = null;
        String[] matches = null;
        int cur = 0;
        
        public CmdCompleter() { module = CmdLine.this; }
        public CmdCompleter(CmdModule mod) { module = mod; }
        public String run(String text, int state) {
            // generate list first time around
            if (state==0) { cur=0; matches = module.getCompletion(text, 0, text!=null ? text.length():0); }
            if (matches!=null && matches.length>0) {
                if (state!=0) { cur = (cur+1)%(matches.length+1); }
                if (cur==matches.length) { return ""; }
                //System.out.println("  "+matches[cur]);
                return matches[cur];
            }
            return "";
        }
    }
    class CmdAttemptCompleter implements Readline.AttemptCompletionFunction {
        CmdModule module = null;
        public CmdAttemptCompleter() { module = CmdLine.this; }
        public CmdAttemptCompleter(CmdModule mod) { module = mod; }
        public String[] run(String text, int start, int end) {
            //System.out.println("\n"+text); 
            return module.getCompletion(text, start, end); 
        }
    }

    /** Returns default completer to expand command line commands. **/
    //public CmdCompleter getCompleter() { return new CmdCompleter(); }
    public CmdAttemptCompleter getCompleter() { return new CmdAttemptCompleter(); }

    /** Does nothing currently. **/
    public void install(CmdModule parent, String args[]) {}
    /**  Breaks up the commands path components and routes arguments to the Appropriate module.  **/
    public void execute(String args[]) {
        if (args==null || args.length==0) { return; }
        int len = args.length;
        int dlm = args[0].indexOf('/', 1);
        String name = args[0];
        String newArgs[] = args;
        if (dlm>0) {
            name = args[0].substring(0,dlm);
            newArgs[0] = args[0].substring(dlm+1);
        } else if (len>1) {
            String tmp[] = new String[len-1];
            for (int i=0; i<len-1; i++) { tmp[i] = args[i+1]; }
            newArgs=tmp;
        } else if (len==1) { newArgs=null; }
        if (name != null) {
            CmdModule mod = (CmdModule)modules.get(name);
            if(mod == null) { mod = (CmdModule)modules.get(name); }
            if(mod != null) { 
                mod.execute(newArgs); 
                mod.postExecute(newArgs);       // post-execution hook
            }
            else {  System.err.println("Module/command " + name + " not found. Type '/help' for help"); }
        } else { System.err.println("Empty command: " + args[0]); }
    }

    /** Does nothing **/
    public void postExecute(String args[]) {}

    /** Returns string to prompt the user with. modname'>' by default. **/
    public String getPrompt() {
        String name = (defaultModule!=null) ? defaultModule.getName() : getName();
        return name + "> ";
    }

    /** Processes command line arguments, and loops over input, processing commands until done. **/
    public static void main(String args[]) {

        System.out.println("\nJCast command line tool (c)2001 AVLSI\nType 'help' or 'lm' for command info\n");
        rss_sampler.start();

        CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );
      
        CommandLineArgs theArgs = cachedArgs;
        
        boolean useReadline = ! theArgs.argExists( "no-readline" );
        
        final String defaultModule = theArgs.getArgValue( "module", null );

        final String fileToSource = theArgs.getArgValue( "source", null );
        
        SearchPath modulePath = new FileSearchPath( theArgs.getArgValue( "module-path", "." ) );

        final String historyFile = theArgs.getArgValue( "history-file", null );

        int historyFileMaxLength=300;
        try {
            historyFileMaxLength=
                Integer.parseInt
                 (theArgs.getArgValue
                  ("history-file-max-length", 
                   String.valueOf(historyFileMaxLength)));
        } catch(NumberFormatException e) {
            System.err.println("Warning: bad history file length: "+
                               theArgs.getArgValue("history-file-max-length","(none)"));
        }
        System.out.println("Using history file length: "+historyFileMaxLength);

        CommandLineArgsIterator cmdIter = theArgs.iterator();

        ArrayList defaultModuleArgsList = new ArrayList();

        while ( cmdIter.hasNext() ) {
            CommandLineArg curr = cmdIter.next();

            if ( ( curr.getName() != "no-readline" ) &&
                 ( curr.getName() != "module" )      &&
                 ( curr.getName() != "source" )      &&
                 ( curr.getName() != "module-path" ) && 
                 ( curr.getName() != "history-file" ) ) {
                
                String currStr= "--" + curr.getName();
                if ( curr.getValue() != null ) {
                    currStr = currStr + "=" + curr.getValue();
                }

                defaultModuleArgsList.add( currStr );

            }

        }
        
        CmdLine interpreter = new CmdLine(true, modulePath );   
        StringContainerIterator strIter = theArgs.nonParsedArgumentsIterator();
        
        while ( strIter.hasNext() ) {
            String currStr = strIter.next();

            if ( defaultModule == null ) {
                
                System.out.println( "Sourcing: " + currStr );
                
                interpreter.routeCommand( "/source " + currStr );
            }
            else {
                defaultModuleArgsList.add( currStr );
            }
        }
        
        String[] defaultModuleArgs = ( String[] ) defaultModuleArgsList.toArray( new String[1] );
     
        if ( defaultModule != null ) {
            interpreter.setDefault( defaultModule );
            interpreter.defaultModule.install(interpreter, defaultModuleArgs);
        }

        if ( fileToSource != null ) {
            try {
                interpreter.routeCommand( "/source " + fileToSource );
            } catch (Throwable t) {
                // TODO: refactor uncaught exception handling
                System.err.println("Uncaught exception");
                t.printStackTrace();
                System.err.println("Exiting");
                // Kill all other (possibly hanging) threads.
                System.exit(1);
            }
        }
        
        
        if (!useReadline) {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            try {
                while (true) {
                    System.out.print(interpreter.getPrompt());
                    String cur = in.readLine();
                    if (cur == null) { break; }
                    cur = cur.trim();
                    interpreter.routeCommand(cur);
                } 
            } catch(IOException ioexception) {
            } catch (Throwable t) {
                System.err.println("Uncaught exception");
                t.printStackTrace();
                System.err.println("Exiting");
                // Kill all other (possibly hanging) threads.
                System.exit(1);
            }
        } else {
            //CmdCompleter comp = interpreter.getCompleter();
            //Readline.rl_completion_entry_function(comp);

            CmdAttemptCompleter comp = interpreter.getCompleter();
            Readline.rl_completion_attempt_function(comp);
            Readline.rl_set_readline_name("jcast");

            // list of String, holds our version of the history since
            // we can't get it from readline
            final LinkedList myHistory;
            final boolean useHistoryFile;
            final File hf; // history file
            String lastCommand="";
            if(historyFile != null) {
                myHistory = new LinkedList();
                hf = new File(historyFile);
                boolean shouldUseHistoryFile;
                try {
                    if(hf.exists()) {
                        BufferedReader hr = new BufferedReader(new FileReader(hf));
                        String line;
                        while(true) {
                            line = hr.readLine();
                            if(line == null)
                                break;
                            // add contents of history file to our
                            // list, and to readline's history
                            if(!line.equals(lastCommand)) {
                                Readline.rl_add_history(line);
                                lastCommand = line;
                                myHistory.addLast(line);
                            }
                        }
                    } else {
                        hf.createNewFile();
                    }
                    shouldUseHistoryFile = true;
                } catch(IOException e) {
                    System.err.println("Error creating/reading history file");
                    shouldUseHistoryFile = false;
                }
                useHistoryFile = shouldUseHistoryFile;
                Runtime.getRuntime().addShutdownHook(new Thread() {
                        public void run() {
                            if(useHistoryFile) {
                                try {
                                    hf.delete();
                                    hf.createNewFile();
                                    BufferedWriter hw = new BufferedWriter(new FileWriter(hf));
                                    Iterator it=myHistory.iterator();
                                    while(it.hasNext()) {
                                        hw.write((String)it.next());
                                        hw.newLine();
                                    }
                                    hw.close();
                                } catch (IOException e) {
                                    System.out.println("Error writing history file");
                                }
                            }

                        }
                    });
            } else {
                useHistoryFile = false;
                hf = null;
                myHistory = null;
            }
            String cur;
            while (true) {
                cur = Readline.readline(interpreter.getPrompt());
                if (cur==null) { break; }
                if (cur.length()>0
                    && !cur.equals(lastCommand)) {
                    Readline.rl_add_history(cur);
                    lastCommand = cur;
                    if (useHistoryFile) {
                        myHistory.addLast(cur);
                        // limit total history lines
                        while(myHistory.size()>historyFileMaxLength)
                            myHistory.removeFirst();
                    }
                }
                try {
                    interpreter.routeCommand(cur);
                } catch (Throwable t) {
                    System.err.println("Uncaught exception");
                    t.printStackTrace();
                    System.err.println("Exiting");
                    // Kill all other (possibly hanging) threads.
                    System.exit(1);
                }
            }
        }
        
        // Kill all other (possibly hanging) threads.
        System.exit(0);
        System.out.println();
    }
    
}
