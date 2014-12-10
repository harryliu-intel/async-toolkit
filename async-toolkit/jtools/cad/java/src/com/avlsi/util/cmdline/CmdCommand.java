/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4

package com.avlsi.util.cmdline;

import java.util.StringTokenizer;
import java.util.ArrayList;
import com.avlsi.util.text.StringUtil;

/**
 * Base for CmdModule internal commands. Derived from CmdModule so commands and
 * modules can be handled uniformly.
 **/
public class CmdCommand implements CmdModule {
    String name, usage, desc, pad;
    String shortUsage;
    String extDesc;

    /**
     * Constructs a command with the given name, ussage message, and help
     * description.
     **/ 
    public CmdCommand(String _name, String _usage, String _desc) { 
        name=_name; usage=_usage; desc=_desc; 
        setShortUsage();
    }

    /** 
     * Constructs a command with the given name, ussage message, help 
     * description, and extended help description. 
     **/ 
    public CmdCommand(String _name, String _usage, String _desc, 
                      String _extDesc) { 
        name=_name; usage=_usage; desc=_desc; extDesc = _extDesc;
        setShortUsage();
    }

    /** Sets the shortUsage string. **/
    private void setShortUsage() {
        // Cut off the usage string at 22 characters so that the
        // abbreviated command listing stays clean & well-formatted.
        // Also, supply a default usage string if none is given.
        if (usage == null) {
            if (name == null) { name = "[NO NAME SPECIFIED]"; }
            usage = name;
        }
        usage = usage.trim();
        int idx = usage.indexOf(" ");
        if (usage.length() > 22 && idx > 0) {
            shortUsage = usage.substring(0,idx) + " ...";
        }
        else {
            shortUsage = usage;
        }
        int ulen = shortUsage.length();
        pad = StringUtil.repeatString(" ", Math.max(24-ulen, 2));
    }

    /** Strips off last piece of command class name. Convenience method. **/
    public String getDefaultName() { 
        String name = getClass().getName();
        name = name.substring(name.lastIndexOf('$')+1).toLowerCase();
        return name; 
    }

    public String getName() { return name; }

    /** Returns a (hopefully) informative message about the command. **/
    public String getHelpMsg() { 
        return "\t"+shortUsage+pad+desc; 
    }

    /** Returns the extended description of the command. **/
    public String getExtendedHelpMsg() {
        String text = extDesc;
        if (extDesc == null || extDesc.length() == 0) { 
            if (!desc.endsWith(".")) {
                text = desc + ".";
            }
            else {
                text = desc;
            }
        }
        return "\nUsage: "+usage+"\n\n    "+
               StringUtil.replaceSubstring(text,"\n","\n    ")+"\n\n";
    }

    /**
     * Returns the usuage string (should be used to remind the user of the
     * correct command syntax).
     **/
    public String getUsage() { return usage; }

    /** Return a list of possible completions for the given command-line. **/
    public String[] getCompletion(String cmd, int start, int end) {
        return null;
    }

    /**
     * Notification that we've been installed in a module, with command line
     * arguments.  Not generally called for commands.
     **/
    public void install(CmdModule parent, String args[]) {}

    /** Splits args into a list of strings broken at occurences of delim. **/
    public static String[] splitArgs(String args, String delim) { 
        return splitArgs(args, delim, false);
    }

    /**
     * Splits args into a list of strings broken at occurences of delim.  If
     * <code>usedelim</code> is set delimiters are included in the list.
     **/
    public static String[] splitArgs(String args, String delim,
                                     boolean usedelim) { 
        String ret[] = null;
        if (args==null) { return null; }
        StringTokenizer tok = new StringTokenizer(args, delim, usedelim);
        int len = tok.countTokens();
        ret = new String[len];
        for (int i=0; i<len; i++) { ret[i]=tok.nextToken(); }
        return ret; 
    }

    /** Breaks up arguments first by quoted strings, then by whitespace. **/
    public static String[] splitArgs(String args) { 
        String qsplit[] = splitArgs(args, "\"", true);
        ArrayList split = new ArrayList();
        int len = (qsplit!=null) ? qsplit.length : 0;
        for (int i=0; i<len; i++) {
            if (qsplit[i].startsWith("\"")) {
                i++;
                if (i<len) { split.add(qsplit[i]); }
                else { split.add(""); }
                i++; // skip next quote
                continue;
            }
            String cur[] = splitArgs(qsplit[i], " \t", false);
            int clen = (cur!=null) ? cur.length : 0;
            for (int j=0; j<clen; j++) { split.add(cur[j]); }
                
        }
        if (split.size()<1) { return null; }
        return (String[])split.toArray(new String[split.size()]); 
    } 

    /** Verify the number of arguments or print an error. **/
    public boolean validateNumArgs(String args[], int n) {
        int num = args==null ? 0 : args.length;
        if (num==n) { return true; }
        System.err.println("Wrong number ["+num+"] of args to "+getName()+
                           "...usage: ");
        System.err.println(getHelpMsg());
        return false;
    }

    /** List all of our arguments, for debugging. **/
    public void dumpArgs(String args[]) {
        System.out.print(getName()+":");
        if (args==null) { System.out.println(); return; }
        for (int i=0; i<args.length; i++) { System.out.print(" "+args[i]); }
        System.out.println();
    }

    /** Converts args[n] to a int, or throws exception. **/
    public int validateIntArg(String args[], int n) throws Exception {
        return validateIntArg(args, n, false);
    }

    /**
     * Converts args[n] to a int, or throws exception.  Prints an error message
     * unless suppresed by <code>noMsg</code>.
     **/
    public int validateIntArg(String args[], int n, boolean noMsg)
        throws Exception {
        String s="";
        try { s=args[n]; return Integer.parseInt(s); }
        catch (Exception e) {
            if (!noMsg) { System.err.println("Non Integer Argument #"+n+" ["+s+
                                             "] to "+getName()); }
            throw e;
        }
    }

    /** Converts args[n] to a int if possible, otherwise returns _default. **/
    public int getIntArg(String args[], int n, int _default) {
        try { return validateIntArg(args, n, true); } catch (Exception e) {}
        return _default;
    }

    /** Converts args[n] to a int, or throws exception. **/
    public long validateLongArg(String args[], int n) throws Exception {
        return validateLongArg(args, n, false);
    }

    /**
     * Converts args[n] to a int, or throws exception.  Prints an error message
     * unless suppresed by <code>noMsg</code>.
     **/
    public long validateLongArg(String args[], int n, boolean noMsg)
        throws Exception {
        String s="";
        try { s=args[n]; return Long.parseLong(s); }
        catch (Exception e) {
            if (!noMsg) { System.err.println("Non Integer Argument #"+n+" ["+s+
                                             "] to "+getName()); }
            throw e;
        }
    }

    /** Converts args[n] to a int if possible, otherwise returns _default. **/
    public long getLongArg(String args[], int n, long _default) {
        try { return validateLongArg(args, n, true); } catch (Exception e) {}
        return _default;
    }


    /** Converts args[n] to a float, or throws exception. **/
    public float validateFloatArg(String args[], int n) throws Exception {
        return validateFloatArg(args, n, false);
    }

    /**
     * Converts args[n] to a float, or throws exception.  Prints an error
     * message unless suppresed by <code>noMsg</code>.
     **/
    public float validateFloatArg(String args[], int n, boolean noMsg)
        throws Exception {
        String s="";
        try { s=args[n]; return Float.parseFloat(s); }
        catch (Exception e) {
            if (!noMsg) { System.err.println("Non FLoat Argument #"+n+" ["+s+
                                             "] to "+getName()); }
            throw e;
        }
    }

    /** Converts args[n] to a float if possible, otherwise returns _default. **/
    public float getFloatArg(String args[], int n, float _default) {
        try { return validateFloatArg(args, n, true); } catch (Exception e) {}
        return _default;
    }

    /** Converts args[n] to a boolean, or throws exception. **/
    public boolean validateBooleanArg(String args[], int n) throws Exception {
        return validateBooleanArg(args, n, false);
    }

    /**
     * Converts args[n] to a boolean, or throws exception. Prints an error
     * message unless suppresed by <code>noMsg</code>.
     **/
    public boolean validateBooleanArg(String args[], int n, boolean noMsg)
        throws Exception {
        Exception thrown = null;
        String s="";
        try {
            s = args[n];
            if (s.equals("true") || s.equals("yes") || s.equals("1") ||
                s.equals("on")) { 
                return true; 
            }
            if (s.equals("false") || s.equals("no") || s.equals("0") ||
                s.equals("off")) { 
                return false; 
            }
        } catch (Exception e) {
            thrown = e;
        }
        if (!noMsg) { System.err.println("Non boolean Argument #"+n+" ["+s+
                                         "] to "+getName()); }
        if (thrown==null) { thrown = new IllegalArgumentException(); }
        throw thrown;
    }

    /**
     * Converts args[n] to a boolean if possible, otherwise returns _default.
     **/
    public boolean getBooleanArg(String args[], int n, boolean _default) {
        try { return validateBooleanArg(args, n, true); } catch (Exception e) {}
        return _default;
    }

    /** Override this in derived classes to do actual work. **/
    public void execute(String args[]) {}

    /**
     * Override this for any command-cleanup (e.g. to print out status after
     * a ^C in DSim).  Will receive the same arguments as the corresponding
     * execute().
     **/
    public void postExecute(String args[]) {}
} 
