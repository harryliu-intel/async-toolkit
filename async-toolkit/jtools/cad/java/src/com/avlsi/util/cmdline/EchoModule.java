/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4

package com.avlsi.util.cmdline;

/** Dummy module for testing */
public class EchoModule implements CmdModule {
    public String getName() { return "Echo"; }
    public String getHelpMsg() { return "\tEcho/\t\t\tdummy test module"; }
    public String getExtendedHelpMsg() { return "What's this silly thing for?"; }
    public String[] getCompletion(String cmd, int start, int end) { return null; }
    public void install(CmdModule parent, String args[]) {}
    public void execute(String args[]) {
        System.out.print("Echo:");
        int len = args!=null ? args.length : 0;
        for (int i=0; i<len; i++) {
            System.out.print(" "+args[i]);
        }
        System.out.println();
    }
    public void postExecute(String args[]) {}
} 
