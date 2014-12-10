/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

// vim:sw=4:expandtab:ai:cin:ts=4

package com.avlsi.util.cmdline;


/** Interface between a module and the command line interpreter. */
public interface CmdModule {
    /** Returns the name of this module. Used to select this module to route commands to.*/
    String getName();
    /** Returns a help or usage message for this command/module */
    String getHelpMsg();
    /** Returns a more extensive help message describing this command/module **/
    String getExtendedHelpMsg();
    /** Returns a list of valid completions for the current command string. 
        start and end mark the boundaries of the word to complete. */
    String[] getCompletion(String cmd, int start, int end);
    /** Acts on a command passed in from the command line interpreter. */
    void execute(String args[]);
    /** To be called after the command completes (^C hook basically). **/
    void postExecute(String args[]);
    /** notifies the module that it has been loaded into another module. */
    void install(CmdModule parent, String args[]);
} 
