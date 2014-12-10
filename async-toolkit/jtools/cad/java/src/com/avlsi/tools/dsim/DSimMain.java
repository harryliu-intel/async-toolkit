/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.dsim;

import com.avlsi.util.cmdline.CmdLine;

import java.io.File;
import java.util.ArrayList;


public class DSimMain {
    /**
     * This class should not be instantiated.
     **/
    private DSimMain() { }

    public static void main( String[] args ) {
        final String myArgs[] = new String[args.length + 2];


        myArgs[0] = "--module=DSim" ;
        myArgs[1] = "--history-file="+System.getProperty("user.home")+
            File.separatorChar+".jdsim_hist";

        int i = 0;
        while ( ( i < args.length ) && ( ! args[i].equals("--help") ) ) {
            
            myArgs[i+2] = args[i];
            
            ++i;
        }

        //If we exitted the above loop because we saw a --help
        if ( i < args.length ) {
            System.out.println( DSimMain.class.getName() + " [--cast-version=(1|2)] [--cast-path=path] [--device-path=path]" );
        }
        else {
            CmdLine.main( myArgs );
        }
    }

}
