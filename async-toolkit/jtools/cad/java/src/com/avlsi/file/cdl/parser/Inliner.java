/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.io.FileReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.util.Map;

import com.avlsi.file.common.HierName;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.text.StringUtil;

public class Inliner {
    private static void usage() {
        System.err.print(
"Usage: java com.avlsi.file.cdl.parser.Inliner\n" +
"   --cdl=<file> (File to be inlined)\n" +
"   --library=<file> (Cells to inline)\n" +
"   [ --cells=<cell:cell:...> ] (Only inline listed cells)\n"
        );
        System.exit(1);
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 

        final CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;

        final String cdlFile = theArgs.getArgValue("cdl", null);
        final String cdlLibrary = theArgs.getArgValue("library", null);
        if (cdlFile == null || cdlLibrary == null) usage();

        final CDLInlineFactory iline = new CDLInlineFactory(true);
        final Reader libraryReader = new FileReader(cdlLibrary);
        final String cells = theArgs.getArgValue("cells", null);
        if (cells == null) {
            iline.addTarget(libraryReader);
        } else {
            final String[] targets = StringUtil.split(cells, ':');
            iline.addTarget(libraryReader, targets);
        }

        final PrintWriter w =
            new PrintWriter(new OutputStreamWriter(System.out));
        final CDLFactoryInterface emitter =
            new CDLFactoryEmitter(w, true, 79, true, true);

        iline.process(new FileReader(cdlFile), emitter);
        w.close();
    }
}
