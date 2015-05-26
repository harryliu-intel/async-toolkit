/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.util.rename;


import java.io.PrintStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import java.util.Collection;
import java.util.StringTokenizer;

import com.avlsi.tools.jauto.PartialExtract;
import com.avlsi.util.container.Triplet;

import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.GDS2NameInterface;
import com.avlsi.file.cdl.util.rename.GDS2ReverseNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CadenceReverseNameInterface;
import com.avlsi.file.cdl.util.rename.IdentityNameInterface;
import com.avlsi.file.cdl.util.rename.CompositeCDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.cdl.util.rename.MWNameInterface;
import com.avlsi.file.cdl.util.rename.MWReverseNameInterface;

import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.defimpl.PedanticCommandLineArgs;

/**
 *  A java version of rename.  Translates lines from stdin using the CDLRenameInterfaces.
 **/

public class Rename {

    public static void usagej() {
        final String className = CDLRenamer.class.getName();

        System.err.print( "Usage: " +
                            System.getProperty( "java.home" ) +
                            System.getProperty( "file.separator" ) +
                            "bin" +
                            System.getProperty( "file.separator" ) +
                            "java " +
                            " -classpath " +
                            System.getProperty( "java.class.path" ) + " " +
                            className + "\n" +
                            "--type=[cell|node|instance|model|all]\n" +
                            "--from=[cast,cadence,gds2]\n" +
                            "--to=[cast,cadence,gds2]\n" );
    }

    public static void usage( String m ) {

        System.err.print( "Usage: rename\n" +
                            "  --type=[cell|node|instance|model|all]\n" +
                            "  --from=[cast,cadence,gds2]\n" +
                            "  --to=[cast,cadence,gds2]\n" );
        if (m != null && m.length() > 0)
            System.err.print( m );
    }

    public static void usage() {
        usage( null );
    }

    private interface RenameAction  {
        String act(String in) throws CDLRenameException;
    }

    private static RenameAction getActionByType(final String type,
                                                final CDLNameInterface theNI) {
        final RenameAction act;
        if(type.equals("cell"))
            act = new RenameAction() { public String act(String in) 
                                              throws CDLRenameException { return theNI.renameCell(in); } };
        else if(type.equals("node"))
            act = new RenameAction() { public String act(String in) 
                                              throws CDLRenameException { return theNI.renameNode(in); } };
        else if(type.equals("instance"))
            act = new RenameAction() { public String act(String in) 
                                              throws CDLRenameException { return theNI.renameSubCellInstance(in); } };
        else if(type.equals("model"))
            act = new RenameAction() { public String act(String in) 
                                              throws CDLRenameException { return theNI.renameTransistorModel(in); } };
        else {
            act = null;
        }
        return act;
    }

    private static CDLNameInterface identityNI;
    private static CDLNameInterface cadenceNI;
    private static CDLNameInterface gds2NI;
    private static CDLNameInterface mwNI;
    private static CDLNameInterface cadenceReverseNI;
    private static CDLNameInterface gds2ReverseNI;
    private static CDLNameInterface mwReverseNI;

    private static CDLNameInterface getForwardInterface(final String name) {
        final CDLNameInterface ni;
        if(name.equals("cast")) {
            if (identityNI == null) identityNI = new IdentityNameInterface();
            ni = identityNI;
        } else if(name.equals("cadence")) {
            if (cadenceNI == null) cadenceNI = new CadenceNameInterface();
            ni = cadenceNI;
        } else if(name.equals("gds2")) {
            if (gds2NI == null) gds2NI = new GDS2NameInterface();
            ni = gds2NI;
        } else if(name.equals("mw")) {
            if (mwNI == null) mwNI = new MWNameInterface();
            ni = mwNI;
        } else {
            ni = null;
        }
        return ni;
    }

    private static CDLNameInterface getReverseInterface(final String name) {
        final CDLNameInterface ni;
        if(name.equals("cast")) {
            if (identityNI == null) identityNI = new IdentityNameInterface();
            ni = identityNI;
        } else if(name.equals("cadence")) {
            if (cadenceNI == null) cadenceReverseNI = new CadenceReverseNameInterface();
            ni = cadenceReverseNI;
        } else if(name.equals("gds2")) {
            if (gds2NI == null) gds2ReverseNI = new GDS2ReverseNameInterface();
            ni = gds2ReverseNI;
        } else if(name.equals("mw")) {
            if (mwReverseNI == null) mwReverseNI = new MWReverseNameInterface();
            ni = mwReverseNI;
        } else {
            ni = null;
        }
        return ni;
    }

    private static CDLNameInterface getInterface(final String from, final String to) {
        final CDLNameInterface toCast = getReverseInterface(from);
        final CDLNameInterface fromCast = getForwardInterface(to);
        if (toCast == null || fromCast == null) {
            return null;
        } else {
            return new CompositeCDLNameInterface(toCast, fromCast);
        }
    }

    public static void main(String[] args)
        throws Exception
    {
        final CommandLineArgs parsedArgs =
            new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs );
        final CommandLineArgs cachedArgs =
            new CachingCommandLineArgs( argsWithConfigs );
        final PedanticCommandLineArgs pedanticArgs =
            new PedanticCommandLineArgs( cachedArgs );
        final CommandLineArgs theArgs = pedanticArgs;

        final String from =
            theArgs.getArgValue( "from", null );

        final String to =
            theArgs.getArgValue( "to", null );

        final String type =
            theArgs.getArgValue( "type", null );

        pedanticArgs.argTag( "from" );
        pedanticArgs.argTag( "to" );
        pedanticArgs.argTag( "type" );
        pedanticArgs.argTag( "partial" );
        pedanticArgs.argTag( "partial-subtype" );

        if ( ! pedanticArgs.pedanticOK( false, true ) ) {
            usage();
            throw new CDLRenameException( pedanticArgs.pedanticString() );
        }

        final PrintStream out = System.out;
        final BufferedReader in = new BufferedReader( new InputStreamReader(System.in));
        if (type != null && type.equals("all")) {

            String line;
            StringTokenizer tk = null;
            String ltype = "cell";
            String lfrom = "cast";
            String lto   = "cast";
            String lname = "";
            String lx = "";
            RenameAction theAct;
            int cnt;
            while((line = in.readLine()) != null) {
                tk = new StringTokenizer( line, " ");
                cnt = 0;
                if (tk.hasMoreTokens()) { lx = tk.nextToken(); cnt++; }
                if (tk.hasMoreTokens()) { lfrom = tk.nextToken(); cnt++; }
                if (tk.hasMoreTokens()) { lto = tk.nextToken(); cnt++; }
                if (tk.hasMoreTokens()) { lname = tk.nextToken(); cnt++; }
                if (cnt == 1) lname = lx;
                if (cnt > 1) ltype = lx;
                if (cnt == 1 || cnt == 4) {
                    final CDLNameInterface ni = getInterface(lfrom, lto);
                    if (ni == null || (theAct = getActionByType(ltype, ni)) == null) {
                        theAct = new RenameAction() {
                            public String act(String in) throws CDLRenameException {
                                return in;
                            }
                        };
                    }
                    out.println( theAct.act( lname ));
                }
            }
        }
        else {
            if(from == null || to == null || type == null) {
                usage();
                throw new CDLRenameException("Must specify a valid from,to name-system and type");
            }

            final CDLNameInterface ni = getInterface(from, to);
            if(ni == null) {
                usage();
                throw new CDLRenameException("Must specify a valid from,to name-system");
            }

            final CDLNameInterface theNI;
            if(theArgs.argExists( "partial" ) || 
               theArgs.argExists( "partial-subtype" )) {
                final CDLNameInterface partialNI = new IdentityNameInterface() {
                        public String renameCell(String in) {
                            final Triplet triplet = PartialExtract.parseCellPlusMinus(in);
                            String ret = (String) triplet.getFirst();
                            if(theArgs.argExists( "partial-subtype") && 
                               !((Collection)triplet.getThird()).isEmpty() ) {
                                ret += ".0";
                            }
                            return ret;
                        }
                    };
                theNI = new CompositeCDLNameInterface(partialNI, ni);
            }
            else {
                theNI = ni;
            }
                
            final RenameAction act = getActionByType(type, theNI);

            if(act == null) {
                usage();
                throw new CDLRenameException("Must specify a valid type");
            }

            final RenameAction theAct = act;

            String line;
            while((line = in.readLine()) != null) {
                out.println( theAct.act(line.trim()));
            }
        }
    }
}
