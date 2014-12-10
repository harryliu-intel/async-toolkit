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

        if (type != null && type.equals("all")) {

            final CDLNameInterface nicast2cadence = new CadenceNameInterface();
            final CDLNameInterface nicast2gds2 = new GDS2NameInterface();
            final CDLNameInterface nicadence2cast = new CadenceReverseNameInterface();
            final CDLNameInterface nicadence2gds2 = new CompositeCDLNameInterface(new CadenceReverseNameInterface(), 
                                                       new GDS2NameInterface());
            final CDLNameInterface nigds22cast = new GDS2ReverseNameInterface();
            final CDLNameInterface nigds22cadence = new CompositeCDLNameInterface(new GDS2ReverseNameInterface(),
                                                       new CadenceNameInterface());
            final RenameAction actsame = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return in;}};
            final RenameAction actcast2cadence4cell = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicast2cadence.renameCell(in);}};
            final RenameAction actcast2cadence4node = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicast2cadence.renameNode(in);}};
            final RenameAction actcast2cadence4instance = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicast2cadence.renameSubCellInstance(in);}};
            final RenameAction actcast2cadence4model = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicast2cadence.renameTransistorModel(in);}};
            final RenameAction actcast2gds24cell = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicast2gds2.renameCell(in);}};
            final RenameAction actcast2gds24node = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicast2gds2.renameNode(in);}};
            final RenameAction actcast2gds24instance = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicast2gds2.renameSubCellInstance(in);}};
            final RenameAction actcast2gds24model = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicast2gds2.renameTransistorModel(in);}};
            final RenameAction actcadence2cast4cell = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicadence2cast.renameCell(in);}};
            final RenameAction actcadence2cast4node = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicadence2cast.renameNode(in);}};
            final RenameAction actcadence2cast4instance = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicadence2cast.renameSubCellInstance(in);}};
            final RenameAction actcadence2cast4model = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicadence2cast.renameTransistorModel(in);}};
            final RenameAction actcadence2gds24cell = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicadence2gds2.renameCell(in);}};
            final RenameAction actcadence2gds24node = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicadence2gds2.renameNode(in);}};
            final RenameAction actcadence2gds24instance = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicadence2gds2.renameSubCellInstance(in);}};
            final RenameAction actcadence2gds24model = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nicadence2gds2.renameTransistorModel(in);}};
            final RenameAction actgds22cast4cell = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nigds22cast.renameCell(in);}};
            final RenameAction actgds22cast4node = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nigds22cast.renameNode(in);}};
            final RenameAction actgds22cast4instance = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nigds22cast.renameSubCellInstance(in);}};
            final RenameAction actgds22cast4model = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nigds22cast.renameTransistorModel(in);}};
            final RenameAction actgds22cadence4cell = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nigds22cadence.renameCell(in);}};
            final RenameAction actgds22cadence4node = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nigds22cadence.renameNode(in);}};
            final RenameAction actgds22cadence4instance = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nigds22cadence.renameSubCellInstance(in);}};
            final RenameAction actgds22cadence4model = new RenameAction() {
                public String act(String in) throws CDLRenameException {
                    return nigds22cadence.renameTransistorModel(in);}};

            final PrintStream out = System.out;
            final BufferedReader in = new BufferedReader( new InputStreamReader(System.in));

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
                    theAct = actsame;
                    if (ltype.equals("cell")) {
                        if (lfrom.equals("cast")) {
                            if (lto.equals("cadence")) {
                                theAct = actcast2cadence4cell;
                            }
                            else if (lto.equals("gds2")) {
                                theAct = actcast2gds24cell;
                            }
                        }
                        else if (lfrom.equals("cadence")) {
                            if (lto.equals("cast")) {
                                theAct = actcadence2cast4cell;
                            }
                            else if (lto.equals("gds2")) {
                                theAct = actcadence2gds24cell;
                            }
                        }
                        else if (lfrom.equals("gds2")) {
                            if (lto.equals("cast")) {
                                theAct = actgds22cast4cell;
                            }
                            else if (lto.equals("cadence")) {
                                theAct = actgds22cadence4cell;
                            }
                        }
                    }
                    else if (ltype.equals("node")) {
                        if (lfrom.equals("cast")) {
                            if (lto.equals("cadence")) {
                                theAct = actcast2cadence4node;
                            }
                            else if (lto.equals("gds2")) {
                                theAct = actcast2gds24node;
                            }
                        }
                        else if (lfrom.equals("cadence")) {
                            if (lto.equals("cast")) {
                                theAct = actcadence2cast4node;
                            }
                            else if (lto.equals("gds2")) {
                                theAct = actcadence2gds24node;
                            }
                        }
                        else if (lfrom.equals("gds2")) {
                            if (lto.equals("cast")) {
                                theAct = actgds22cast4node;
                            }
                            else if (lto.equals("cadence")) {
                                theAct = actgds22cadence4node;
                            }
                        }
                    }
                    else if (ltype.equals("instance")) {
                        if (lfrom.equals("cast")) {
                            if (lto.equals("cadence")) {
                                theAct = actcast2cadence4instance;
                            }
                            else if (lto.equals("gds2")) {
                                theAct = actcast2gds24instance;
                            }
                        }
                        else if (lfrom.equals("cadence")) {
                            if (lto.equals("cast")) {
                                theAct = actcadence2cast4instance;
                            }
                            else if (lto.equals("gds2")) {
                                theAct = actcadence2gds24instance;
                            }
                        }
                        else if (lfrom.equals("gds2")) {
                            if (lto.equals("cast")) {
                                theAct = actgds22cast4instance;
                            }
                            else if (lto.equals("cadence")) {
                                theAct = actgds22cadence4instance;
                            }
                        }
                    }
                    else if (ltype.equals("model")) {
                        if (lfrom.equals("cast")) {
                            if (lto.equals("cadence")) {
                                theAct = actcast2cadence4model;
                            }
                            else if (lto.equals("gds2")) {
                                theAct = actcast2gds24model;
                            }
                        }
                        else if (lfrom.equals("cadence")) {
                            if (lto.equals("cast")) {
                                theAct = actcadence2cast4model;
                            }
                            else if (lto.equals("gds2")) {
                                theAct = actcadence2gds24model;
                            }
                        }
                        else if (lfrom.equals("gds2")) {
                            if (lto.equals("cast")) {
                                theAct = actgds22cast4model;
                            }
                            else if (lto.equals("cadence")) {
                                theAct = actgds22cadence4model;
                            }
                        }
                    }
                    out.println( theAct.act( lname ));
                }
            }
        }
        else {
            CDLNameInterface ni = null;

            if(from == null || to == null || type == null) {
                usage();
                throw new CDLRenameException("Must specify a valid from,to name-system and type");
            }

            if(from.equals("cast")) {
                if(to.equals("cast"))
                    ni = new IdentityNameInterface();
                else if(to.equals("cadence"))
                    ni = new CadenceNameInterface();
                else if(to.equals("gds2"))
                    ni = new GDS2NameInterface();            
            }
            else if(from.equals("cadence")) {
                if(to.equals("cast"))
                    ni = new CadenceReverseNameInterface();
                else if(to.equals("cadence"))
                    ni = new IdentityNameInterface();
                else if(to.equals("gds2"))
                    ni = new CompositeCDLNameInterface(new CadenceReverseNameInterface(), 
                                                       new GDS2NameInterface());
            }
            else if(from.equals("gds2")) {
                if(to.equals("cast"))
                    ni = new GDS2ReverseNameInterface();
                else if(to.equals("cadence"))
                    ni = new CompositeCDLNameInterface(new GDS2ReverseNameInterface(),
                                                       new CadenceNameInterface());
                else if(to.equals("gds2"))
                    ni = new IdentityNameInterface();
            }
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
                
            final PrintStream out = System.out;
            final BufferedReader in = new BufferedReader( new InputStreamReader(System.in));
            RenameAction act = null;

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
