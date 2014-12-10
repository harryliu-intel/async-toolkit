/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 */

package com.avlsi.csp.csp2xml;

import com.avlsi.fast.ports.ArrayType;
import com.avlsi.csp.ast.CSPProgram;
import com.avlsi.csp.util.CSPCellInfo;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast.CastSyntaxException;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.ports.ChannelType;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.io.FileSearchPath;
import java.io.IOException;
import java.util.Iterator;
import com.avlsi.fast.ports.NodeType;
import java.io.OutputStreamWriter;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.fast.ports.PortTypeInterface;
import java.io.PrintWriter;
import com.avlsi.fast.ports.StructureType;
import com.avlsi.csp.ast.VisitorException;

public class Csp2Xml {
    private static void printPort(XmlTourist x, PortDefinition port) {
        int dir = port.getDirection();
        String dirStr = "unknown";
        switch (dir) {
        case PortDefinition.NONE:
            dirStr = "none";
            break;
        case PortDefinition.IN:
            dirStr = "in";
            break;
        case PortDefinition.OUT:
            dirStr = "out";
            break;
        case PortDefinition.INOUT:
            dirStr = "inout";
            break;
        }
        x.beginTag("Port name=\"" + port.getName() + "\" dir=\"" +
                   dirStr + "\"");
        printType(x, port.getType());
        x.endTag("Port");
    }
           
    private static void printType(XmlTourist x, PortTypeInterface t) {
        if (t instanceof ChannelType) {
            ChannelType c = (ChannelType) t;
            StringBuffer buf = new StringBuffer("<ChannelType type=\"");
            buf.append(c.getTypeName());
            buf.append('\"');
            if (c.isArrayed())
                buf.append(" width=\"" + c.getWidth() + "\"");
            buf.append("/>");
            x.print(buf.toString());
        } else if (t instanceof NodeType) {
            NodeType n = (NodeType) t;
            StringBuffer buf = new StringBuffer("<NodeType");
            if (n.isArrayed())
                buf.append(" width=\"" + n.getWidth() + "\"");
            buf.append("/>");
            x.print(buf.toString());
        } else if (t instanceof ArrayType) {
            ArrayType a = (ArrayType) t;
            x.beginTag("ArrayType");
            x.beginTag("min");
            x.print("<integer val=\"" + a.getMinIndex() + "\" base=\"10\"/>");
            x.endTag("min");
            x.beginTag("max");
            x.print("<integer val=\"" + a.getMaxIndex() + "\" base=\"10\"/>");
            x.endTag("max");
            x.beginTag("elementType");
            printType(x, a.getArrayedType());
            x.endTag("elementType");
            x.endTag("ArrayType");
        } else if (t instanceof StructureType) {
            StructureType s = (StructureType) t;
            StringBuffer buf = new StringBuffer("StructureType");
            if (s.getTag() != null) {
                buf.append(" type=\"");
                buf.append(s.getTag());
                buf.append('\"');
            }
            x.beginTag(buf.toString());
            for (Iterator it = s.iterator(); it.hasNext(); ) {
                PortDefinition port = (PortDefinition) it.next();
                printPort(x, port);
            }
            x.endTag("StructureType");
        } else {
            throw new RuntimeException("unknown port type " +
                                       t.getClass().getName());
        }
    }

    private static void usage(int exitStatus) {
        System.out.println("Usage: csp2xml --cast-path=cast_path" +
                                        " [--cell=cell_name]" +
                                        " [--version]");
        System.exit(exitStatus);
    }

    public static void main(String[] args)
        throws CastSemanticException, CastSyntaxException,
               IOException, VisitorException {
        // Command-line handling code stolen from Csp2TT
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
	final CommandLineArgs argsWithConfigs =
	    new CommandLineArgsWithConfigFiles(parsedArgs); 
	final CommandLineArgs cachedArgs = 
	    new CachingCommandLineArgs(argsWithConfigs);
	final CommandLineArgs theArgs = cachedArgs;
	
        if (theArgs.argExists("version")) {
            System.out.println(
                com.avlsi.util.debug.VersionInfo.getVersionString(
                    Csp2Xml.class));
        }

	final FileSearchPath castFSP =
            new FileSearchPath(theArgs.getArgValue("cast-path", "."));

        final String castCellName = theArgs.getArgValue("cell", null);

        if (castCellName == null ||
            theArgs.nonParsedArgumentsIterator().hasNext())
            usage(1);

        final CastFileParser cfp = new CastFileParser(castFSP, "2");
        final CellInterface cell = cfp.getFullyQualifiedCell(castCellName);

        CSPCellInfo cspInfo = cell.getCSPInfo();
        CSPProgram cspProg = cell.getCSPProgram();

        PrintWriter p = new PrintWriter(new OutputStreamWriter(System.out));
        XmlTourist x = new XmlTourist(p);
        x.beginTag("csp type=\"" + cspInfo.getType() + "\"");
        x.beginTag("PortDefinitions");
        for (Iterator it = cspInfo.getPortDefinitions(); it.hasNext(); ) {
            PortDefinition port = (PortDefinition) it.next();
            printPort(x, port);
        }
        x.endTag("PortDefinitions");
        cspProg.accept(x);
        x.endTag("csp");
        p.flush();
    }
}
