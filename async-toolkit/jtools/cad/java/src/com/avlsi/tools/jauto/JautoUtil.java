/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */



package com.avlsi.tools.jauto;

import java.io.File;
import java.io.FilenameFilter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilterWriter;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Writer;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Set;
import java.util.TreeSet;
import java.util.TreeMap;
import java.util.StringTokenizer;

import com.avlsi.util.text.NumberFormatter;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.InvalidOperationException;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.fast.*;
import com.avlsi.fast.ports.*;
import com.avlsi.file.common.HierName;
import com.avlsi.file.cdl.parser.CDLFactoryEmitter;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.cast.*;
import com.avlsi.tools.lvs.NetGraph;
import com.avlsi.tools.cadencize.Cadencize;
import com.avlsi.tools.cadencize.CadenceInfo;
import com.avlsi.util.debug.Debug;
import com.avlsi.netlist.*;
import com.avlsi.netlist.impl.*;
import com.avlsi.io.FileSearchPath;

import com.avlsi.cell.CellInterface;

import com.avlsi.util.container.AliasedMap;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;


public class JautoUtil {
    
    private JautoUtil() {
        throw new AssertionError();
    }

    static public void getLeafCellInstantiationPaths
        (List/*<ConnectionInfo>*/ lst1,
         CellType ct1,
         Map/*<CellType,List<List<ConnectionInfo>>>*/ map1)
    {
        if(ct1.getAllInstances().size() > 0){
            for (Iterator ita = ct1.getAllInstances().iterator();
                 ita.hasNext(); ) {
                ConnectionInfo cia = (ConnectionInfo)ita.next();

                List/*<ConnectionInfo>*/ lsta =
                    new ArrayList/*<ConnectionInfo>*/(lst1);
                lsta.add(cia);

                getLeafCellInstantiationPaths(lsta, cia.child, map1);
            }
        }
        else{
            List/*<List<ConnectionInfo>>*/ lsta =
                (List/*<List<ConnectionInfo>>*/) map1.get(ct1);
            if (lsta == null) {
                lsta = new ArrayList/*<List<ConnectionInfo>>*/();
                map1.put(ct1, lsta);
            }
            lsta.add(lst1);
        }
    }


    static public String getFlatCanonicalName(CellNet cn1,
                                              List/*<ConnectionInfo>*/ lst1,
                                              List/*<ConnectionInfo>*/ lst2)
    {
        if (!cn1.isPortNet() || lst1.isEmpty()) {
            // Internal net, return name directly
            String flatName = getFlatName(cn1, lst1);
            lst2.add(cn1);
            return flatName;
        } else {
            // Port net, search further up hierarchy
            final int depth = lst1.size();
            List/*<List<ConnectionInfo>>*/ lsta =
                new ArrayList/*<List<ConnectionInfo>>*/(lst1);
            lsta.remove(depth-1);

            ConnectionInfo cia = (ConnectionInfo)lst1.get(depth-1);

            List/*<String>*/ lstb = new ArrayList/*<String>*/(1);

            for (Iterator ita = cia.getParentNet(cn1).iterator();
                 ita.hasNext(); ) {
                CellNet cna = (CellNet)ita.next();

                lstb.add(getFlatCanonicalName(cna, lsta, lst2));
            }

            assert lstb.size() < 2
                : "One port net connecting to more than one cell net " +
                   "in parent.\nParent cell: " + cia.parent.typeName +
                   "\nChild cell: " + cia.child.typeName;

            return (String) lstb.get(0);
        }
    }


    static public String getFlatName(CellNet cn1,
                                     List/*<ConnectionInfo>*/ lst1)
    {
        final StringBuffer sb = new StringBuffer();
        for (Iterator ita = lst1.iterator(); ita.hasNext(); ) {
            ConnectionInfo cia = (ConnectionInfo)ita.next();
            sb.append("/X" + cia.nameInParent.getCadenceString());
        }

        sb.append("/" + cn1.canonicalName.getCadenceString());

        return sb.toString();
    }


    static public String getMostCanonicalName(List/*<String>*/ lst1,
                                              char ch1)
    {
        return (String)lst1.get(0);
    }


    static public double calculateDelay(HalfOperator ho1, GlobalNet gn1)
    {
        double delay = 0.0;
        List<FunctionTerm> lsta = new ArrayList<FunctionTerm>();
        gn1.getDelayFunction(ho1, lsta);
        for (FunctionTerm ftma : lsta) {
            assert ftma.type == FunctionTerm.Type.CONSTANT
                : "Should get constant delay values at this stage.";

            delay += ftma.coefficient;
        }

        return delay;
    }

}
