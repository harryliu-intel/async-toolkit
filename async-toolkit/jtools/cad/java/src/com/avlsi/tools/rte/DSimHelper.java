/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.rte;

import java.io.*;
import java.util.*;
import com.avlsi.cell.CellInterface;
import com.avlsi.fast.EnvBlock;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.BlockIterator;

import com.avlsi.tools.tsim.*;
import com.avlsi.tools.cosim.CoSimParameters;
import com.avlsi.tools.dsim.DSim;
import com.avlsi.tools.dsim.DSimUtil;
import com.avlsi.util.container.Pair;
import com.avlsi.tools.dsim.Node;
import com.avlsi.fast.ports.*;
import com.avlsi.tools.rte.htmlWriter.*;
import com.avlsi.util.debug.*;
import com.avlsi.util.text.*;

import java.util.*;
import com.avlsi.prs.*;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;

/** 
 * This class contains code which should ideally be implemented in DSim 
 * This class will become deprecated as soon as all those features find 
 * their way into DSim. Note that they haven't been directly implemented 
 * in DSim since the CoSim UI will be changing soon. 
 **/

public class DSimHelper {

    /** is passed cell an actual cell and not node, defchan etc **/
    public static boolean isRealCell(CellInterface cell){
	return (cell.containsCompletePrs() 
		|| cell.containsRunnableCsp() 
		|| cell.containsJava() 
		|| cell.containsCompleteSubcells());
     }


    public static int cellStatistics(CellInterface cell, HashSet unimpl){

	Iterator i;
	int result = 0;
	int nodes = 0;
	int totalnodes = 0;

	i = cell.getProductionRuleSet().getProductionRules();
	while(i.hasNext()){
	    ProductionRule prs = (ProductionRule)i.next();
	    result++;
	}
	
	if(!(cell.containsCompletePrs()||cell.containsCompleteSubcells()))
	    unimpl.add(cell);
	
	for (i = cell.getSubcellPairs(); i.hasNext(); ) {
	    //if(cell.isNode())nodes++;
	    final Pair p = (Pair) i.next();
	    final CellInterface subcell = (CellInterface) p.getSecond();
	    final HierName name = (HierName)p.getFirst();
	    if(isRealCell(subcell) && subcell.hasRealProductionRule()){
		result += cellStatistics(subcell, unimpl);
	    }
	    else if(isRealCell(subcell) && !subcell.hasRealProductionRule()){
		if(!subcell.containsCompleteSubcells() &&
                   !subcell.containsCompletePrs()){
		    unimpl.add(subcell);
		}
		result += cellStatistics(subcell, unimpl);
	    }
	}
	return result;
    }
    

    
    /** Generate a list of commands that are trivially translated 
	into something CoSim.setParams() can be passed, this is used
	to indicate what level we want to simulated everything at
    **/
    public static void getSimParams(CellInterface cell){
	
	return;
    }

    /** main routine for testing these features **/
    public static void main(String []args){
	
	System.out.println("Hello World\n");
	return;
    }
}
