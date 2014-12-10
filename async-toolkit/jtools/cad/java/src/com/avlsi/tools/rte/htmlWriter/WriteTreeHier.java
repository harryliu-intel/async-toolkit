/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */
package com.avlsi.tools.rte.htmlWriter;

import java.io.*;
import java.util.*;

import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.rte.*;
import com.avlsi.util.container.Pair;


/**
 *  Generate the Tree hiearachy information for each test
 *
 * @author Abe Ankumah
 * @version $Revision$ $Date$
 */

public class WriteTreeHier extends HtmlStandardWriter {
    
    /**
     *
     * @param filename File to be generated.
     */
    public WriteTreeHier(String filename) throws IOException {
        super(filename);
    }

    protected void startWrite(){
	
    }

    /**
     * 
     */
    public void writeTreeHier(String testname, CellInterface cell,String prefix) {
	printPartialHeader(cell.getType() +" Hierarchy", "Regression Test Engine", 
			   RegressionTestEngine.getStyleSheet(1));
        body("#ffffff");
	table(0, "100%", 1, 0);
	tr();
	td();
	println(RegressionTestEngine.getLogo(1));
	tdEnd();
	trEnd();
	tableEnd();
	navLinks(true, false, 1);
	center();
	h2();
	println("Hierarchy Information For " +prefix+cell.getFullyQualifiedType());	
	h2End();
	println("(" +prefix+cell.getModuleName() +")");
	centerEnd();
	printHierInfo(testname, cell,prefix);
	hr();
	
	HashSet inst;
	inst = (HashSet)RegressionTestEngine.get().
	    getInstantiators(cell.getFullyQualifiedType());
	
	println("<dl><dt><b>All Known Instantiators: </b><dd>");
	println(printInstantiators(inst,prefix)+"</dl>");
	
	hr();
	FulcrumCopyright();
	bodyEnd();
        htmlEnd();
        flush();
    }   

    /**
     * know instantiators of this cell; FIX ME; change name of HashSet instance
     */
    public String printInstantiators(HashSet unimpl, String prefix)
    {
	String ucells="";
	if(unimpl == null)return "None";
	if(unimpl.isEmpty())return "None";
	else {
	    Iterator i = unimpl.iterator();
	    while(i.hasNext()){
		CellInterface cell = (CellInterface)i.next();
		ucells += "<a href=\""+prefix+cell.getFullyQualifiedType()+".html"
		    +"\">" +prefix+cell.getFullyQualifiedType();
		if(i.hasNext())ucells += "</a>, ";
		else ucells += "</a>.";
	    }
	    return ucells;
	}
    }

    protected void printHierInfo(String testname, CellInterface cell, String prefix){
	ul();
	li("circle");
	String hashedFilename = RegressionTestEngine.get().hashCellNameToFileName(cell.getType());
	printHyperLink("../"+
		       RegressionTestEngine.modules+
		       (prefix+cell.getModuleName()).replace('.', '/')+"/"+
		       hashedFilename+".html",
		       "tmp" , prefix+cell.getFullyQualifiedType(),true);
	printInstanceInfo(testname, cell, RegressionTestEngine.get().basedir,prefix);
	ulEnd();
    }
}


