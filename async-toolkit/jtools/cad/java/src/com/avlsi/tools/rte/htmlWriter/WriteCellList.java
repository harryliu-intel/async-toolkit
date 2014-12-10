/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id: Exp $
 */
package com.avlsi.tools.rte.htmlWriter;

import java.io.*;
import java.util.*;
import com.avlsi.tools.rte.*;

import com.avlsi.cell.CellInterface;
import com.avlsi.util.container.Pair;
/**
 * Generate file which contains a list of all the cells being used
 * in the simulation. 
 *
 * @author Abe Ankumah
 * @version $Revision:  $ $Date:  $
 */

public class WriteCellList extends HtmlStandardWriter {
    
    /**
     * Constructor to construct WriteCellList object.
     * @param filename File to be generated.
     */
    public WriteCellList(String filename) throws IOException {
        super(filename);
    }

    /**
     * takes a maps of the cells and simply prints them out
     */ 
    public void writeCellList(SortedMap cells, String prefix) {

       
	printPartialHeader("Cell List", "Regression Test Engine", null);
	body("white");
	fontSizeStyle("+1", "FrameHeadingFont");
	bold();
	print("All Cells");
	boldEnd();
	fontEnd();
	p();
	table(0, "100%", 0, 0);
	tr();
	tdNowrap();
	fontStyle("FrameItemFont");
	
	Iterator i = cells.keySet().iterator();
	String simResults, celltype;
	
	while(i.hasNext()){
	    
	    celltype = (String)i.next();
	    CellInterface cell = (CellInterface)((Pair)cells.get(celltype)).getFirst();

	    String hashedfileName = RegressionTestEngine.get().hashCellNameToFileName(cell.getType());
	    simResults = RegressionTestEngine.modules
		+(prefix+cell.getModuleName()).replace('.', '/')
		+"/"+hashedfileName+".html";
	    println("<A HREF=\""+simResults +"\" TARGET=\""
		    +RegressionTestEngine.mainFrame+"\">"
		    +celltype +"</A>");
	    br();
	}
	
	fontEnd();
	tdEnd();
	trEnd();
	tableEnd();
	bodyEnd();
	htmlEnd();
	flush();
    }   
    
    
}
