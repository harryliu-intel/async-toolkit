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

import com.avlsi.cell.CellInterface;
import com.avlsi.tools.rte.*;

/**
 * General page to which results for a simulation are added
 *
 * @author Abe Ankumah
 * @version $Revision:  $ $Date:  $
 */

public class WriteSimResults extends HtmlStandardWriter {
    
    private String stylesheet;
    private String logo;
    
    /**
     * Constructor to construct WriteCellList object.
     * @param filename File to be generated.
     */
    public WriteSimResults(String filename) throws IOException {
        super(filename);
    }

    /**
     *  init writing the results
     */ 
    public void init(String cellname, String basedir, int depth) {
	
	logo =  RegressionTestEngine.getLogo(depth);
	stylesheet =  RegressionTestEngine.getStyleSheet(depth);

	printPartialHeader("Test Result for" +cellname, "Regression Test Engine", 
			   stylesheet);
        body("#ffffff");
	table(0, "100%", 1, 0);
	tr();
	td();
	println(logo);
	tdEnd();
	trEnd();
	tableEnd();
	navLinks(true, false, depth);
	h2();
	center();
	println("Simulation Results for " +cellname);
	centerEnd();
	h2End();
	
	/* print tmp hier info here */
    }   
    
    /**
     *  finish the document
     */
    public void end(){
	hr();
	FulcrumCopyright();
	bodyEnd();
	htmlEnd();
	flush();
    }
}

