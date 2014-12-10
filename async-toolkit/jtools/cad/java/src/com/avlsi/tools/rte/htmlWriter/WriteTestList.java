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
import com.avlsi.util.container.Pair;

/**
 *  Writes out a list of cells being tested
 *
 * @author Abe Ankumah
 * @version $Revision:  $ $Date:  $
 */

public class WriteTestList extends HtmlStandardWriter {
    
    /**
     * Constructor to construct WriteCellList object.
     *
     * @param filename File to be generated.
     */
    public WriteTestList(String filename) throws IOException {
        super(filename);
    }

    /**
     * 
     */
    public String writeTestList(CellInterface []envcell, String prefix) {
	
	boolean flag = true;
	String firstFile ="";
       	
	printPartialHeader("Test List", "Regression Test Engine", null);
	body("white");
	fontSizeStyle("+1", "FrameHeadingFont");
	bold();
	print("All Tests");
	boldEnd();
	fontEnd();
	p();
	table(0, "100%", 0, 0);
	tr();
	tdNowrap();
	fontStyle("FrameItemFont");
	
	for(int j=0; j < envcell.length; j++){
            CellInterface subcell = envcell[j];
            if(DSimHelper.isRealCell(subcell)){
                String celltype = subcell.getType();
                String moduleName = prefix+subcell.getModuleName();
                String hashedFilename = RegressionTestEngine.get().
                    hashCellNameToFileName(celltype);
                //get the name of the firstFile
                if(flag){firstFile = 
                             moduleName+"."+hashedFilename+".html"; 
                flag = false;}             
                println("<A HREF=\"" 
                        +RegressionTestEngine.hier+moduleName+"."+hashedFilename 
                        +".html\" TARGET=\""
                        +RegressionTestEngine.mainFrame+"\">"
                        +moduleName+"."+hashedFilename +"</A>");
                br();
            }
	}
	
	fontEnd();
	tdEnd();
	trEnd();
	tableEnd();
	bodyEnd();
	htmlEnd();
	flush();
	//return the name of the first file
	return firstFile;
    }   
    
}
