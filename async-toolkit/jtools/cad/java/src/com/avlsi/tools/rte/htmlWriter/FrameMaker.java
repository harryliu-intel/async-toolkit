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

/**
 * Generate the index.html file for brower. If multiple top-level test are specified
 * create a page containing a frame with three sections, otherwise just make 
 * a frame with two sections.
 *
 * @author Abe Ankumah
 * @ersion $Revision:  $ $Date:  $
 */
public class FrameMaker extends HtmlStandardWriter {
    
    /**
     * Constructor to construct FrameMaker object. 
     * @param filename File to be generated.
     */
    public FrameMaker(String filename) throws IOException {
        super(filename);
    }

    /**
     * Print the frame sizes and their contents. Frame ratios are hardcoded for
     * now, might support allowing it to be optionally sized.
     */
    public void printFrameDetails(int noOfTests, String mframeFile) {
        frameSet("cols=\"20%,80%\"");
        if (noOfTests < 2) {
            frame("src=\"" +RegressionTestEngine.cellListFile
                  + "\" name=\"" +RegressionTestEngine.cellListFrame+"\"");
            frame("src=\""+mframeFile  + 
                  "\" name=\"" +RegressionTestEngine.mainFrame+"\"");
        } else {
            frameSet("rows=\"30%,70%\"");
            frame("src=\""+RegressionTestEngine.testListFile+"\" name=\""
		  +RegressionTestEngine.testListFrame+"\"");
	    frame("src=\"" +RegressionTestEngine.cellListFile
                  + "\" name=\"" +RegressionTestEngine.cellListFrame+"\"");
            frameSetEnd();
	    frame("src=\""+mframeFile  + 
		  "\" name=\"" +RegressionTestEngine.mainFrame+"\"");
        }
        frameSetEnd();
    }   
}
