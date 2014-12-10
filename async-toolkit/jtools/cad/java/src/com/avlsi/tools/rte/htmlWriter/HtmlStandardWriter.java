/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id: $
 * 
 */

package com.avlsi.tools.rte.htmlWriter;

import java.io.*;
import java.util.*;
import java.text.MessageFormat;
import com.avlsi.util.htmlWriter.*;
import com.avlsi.tools.rte.*;

import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.tools.rte.*;
import com.avlsi.util.container.Pair;

/** 
* Class for the Html Format Code Generation specific to RTE.
* This Class contains methods related to the Html Code Generation which 
* are used extensively while generating the entire documentation. 
*
* Adapted from Sun Microsystems, javadoc code.
* @author Abe Ankumah
*/
public class HtmlStandardWriter extends HtmlDocWriter {
    
    /**
     * Name of the file getting generated. If the file getting generated is 
     * "java/lang/Object.html", then the filename is "Object.html".
     */   
    public String filename = "";
    
    /**
     * Constructor to construct the HtmlStandardWriter object.
     *
     * @param filename File to be generated.
     */
    public HtmlStandardWriter(String filename) throws IOException {
        super(filename);
        this.filename = filename;
    }
  
    /**
     * Print the navigation bar for the Html page at the top and and the bottom.
     *
     * @param header If true print navigation bar at the top of the page else
     * print the nevigation bar at the bottom.
     */
    protected void navLinks(boolean header, boolean stats, int depth) {
	
	String reldir ="";
	for(int i=0; i < depth; i++)reldir += "../";
	
        println("");
        println("<!-- ========== START OF NAVBAR ========== -->");
        
	if (header) {
	    anchor("navbar_top");
	} else {
	    anchor("navbar_bottom");
	}
	table(0, "100%", 1, 0);
	tr();
	tdColspanBgcolorStyle(2, "#EEEEFF", "NavBarCell1");
	println("");
	if (header) {
	    anchor("navbar_top_firstrow");
	} else {
	    anchor("navbar_bottom_firstrow");
	}
	table(0, 0, 3);
	print("  ");
	trAlignVAlign("center", "top");
	
	if(stats)
	    navLinkBlank("Statistics");
	else 
	    navLinkContents("Statistics", reldir
			    +RegressionTestEngine.statisticsHtml);
	
	navLinkContents("Test Info", reldir
			+RegressionTestEngine.testInfoHtml);
	navLinkContents("Help Doc", reldir
			+RegressionTestEngine.helpHtml);
	navLinkContents("Examples", reldir
			+RegressionTestEngine.examplesHtml);
	

	print("  ");
	trEnd();
	tableEnd();
	tdEnd();
	
	tdAlignVAlignRowspan("right", "top", 3);
	tdEnd();
	trEnd();
	trEnd();
	
	tableEnd();
	println("<!-- =========== END OF NAVBAR =========== -->");
	println("");
    }

    /**
     * Highlight "Tree" word in the navigation bar, since this is the tree page.
     */
    protected void navLinkBlank(String name) {
        navCellRevStart();
        fontStyle("NavBarFont1Rev");
        bold(name);
        fontEnd();
        navCellEnd();
    }              
    
    /**
     * Print link to the "overview-summary.html" page.
     */
    protected void navLinkContents(String name, String link) {
        navCellStart();
        printHyperLink(link, "",
                       name, true, "NavBarFont1");
        navCellEnd();
    }
                                
    /**
     * Description for a cell in the navigation bar.
     */
    protected void navCellStart() {
        print("  ");
        tdBgcolorStyle("#EEEEFF", "NavBarCell1");
        print("    ");
    }
    
    /**
     * Description for a cell in the navigation bar, but with reverse 
     * high-light effect.
     */
    protected void navCellRevStart() {
        print("  ");
        tdBgcolorStyle("#FFFFFF", "NavBarCell1Rev");
        print(" ");
        space();
    }

    /**
     * Closing tag for navigation bar cell.
     */
    protected void navCellEnd() {
        space();
        tdEnd();
    }

    /**
     * Print Html tag <FRAME=arg>.
     * 
     * @param arg Argument for the tag.
     */
    public void frame(String arg) {
        println("<FRAME " + arg + ">");
    }

    /**
     * Print Html tag <FRAMESET=arg>.
     * 
     * @param arg Argument for the tag.
     */
    public void frameSet(String arg) {
        println("<FRAMESET " + arg + ">");
    }

    /**
     * Print Html closing tag </FRAMESET>.
     */
    public void frameSetEnd() {
        println("</FRAMESET>");
    }


    /**
     *   I'm not sure if this really belongs here, will probably move later!
     */
    public void printInstanceInfo(final String prefix, final CellInterface cell,
				  String basedir, String cell_prefix)
    { 
	TreeSet donecells = new TreeSet();
	for (final Iterator i = cell.getSubcellPairs(); i.hasNext(); ) {
	    final Pair p = (Pair) i.next();
	    final HierName subcellName = (HierName) p.getFirst();
	    final CellInterface subcell = (CellInterface) p.getSecond();
	    
	    final String fullName;
	    if (prefix == null) {
		 fullName = subcellName.getAspiceString();
	    } else {
		 fullName = prefix + "." + subcellName.getAspiceString();
	    }
	    
	    if(!donecells.contains(subcell.getFullyQualifiedType())){
		//if (subcell.hasRealProductionRule()) {
		if(DSimHelper.isRealCell(subcell)){
		    String hashedFileName = RegressionTestEngine.
			get().hashCellNameToFileName(subcell.getType());
		    ul();
		    li("circle");
		    print(prefix +".");
		    printHyperLink("../"+
				   RegressionTestEngine.modules+
                                   (cell_prefix+subcell.getModuleName()).replace('.', '/')+"/"+
                                   hashedFileName+".html", 
				   "tmp",cell_prefix+subcell.getFullyQualifiedType(),true);
		    printInstanceInfo(fullName, subcell, basedir,cell_prefix);
		    ulEnd();
		}
		donecells.add(subcell.getFullyQualifiedType());
	    }
	}
    }    
}  
