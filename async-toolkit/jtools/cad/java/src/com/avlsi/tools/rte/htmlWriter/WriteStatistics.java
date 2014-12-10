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

import com.avlsi.util.text.*;
import java.io.*;
import java.util.*;
import com.avlsi.tools.rte.*;
import com.avlsi.cell.CellInterface;

/*
 *  This class us used to publish results of a regression test engine run. It 
 *  will also optionally e-mail specifiied users if a certain class of errors 
 *  are detected; 
 *
 * @author Abe Ankumah
 * @version $Revision:  $ $Date:  $
 */
public class WriteStatistics extends HtmlStandardWriter {
    
    /**
     **   TO DO: Fix up final appearance of statistics page
     **/

    /** some constants **/
    private static final String bgcolor = "#ffffff";
	    
    /** data members used for statistics summary **/
    private int fragments = 0;
    private int csp_errors = 0;
    private int failed = 0;
    private int passed = 0;
    private int rteignore = 0;
    private int cosimwillfail = 0;
    private int synchronous = 0;
    private int unimplementable = 0;
    private int notests = 0;
    private int exception = 0;
    private int _ntpcspec = 0;
    private int _cyclenode = 0;
    private int cosimnum = 0;
    private int ntpctarget = 0;
    private int ntpcedtarget = 0;

    private static String passedPageLink = "logs/passed.html";
    private static String failedPageLink = "logs/failed.html";
    private static String rteignorePageLink = "logs/rte_ignore.html";
    private static String cosimwillfailPageLink = "logs/cosim_will_fail.html";
    private static String cspErrorPageLink = "logs/csp.html";
    private static String cycleNodePageLink = "logs/cycle_node.html";
    private static String ntpcSpecPageLink = "logs/ntpc.html";
    private static String exceptionPageLink = "logs/expt.html";
    private static String ntpctargetPageLink = "logs/ntpctarget.html";
    private static String ntpcedtargetPageLink = "logs/ntpcedtarget.html";
		
    private WriteSimResults passedPage;
    private WriteSimResults failedPage;
    private WriteSimResults rteignorePage;
    private WriteSimResults cosimwillfailPage;
    private WriteSimResults cspErrorPage; 
    private WriteSimResults cycleNodePage; 
    private WriteSimResults ntpcSpecPage; 
    private WriteSimResults exceptionPage;
    private WriteSimResults ntpctargetPage;
    private WriteSimResults ntpcedtargetPage;
    
    /**
     * Constructor to construct WriteStatistics object.
     * @param filename of File to be generated.
     */
    public WriteStatistics(String filename) throws IOException {
        super(filename);
	passedPage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+passedPageLink);
	failedPage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+failedPageLink);
	rteignorePage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+rteignorePageLink);
	cosimwillfailPage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+cosimwillfailPageLink);
	cspErrorPage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+cspErrorPageLink);
	cycleNodePage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+cycleNodePageLink);
	ntpcSpecPage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+ntpcSpecPageLink);
	exceptionPage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+exceptionPageLink);
	ntpctargetPage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+ntpctargetPageLink);
        ntpcedtargetPage = new WriteSimResults(RegressionTestEngine.get().basedir+"/"+ntpcedtargetPageLink);

	passedPage.init("Passed Cells",RegressionTestEngine.get().basedir,1);
	failedPage.init("Failed Cells", RegressionTestEngine.get().basedir,1);
	rteignorePage.init("RTE IGNORE Directives", RegressionTestEngine.get().basedir,1);
	cosimwillfailPage.init("COSIM_WILL_FAIL Directives", RegressionTestEngine.get().basedir,1);
	cspErrorPage.init("CSP Errors", RegressionTestEngine.get().basedir,1);
	cycleNodePage.init("cycle_node not specified", RegressionTestEngine.get().basedir,1);
	ntpcSpecPage.init("ntpc_spec not specified", RegressionTestEngine.get().basedir,1);
	exceptionPage.init("Exception on Instantiation", RegressionTestEngine.get().basedir,1);
	ntpctargetPage.init("NTPC Target Off", RegressionTestEngine.get().basedir,1);
        ntpcedtargetPage.init("NTPC Target Off (Estimated)", RegressionTestEngine.get().basedir,1);
	
    }

    /**
     * Work horse routine, prints all information on the statistics page, takes 
     * a string for when the simulation started a map of cell names a result string
     */ 
    public void writeStats(String starttime, SortedMap cells, 
			   Vector brokenFiles, List dispatched, 
			   List undispatched, String prefix) {
	
	//add fulcrum log import stylesheet for page
	printPartialHeader("Simulations Statistics", "Regression Test Engine", 
			   RegressionTestEngine.getStyleSheet(0));
        body(bgcolor);
	table(0, "100%", 1, 0);
	tr();
	td();
	println(RegressionTestEngine.getLogo(0));
	tdEnd();
	trEnd();
	tableEnd();
	navLinks(true, true, 0);
	h2();
	center();
	println("Simulation Statistics");
	centerEnd();
	h2End();
	
	printTimeInfo(starttime);
	
	println("A total of " +cells.size() +" cell(s) were simulated");
	
	if(brokenFiles.size() != 0){
	    hr();
	    println("Flawed input files: <br>");
	    for(int i=0; i < brokenFiles.size(); i++)
		println(brokenFiles.get(i) +"<br>");
	}

	if(dispatched.size() != 0){
	    hr();
	    println("<h3>Dispatched but not done before timeout: </h3><p>");
	    for(int i=0; i < dispatched.size(); i++)
		println(dispatched.get(i) +"<br>");
	}

	if(undispatched.size() != 0){
	    hr();
	    println("<h3>Undispatched Cells before timeout: </h3><p>");
	    for(int i=0; i < undispatched.size(); i++)
		println(undispatched.get(i) +"<br>");
	}
	
	String res_type;
	Iterator names = cells.keySet().iterator();
	Iterator results = cells.values().iterator();
	boolean makeBold = false;
	SimResults result;
	String cellname;
	while(names.hasNext() && results.hasNext()){
	    cellname = (String)names.next();
            //cellname = prefix+cellname;
	    result = (SimResults)results.next();
	    // get statictics summary information here 
	    if(result.simPassed()){ 
		passed++;
		passedPage.println(cellLink(cellname));
		passedPage.br();
	    }
	    if(result.hasRTEIgnore()){ 
		rteignore++;
		rteignorePage.println(cellLink(cellname) +": "+result.getRTEIgnoreStr());
		rteignorePage.br();
	    }	    
	    if(result.hasCosimWillFail()){ 
		cosimwillfail++;
		cosimwillfailPage.println(cellLink(cellname) +": "+result.getCosimWillFailStr());
		cosimwillfailPage.br();
	    }
	    if(result.simFailed()){ 
		failed++; 
		failedPage.println(cellLink(cellname));
		failedPage.br();
	    }
	    if(result.cspError()){
		csp_errors++;
		cspErrorPage.println(cellLink(cellname));
		cspErrorPage.br();
	    }
	    
	    if(!result.hasCycleNode()){
		_cyclenode++;
		cycleNodePage.println(cellLink(cellname));
		cycleNodePage.br();
	    }
	    
	    if(!result.hasNTPCspec()){
		_ntpcspec++;
		ntpcSpecPage.println(cellLink(cellname));
		ntpcSpecPage.br();
	    }

	    if(result.runtimeException()){
		exception++;
		exceptionPage.println(cellLink(cellname));
		exceptionPage.br();
	    }

	    if(result.isTestable() && result.hasNTPCspec() && !result.getNTPCTarget()){
		ntpctarget++;
		ntpctargetPage.println(cellLink(cellname));
		ntpctargetPage.br();
	    }
            
            if(result.isTestable() && result.hasNTPCspec() && 
               !result.getNTPCTargetModifiedDelay()){
		ntpcedtarget++;
		ntpcedtargetPage.println(cellLink(cellname));
		ntpcedtargetPage.br();
	    }
	    
	    //System.out.println("getSimType(): " +result.getSimType());
	    if(result.getSimType() == SimResults.CSPCOSIM || 
	       result.getSimType() == SimResults.JAVACOSIM){ 
		cosimnum++; 
		
	    }
	    if(result.isFragment()){fragments++;}
	    if(result.isSynchronous()){synchronous++;}
	    if(result.isUnimplementable()){unimplementable++;}
	    if(result.missingTest()){notests++;}
	    
	}
	
	/**** write out the summary information ***/
	 hr();
	 h2();
	 center();
	 println("Results summary");
	 h2End();
	 table(1, "100%", 1, 1);
	 int all = cells.size();
	 int ni = fragments;
	 FillSummaryTable(true, "Info class", "# cells", "% cells", null);
	 FillSummaryTable(false, "Passed", String.valueOf(passed), 
			  NumberFormatter.format(100.0*(double)passed/(all-ni),
						 2), passedPageLink);
	 FillSummaryTable(false, "Failed", String.valueOf(failed), 
			  NumberFormatter.format(100.0*(double)failed/(all-ni),
						 2), failedPageLink);
	 FillSummaryTable(false, "RTE Ignore", String.valueOf(rteignore), 
			  NumberFormatter.format(100.0*(double)rteignore/(all-ni),
						 2), rteignorePageLink);
	 FillSummaryTable(false, "Cosim Will Fail", String.valueOf(cosimwillfail), 
			  NumberFormatter.format(100.0*(double)cosimwillfail/(all-ni),
						 2), cosimwillfailPageLink);
	 FillSummaryTable(false, "Fragment", String.valueOf(fragments), 
			  NumberFormatter.format(100.0*(double)fragments/all,
						 2), null);
	 FillSummaryTable(false, "CSP Errors", String.valueOf(csp_errors), 
			  NumberFormatter.format(100.0*(double)csp_errors/all,
						 2), cspErrorPageLink);
	 FillSummaryTable(false, "Synchronous", String.valueOf(synchronous), 
			  NumberFormatter.format(100.0*(double)synchronous/all,
						 2), null);
	 FillSummaryTable(false, "Unimplementable", 
			  String.valueOf(unimplementable), 
			  NumberFormatter.
			  format(100.0*(double)unimplementable/all,2), null);
	  FillSummaryTable(false, "Cells Cosimulated", 
			   String.valueOf(cosimnum), 
			   NumberFormatter.
			   format(100.0*(double)cosimnum/(all-ni),2), null);
	 FillSummaryTable(false, "Missing Tests", String.valueOf(notests), 
			  NumberFormatter.format(100.0*(double)notests/all,2), 
			  null);
	 FillSummaryTable(false, "Exception on Instantiation", 
			  String.valueOf(exception), 
			  NumberFormatter.
			  format(100.0*(double)exception/(all-ni),2),
			  exceptionPageLink);

	 FillSummaryTable(false, "Missing NTPC", String.valueOf(_ntpcspec), 
			  NumberFormatter.
			  format(100.0*(double)_ntpcspec/all,2), 
			  ntpcSpecPageLink);
	 
	 FillSummaryTable(false, "Missing cycle_node", 
			  String.valueOf(_cyclenode), 
			  NumberFormatter.format(100.0*(double)_cyclenode/all,
						 2), cycleNodePageLink);
	 
	 FillSummaryTable(false, "Failed NTPC Target", 
			  String.valueOf(ntpctarget), 
			  NumberFormatter.
			  format(100.0*(double)ntpctarget/(all-ni),2), 
			  ntpctargetPageLink);

         FillSummaryTable(false, "Failed NTPC Target (Modified Delay)", 
			  String.valueOf(ntpcedtarget), 
			  NumberFormatter.
			  format(100.0*(double)ntpcedtarget/(all-ni),2), 
			  ntpcedtargetPageLink);
	 tableEnd();
	 centerEnd();
	 
	 passedPage.end();
	 failedPage.end();
	 rteignorePage.end();
	 cosimwillfailPage.end();
	 cspErrorPage.end();
	 cycleNodePage.end();
	 ntpcSpecPage.end();
	 exceptionPage.end();
	 ntpctargetPage.end();
         ntpcedtargetPage.end();

	 passedPage.flush();
	 failedPage.flush();
	 rteignorePage.flush();
	 cosimwillfailPage.flush();
	 cspErrorPage.flush();
	 cycleNodePage.flush();
	 ntpcSpecPage.flush();
	 exceptionPage.flush();
	 ntpctargetPage.flush();
         ntpcedtargetPage.flush();
	 
	 /** FIXME: close these pages sometime **/
    }
    
    /**
     * generate row for summay into statics table 
     **/
    public void FillSummaryTable(boolean bold, String type, String val1, String val2, String link){
	
	//write a three coloum table for stats summary, bold for header
	tr();
	tdNowrap();
	if(bold)bold();
	if(link == null){
	    /** FIX ME: make link here **/
	    print(type);
	}
	else {
	    println("<A HREF=\""+link
		    +"\" TARGET=\""
		    +RegressionTestEngine.mainFrame +"\">"
		    +type +"</A>");
	}
	boldEnd();
	tdEnd();
	tdNowrap();  
	if(bold)bold();
	print(val1);
	boldEnd();
	tdEnd();
	tdNowrap();
	if(bold)bold();
	print(val2);
	boldEnd();
	tdEnd();
	trEnd();
    }

    /** returns cellsname for cell under <basedir>/logs/ in linkable format**/
    public String cellLink(String cellname){
	return "<A HREF=\"../modules/" +cellname.replace('.','/')
	    +".html \" TARGET=\""
	    +RegressionTestEngine.mainFrame +"\">"
	    +cellname +"</A>";
    }
    
    /**
     *  print the simulation time information for a given run  
     **/
    public void printTimeInfo(String starttime) {
	
	table(0, "100%", 0, 0);
	
	tr();
	tdNowrap();
	fontStyle("FrameItemFont");
	print("Simulation started at: ");
	tdEnd();
	tdNowrap();
	bold();
	print(starttime);
	boldEnd();
	tdEnd();
	trEnd();
	
	Calendar calendar = new GregorianCalendar(TimeZone.getDefault());
	String endtime = calendar.getTime().toString();
	
	tr();
	tdNowrap();
	fontStyle("FrameItemFont");
	print("Simulation ended at: ");
	tdEnd();
	tdNowrap();
	bold();
	print(endtime);
	boldEnd();
	tdEnd();
	trEnd();
	tableEnd();
    }

    /**
     *  end the page with <hr> and Fulcrum copyright notice, force write
     **/
    public void end(){
	hr();
	FulcrumCopyright();
	bodyEnd();
	htmlEnd();
	flush();
    }
}


