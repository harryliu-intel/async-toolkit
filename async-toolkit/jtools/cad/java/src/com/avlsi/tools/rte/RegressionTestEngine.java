/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */
 
package com.avlsi.tools.rte;

import java.io.*;
import java.util.*;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.SortedMap;
import java.util.Collections;
import java.util.TreeMap;
import java.util.StringTokenizer;

import com.avlsi.io.FileSearchPath;
import com.avlsi.cast.CastFile;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast.CastSemanticException;
import com.avlsi.cast2.impl.CastParsingOption;
import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.exception.AssertionFailure;
import com.avlsi.cast.impl.CellInterfaceCollectionIterator;

import com.avlsi.util.container.Pair;
import com.avlsi.tools.rte.htmlWriter.*;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.CommandLineArg;

/**
 * Entry point for RTEServer; 
 *  <ul><li>Build list of unique cells to test
 *      <li>
 *  </ul>
 *
 * @author Abe Ankumah
 * @version 1.0
 **/
public class RegressionTestEngine {
    
    /* class data members */
    private CellInterface []envcell;
    private Vector brokenFiles=null;
    private static SortedMap cell_list;
    private static SortedMap fileNames;
    private static SortedMap done_cells;
    private static List dispatched;
    private int cells_remaining;
    private int filename_cntr;
    private FileWriter checkpoint;
    private PrintStream fexceptions;
    private TreeMap child_parent;
    private boolean no_recurse;

    /* define some test constants */
    private static final String castVersion = "2";
    private static final int NOT_TESTED = 0x0; 
    private static final int PASSED = 0x1; 
    private static final int FAILED = 0x2; 
    private static final int MAX_FILENAME_LENGTH = 0x80;

    /* define all the useful strings/file name conventions here */
    public static final String indexHtml = "index.html";
    public static final String statisticsHtml = "statistics.html";
    public static final String helpHtml = "help-doc.html";
    public static final String testInfoHtml = "test-info.html";
    public static final String examplesHtml = "examples.html";
    
    public static final String testListFile = "all-tests.html";
    public static final String testListFrame = "testListFrame";
    
    public static final String cellListFile = "cell-list.html";
    public static final String cellListFrame = "cellListFrame";
    
    public static final String mainFrame = "mainFrame";

    /* define names for directory structure */
    public static final String logs = "logs/";  
    public static final String hier = "hier/";
    public static final String modules = "modules/";
    public static final String ckptfile = "run.checkpoint";
    public static final String exptfile = "exception.txt";
    public static final String ckptdelim = "\t\t";

    public String logo;
    public String stylesheet_css;
    public String StyleSheetInclude;
    public String basedir;
    public String cell_prefix = "";
    
    private int timeout = 2500;
    public static final int AMINUTE = 60000;     //one minute
    
    public boolean timedOut(){return (timeout <= 0); }
    public void adjustTimeout(int minutes){this.timeout -= minutes; }

    int getTimeOut(){return timeout; }
    void setTimeOut(int timeout){this.timeout = timeout; }    

    private static RegressionTestEngine singleton = null;
    
    public static RegressionTestEngine get(){
	if(singleton == null)
	    singleton = new RegressionTestEngine();
	
	return singleton;
    }
    
    public void setFileNames(String bdir){
	
	basedir = bdir+"/";
	//System.out.println(basedir);
	stylesheet_css = basedir+"stylesheet.css";
	
	StyleSheetInclude =
	    "<LINK REL =\"stylesheet\" TYPE=\"text/css\" HREF=\""
	    +stylesheet_css+"\" TITLE=\"Style\">";
	    	
	logo = 
	    "<TD ALIGN=\"LEFT\" HEIGHT=\"90\"><IMG SRC=\""+basedir+"logo.png"+
	    "\" WIDTH=\"250\""
	    +" HEIGHT=\"90\" ALT=\"Fulcrum\" BORDER=\"0\" align=\"left\"></A>";
	
	/** make the needed directories **/
	createDirectory(basedir+hier);
	createDirectory(basedir+modules);
	
	try {
	    checkpoint = new FileWriter(basedir+logs+ckptfile);
	    fexceptions = new 
                PrintStream(new FileOutputStream(basedir+logs+exptfile));
	    
	}
	catch(Exception e){
	    e.printStackTrace();
	    System.exit(-1);
	}

	return;
    }

    /** BUG FIXES; Making RTE Interface more web friendly *****/
    public static String getLogo(int depth){
	String revDir ="";
	String link ="";
	for(int i=0; i < depth; i++)
	    revDir += "../";
	
	return  "<TD ALIGN=\"LEFT\" HEIGHT=\"90\"><IMG SRC=\""+revDir
	    +"logo.png"+
	    "\" WIDTH=\"250\""
	    +" HEIGHT=\"90\" ALT=\"Fulcrum\" BORDER=\"0\" align=\"left\"></A>";
    }
    
    public static String getStyleSheet(int depth){
	String revDir ="";
	String link ="";
	for(int i=0; i < depth; i++)
	    revDir += "../";
	
	return "<LINK REL =\"stylesheet\" TYPE=\"text/css\" HREF=\""
	    +revDir+"stylesheet.css"+"\" TITLE=\"Style\">";
	
    }
    
    /* private constuctor */
    private RegressionTestEngine(){
	
	//all accesses to this map are synchronized 
	cell_list= 
	    Collections.synchronizedSortedMap(new TreeMap()); 
	done_cells = 
	    Collections.synchronizedSortedMap(new TreeMap()); 
	dispatched = 
	    Collections.synchronizedList(new ArrayList());
	fileNames= 
	    Collections.synchronizedSortedMap(new TreeMap()); 
	this.cells_remaining = 0;
	this.filename_cntr = 0;
	this.brokenFiles = new Vector();
	this.child_parent = new TreeMap();
        this.no_recurse = false;
    }
    
    /* this routines returns the Sorted map, no changes are made to it */
    public SortedMap getMap(){return cell_list; }

    /** enable/disable recursion into passed cell **/
    public void setNoRecurse(boolean no_recurse){
        this.no_recurse = no_recurse;
    }
    
    /** this routines the broken Files the rte attempted to simulate **/
    public Vector getBrokenFiles(){return this.brokenFiles; }

    /** return the list of cells which have been dispatched but are not **/
    public List getDispatched(){return dispatched; }

    /* return a sorted map of all the cells that are done */
    public SortedMap getDoneCells(){return done_cells; }

    /* return an entry in the map to the client performing the test */
    public synchronized CellInterface getTest(){
	
	CellInterface cell = null; 
	if(cell_list.size() > 0){
	    //get the last key and use it to retrive the value from map
	    String cellname = (String)(cell_list.lastKey());
	    cell = (CellInterface) ((Pair)cell_list.get(cellname)).getFirst();
	    cell_list.remove(cellname);

            //save a list of files which have been dispatched
            dispatched.add(cell_prefix+cell.getFullyQualifiedType());
	}

	//return cell interface or null
	return cell;
    }

    public List unDispatchedCells(){
	List undispatched = Collections.synchronizedList(new ArrayList());
	Iterator i = cell_list.keySet().iterator();
	while(i.hasNext()){
	    undispatched.add(i.next());
	}
	return undispatched;
    }

    /* add a given cell to the cell_list map */
    public void addCell(CellInterface cell){
	//put the name of the cell and the cell interface and associated
	//testing status into the map
	cell_list.put(cell_prefix+cell.getFullyQualifiedType(), 
		      new Pair(cell, new Integer(NOT_TESTED)));
    }

    /**
     * returns true if there any more tests left 
     * //this should eventually include the standard dispatched condition 
     **/
    public boolean haveTest(){
	return (cell_list.size() > 0);
    }
    
    /**
     * this routine is used to determine if all the tests are done or not
     */
    public boolean AllCellsDone(){
	return (cells_remaining == 0);
    }
    
    /** 
     * this routine is used to modify the map used to store the cell 
     * (cellname, Pair(cell interface, testresult flag))
     **/
    public synchronized void writeResults(CellInterface cell, 
					  SimResults result){
	this.cells_remaining--;
	done_cells.put(cell_prefix+cell.getFullyQualifiedType(), result);
	dispatched.remove(cell_prefix+cell.getFullyQualifiedType());
	/** write to the checkpoint file ***/
	try {
	    checkpoint.write(this.cells_remaining +"/" 
			     +cell_prefix+cell.getFullyQualifiedType() +ckptdelim 
			     +result.getResultDetail() +ckptdelim
			     +result.getDuration() +"\n");
	    checkpoint.flush();
	}
	catch(Exception e){
	    e.printStackTrace();
	}
    }

    /**
     * pretty printer for list of cells ** FIX ME ** comments
     **/
    //public String 

    /**
     *  for debugging purposes --pretend as if we are done
     */ 
    public void fakeDone(){this.cells_remaining = 0; }

    //Takes a cellName or moduleName and creates the appropirate cellInferface
    public void getTestInfo(String cellName, String moduleName, String cellList, String []path) throws Exception {
	
	//Instantiate a CastFile parser
	final CastFileParser cfp = 
	    new CastFileParser(new FileSearchPath(path), castVersion);
        
        try {
            CellInterfaceCollectionIterator cellsIter;
            if(cellList != null){
                final BufferedReader br =
                    new BufferedReader(new FileReader(cellList));
                String line;
                final Collection<CellInterface> cells =
                    new ArrayList<CellInterface>();
                while ((line = br.readLine()) != null) {
                    if (!line.startsWith("#")) {
                        line = line.trim();
                        cells.add(cfp.getFullyQualifiedCell(line));
                    }
                }
                envcell = cells.toArray(new CellInterface[0]);
                for (CellInterface c : envcell) {
                    this.InstanceInfo(c, 0);
                }
            } else if(cellName != null){
                envcell = new CellInterface[1];
                envcell[0] = cfp.getFullyQualifiedCell( cellName );
                this.InstanceInfo(envcell[0], 0);
            }
            else {
                int i=0; 
                int size=0;
                final CastFile cf = cfp.parseModule(moduleName);
                
                cellsIter = cf.getAllCellsPossible();
                //TODO: this is reall poor form, fixme
                while(cellsIter.hasNext()){ size++; cellsIter.next(); }
                envcell = new CellInterface[size];
                cellsIter = cf.getAllCellsPossible();
                while(cellsIter.hasNext()){
                    envcell[i]=(CellInterface)cellsIter.next(); 
                    this.InstanceInfo(envcell[i], 0);
                    i++;
                }
            }
        }
        catch(CastSemanticException e){
            com.avlsi.tools.dsim.ExceptionPrettyPrinter
                                .printException(e, fexceptions);
            com.avlsi.tools.dsim.ExceptionPrettyPrinter
                                .printException(e, System.err);
            System.exit(-1);            
        }
        catch(Throwable t){
            t.printStackTrace(fexceptions);
            t.printStackTrace();
            System.exit(-1);
        }
    	fexceptions.flush();
	//Intialize how many cells we intend to simulate for bookkeeping
	this.cells_remaining = cell_list.size();
    }

    /**
     * Set Prefix to prepend to cellnames
     */
    public void setCellPrefix(String cell_prefix){
        this.cell_prefix = cell_prefix +".";
    }

    /**
     * get the Prefix to prepend to cellnames
     */
    public String getCellPrefix(){
        return this.cell_prefix;
    }

    /**
     *  create all the directories needed, prevent competion on the part 
     *  of the different clients --thanks Kim!
     */
    public void createModuleDirs(SortedMap mycells){
	CellInterface cell = null;
	Iterator i = mycells.values().iterator();
	while(i.hasNext()){
	    cell = (CellInterface)((Pair)i.next()).getFirst();
	    createDirectory(this.basedir+modules+
			    ((cell_prefix+cell.getModuleName()).replace('.', '/')));
	}
    }

    /** Provide mapping for instantiators **/
    public void getInstantiators(CellInterface cell)
    {
	HashSet instantiators;
	
	for (final Iterator i = getNonInlined(cell).iterator(); i.hasNext(); ) {
	    
	    final Pair p = (Pair) i.next();
	    final CellInterface subcell = (CellInterface) p.getSecond();
	    instantiators = (HashSet)child_parent.
		get(subcell.getFullyQualifiedType());
	    if(instantiators == null){
		instantiators = new HashSet();
		instantiators.add(cell);
		child_parent.put(subcell.getFullyQualifiedType(), 
				 instantiators);
	    }
	    else {
		instantiators.add(cell);
	    }
	}
    }

    /** get HashSet for all cells which the passed Cell Instantiates **/
    public HashSet getInstantiators(String cellType)
    {
	HashSet hs = (HashSet)child_parent.get(cellType);
	return hs;
    }

    private Collection getNonInlined(final CellInterface cell) {
        final List noninlined = new ArrayList();
        for (final Iterator i = cell.getAllSubcellPairs(); i.hasNext(); ) {
            final Pair p = (Pair) i.next();
            final HierName name = (HierName) p.getFirst();
            final CellInterface subcell = (CellInterface) p.getSecond();
            if (!subcell.isNode() && !subcell.isChannel() &&
                name.getParent() == null) {
                noninlined.add(p);
            }
        }
        return noninlined;
    }

    /**
     * traverse the entire hierarchy up to the specified depth generating 
     * storing a list of all the unique cells in a cell_list class member
     * A depth < 0 indicates decending down the entire tree
     *
     * Also generate a reverse mapping of cells and what it 
     * instantiates
     **/
    
    public void InstanceInfo(final CellInterface cell, int depth)
        throws IOException {
        if(!cell_list.containsKey(cell_prefix+cell.getFullyQualifiedType())){
            if(DSimHelper.isRealCell(cell)){
                this.addCell(cell);  
                System.out.print("."); System.out.flush();
            }
        }
	for (final Iterator i = getNonInlined(cell).iterator(); i.hasNext(); ) {

	    final Pair p = (Pair) i.next();
	    final CellInterface subcell = (CellInterface) p.getSecond();

	    //get reverse mapping of direct instantiators
	    this.getInstantiators(subcell);

	    if(!cell_list.containsKey(cell_prefix+subcell.getFullyQualifiedType())){
                if(DSimHelper.isRealCell(subcell)){
                    if(!no_recurse) {
                        InstanceInfo(subcell, depth++);
                        this.addCell(subcell);
                        System.out.print(".");
                        System.out.flush();
                    }
		}
	    }
	}
    }
    
    
    /**
     *  We need to have this routine to get around the long file names problem.
     *  Unix has a limit on how long the name of a file can be, so we need to 
     *  keep a table mapping cellName to their associated filename, most of 
     *  the time the filenames will just be the same as the cellnames.
     */
    public String hashCellNameToFileName(String cellName){
	
	String hashedFileName = ((String)fileNames.get(cellName));
	if(hashedFileName == null){
	    if(cellName.length() < MAX_FILENAME_LENGTH){
		hashedFileName = cellName;
	    }
	    else {
		hashedFileName = cellName.substring(0, MAX_FILENAME_LENGTH) 
		    +"_"+filename_cntr;
		filename_cntr++;
	    }
	    fileNames.put(cellName, hashedFileName);
	}
	return hashedFileName;
    }


    /**
     * Given a path string create all the directories in the path. For example,
     * if the path string is "lib/buffer", the method will create directory 
     * "lib" and then "lib/buffer" if they don't exist. The file separator 
     * string "/" is platform dependent system property.
     * 
     * @param path Directory path string.
     */
    public static void createDirectory(String path) {
        if (path == null || path.length() == 0) {
            return;
        }
        File dir = new File(path);
        try {
            if (dir.exists()) {
                return;
            } else {
                if (dir.mkdirs()) {
                    return;
                } else {
                    
		    System.err.println("Unable to create "+path
				       +"; check permissions");
                    System.exit(-1);
                }
            }
        } catch (Exception exc) {
            exc.printStackTrace();
	    System.exit(-1);
	} 
    }
    
    //static help method 
    public static void help(){
	System.err.println(RegressionTestEngine.get().getClass().getName()+"\n"
			   + "\t--cast-path=cast_path \n"
			   + "\t--cell=<fully.qualified.cell.NAME>|--module=<module.name> \n"
			   + "\t--work-dir=results_dir \n"
                           + "\t--cell-prefix=cell_prefix \n"
			   + "\t--server-limit=number of servers \n"
                           + "\t--no-recurse=[0|1] \n"
			   + "\t--walltime=max real time \n"
			   + "\t--version=get version  \n"
			   + RegressionTestEngine.version());
	return;
    }
    
    //get version info
    public static String version(){
        return com.avlsi.util.debug.VersionInfo.getVersionString(RegressionTestEngine.class);
    }
    
    /** 
     * Returns a String array from a colon delimited string; good for path, 
     *	fileNames, ports etc 
    **/
    public static String [] getStringArray(String pathstr){
	int i=0;
	StringTokenizer pathtok = new StringTokenizer(pathstr, ":");
	String []path = new String[pathtok.countTokens()];	
	while(pathtok.hasMoreTokens()){
	    path[i++] = pathtok.nextToken();
	}
	return path;
    }

    /*** this will return the env cell **/
    public CellInterface []getEnvCell(){return this.envcell; }

    /** main routine for the Entire RTE infrastructure **/
    public static void main (String []args) throws Exception {
	
	String []path =null;
	String cellName = null;
        String moduleName = null;
        String cellList = null;
	String basedir = null;
        String cell_prefix = "";
        boolean no_recurse = false;  //default value for recursing
	int numclients=4;    //default number of clients
	int timeout=24;      //default timout
	
	/**** new command line processing stuff ****/
	CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs ); 
	
        CommandLineArgs cachedArgs = 
            new CachingCommandLineArgs( argsWithConfigs );
	
        CommandLineArgs theArgs = cachedArgs;
	
	CommandLineArgsIterator cmdIter = theArgs.iterator();
	
	while ( cmdIter.hasNext() ) {
	    CommandLineArg curr = cmdIter.next();
	    if((curr.getName()).equals("cast-path")){
		String pathstr = curr.getValue();
		path = getStringArray(pathstr);
		if(path == null){
		    System.err.println("ERROR: please specify a cast-path");
		    System.exit(0);
		}
	    }
            if((curr.getName()).equals("cell")){
		cellName = curr.getValue();
            }
            if((curr.getName()).equals("module")){
		moduleName = curr.getValue();
            }
            if((curr.getName()).equals("cell-list")){
		cellList = curr.getValue();
            }
            if((curr.getName()).equals("work-dir")){
		basedir = curr.getValue();
		if(basedir == null){
		    System.out.println("Working directory specified");
		    System.exit(0);
		}
            }
	    if((curr.getName()).equals("server-limit")){
		numclients = new Integer(curr.getValue()).intValue();
	    }
            if((curr.getName()).equals("no-recurse")){
		no_recurse = !curr.getValue().equals("0");
	    }
	    if((curr.getName()).equals("walltime")){
		//walltime in hours
		timeout = new Integer(curr.getValue()).intValue();
	    }
            if((curr.getName()).equals("cell-prefix")){
		cell_prefix = curr.getValue();
                System.out.println("Setting cell prefix to: "+cell_prefix);
		if(cell_prefix == null)cell_prefix = "";
            }
	    if((curr.getName()).equals("help")){
		RegressionTestEngine.help();
		System.exit(-1);
	    }
	    if((curr.getName()).equals("version")){
		System.err.println(RegressionTestEngine.version());
		System.exit(-1);
	    }
	    //process other potential args here
	}
        if(cellName == null && moduleName == null && cellList == null){
            System.err.println("ERROR: please specify either a cell, a module, or a cell list\n");
            System.exit(-1);
        }
	RegressionTestEngine rte =  RegressionTestEngine.get();
        if(!cell_prefix.equals(""))
            rte.setCellPrefix(cell_prefix);
	rte.setTimeOut(timeout*60);
	rte.setFileNames(basedir);
	rte.setNoRecurse(no_recurse);

	System.out.println("Building cell list to test.");
	rte.getTestInfo(cellName, moduleName, cellList, path);
	
        CellInterface []cell = rte.getEnvCell();
	
	System.out.println("\ncell list constructed");
	System.out.println("Creating module directories for results");
	rte.createModuleDirs(rte.getMap());
	
	//now create all the hier pages in this simulation run
	String hashedFilename;
	for(int j=0; j < cell.length; j++){
	    //skip cells which were broken
	    if(cell[j] == null)continue;
            
            if(DSimHelper.isRealCell(cell[j])){
                hashedFilename = RegressionTestEngine.get().
                    hashCellNameToFileName(cell[j].getType());
                WriteTreeHier th = new WriteTreeHier(RegressionTestEngine.get().basedir+
                                                     RegressionTestEngine.hier
                                                     +rte.getCellPrefix()
                                                     +cell[j].getModuleName()+"."
                                                     +hashedFilename +".html");
                th.writeTreeHier("_rte",cell[j],rte.getCellPrefix());
                th.close();
            }
	    for (Iterator i = cell[j].getSubcellPairs(); i.hasNext(); ) {
		Pair p = (Pair) i.next();
		CellInterface subcell = (CellInterface) p.getSecond();
		final HierName subcellName = (HierName) p.getFirst();
		//if(subcell.hasRealProductionRule()){
		if(DSimHelper.isRealCell(subcell)){
		    hashedFilename = RegressionTestEngine.get().
			hashCellNameToFileName(subcell.getType());
		    WriteTreeHier th = new WriteTreeHier(RegressionTestEngine.get().basedir+
							 RegressionTestEngine.hier
                                                         +rte.getCellPrefix()
							 +subcell.getModuleName()+"."
							 +hashedFilename +".html");
		    th.writeTreeHier(subcellName.getAspiceString(),subcell,rte.getCellPrefix());
                    th.close();
		}
	    }
	}
        	
	WriteTestList tl = new WriteTestList(RegressionTestEngine.get().basedir
					     +testListFile);
	String firstFile = tl.writeTestList(cell, rte.getCellPrefix());
        tl.close();
	System.out.println("FirstFile: "+firstFile);
	
	//now create the cell list page
	WriteCellList cl = new WriteCellList(RegressionTestEngine.get().
					     basedir +cellListFile);
	cl.writeCellList(rte.getMap(),rte.getCellPrefix());
        cl.close();
	
	Object stallobject = new Object();

	//start server and pass this instance of the RTE to it
	//FIX ME add port as argument to server and probably root dir
	
	//get time this server started
	Calendar calendar = new GregorianCalendar(TimeZone.getDefault());
	String starttime = calendar.getTime().toString();
	
	System.out.println("Starting RTE Server");
	RTEServer rteserver = new RTEServer(rte, stallobject, 
					    numclients,300);
					    
	if(!rteserver.getStatus()){
	    System.out.println("FATAL: Server could not be started\n");
	    System.exit(-1);
	}
	System.out.println("Server Running at port " + rteserver.getPort());
	
	//dispatch jobs to all clients that are polling the server
	System.out.println("Ready to dispatch jobs. " +cell_list.size()
			   +" cell(s) ready to be tested.");
	System.out.println("Listening for clients ...");
	rteserver.launch_jobs();

	//busy wait while all the tests are not done
	System.out.println("Walltime of " +(rte.getTimeOut()/60) 
			   +":00:00 in effect");
	while(!rte.AllCellsDone()){ 
	    Thread.sleep(RegressionTestEngine.AMINUTE); 
	    //System.out.println(rte.getTimeOut() +" more minutes to go");
	    rte.adjustTimeout(1);
	    if(rte.timedOut())break;
	}
	if(!rte.timedOut())
	    System.out.println("Simulation of all cells complete ...");
	else 
	    System.out.println("Simulation timed out  ..." +rte.getTimeOut());
	
	System.out.println("Publishing Results");

	//write out the simulation stats
	WriteStatistics ss = new WriteStatistics(RegressionTestEngine.get().
						 basedir+statisticsHtml);
						 
	ss.writeStats(starttime, rte.getDoneCells(), rte.getBrokenFiles(), 
		      rte.getDispatched(), rte.unDispatchedCells(), rte.getCellPrefix());
	ss.end();
	System.out.println("Statistics generated!");
	
	//start publishing 
	FrameMaker indexpage = new FrameMaker(RegressionTestEngine.get().
					      basedir+indexHtml);
					      
	indexpage.printPartialHeader("Test Results", 
				     "Regression Test Engine", null);
	
	indexpage.printFrameDetails(2, hier+firstFile);
	indexpage.flush();
	
	/** close all connections to this server **/
	rteserver.close();
	/** force the virtual machine to exit; **/
	System.exit(-1);
    }
}
