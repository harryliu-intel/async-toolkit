/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.jauto;

import java.util.*;
import java.io.*;
import com.avlsi.prs.*;
import com.avlsi.cell.CellInterface;
import com.avlsi.cast.CastFileParser;
import com.avlsi.file.common.HierName;
import com.avlsi.io.FileSearchPath;

import com.avlsi.util.container.Pair;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import java.util.TreeSet;

/** Should someday be integrated with CastStat. **/
public class CountPrs {

    /** data members **/
    private CellInterface cell;
    private TreeSet unimpl;
    private TreeSet celldata;
    private TreeSet prscells;
    
    private TreeSet subtype;
    private String []path;
    private int leafinstance;

    /** constructor **/
    public CountPrs(){
	unimpl = new TreeSet();
	celldata = new TreeSet();
	prscells = new TreeSet();
	//prsinst = new TreeMap();
	subtype = new TreeSet();
	path = null;
	leafinstance = 0;
    }
    
    /** set the path being used here **/
    public void setPath(String []path){
	this.path = path;
    }

    //copied cell rename code here
      public static String renameCell(String name)
	throws Exception {
	
 boolean badName = false;
        final StringBuffer sb = new StringBuffer();
        boolean inParens = false;    
        int curlyCount = 0;

        final int nameLength = name.length();

        int i = 0;
        while ( ( i < nameLength ) &&
                ( ! badName ) ) {
            final char c = name.charAt(i);
              
            switch (c) {
            case '.':
                badName = inParens;
                sb.append('.'); 
                break;
            case ',':
                badName = !inParens;
                sb.append("_"); 
                break;
            case '(':
                inParens = true;
                sb.append("-L");
                break;
           case ')':
                badName = !inParens;
                inParens = false;
                sb.append("-R");
                break;
            case '{':
                badName = !inParens;
                ++curlyCount;
                sb.append("-L");
                break;
            case '}':
                badName = ! ( inParens && ( curlyCount > 0 ) );
                --curlyCount;
                sb.append("-R");
                break;
            case '-':
                sb.append('-');
                badName = ( ( ! inParens ) ||
                            ( i >= ( nameLength - 1 ) ) ||
                            ( ! ( Character.isDigit( name.charAt( i + 1 )))));
                break;
            case '_': 
                sb.append('_'); 
                break;
            default:
                if (Character.isLetterOrDigit(c)) {
                    sb.append(c);
                }
                else {
                    final String errorMessage =
                        "Unable to translate '" +
                        Character.toString(c) +
                        "' (0x" +
                        Integer.toString( ( int ) c, 16 ) +
                        ")";
                    throw new Exception( errorMessage );
                }
            }
            ++i;
        }
        badName = badName || inParens || ( curlyCount != 0 ) ;
        if ( badName ) {
            final String errorMessage =
                "\"" + name + "\" is an invalid cell name.";
            throw new Exception( errorMessage );
        }
        return sb.toString();
    }
    
    /** count the number of PRS this cell has **/
    public int countPrs(CellInterface cell, String cname, int depth){
	Iterator i;
	int val, result = 0;
	
	if(isRealCell(cell)){
	    subtype.add(cell.getFullyQualifiedType());
	    //for(int tab=0; tab < depth; tab++)
	    //System.out.print("---");
	    //System.out.println("+"+cell.getFullyQualifiedType() +" " +cname);
	}
	    
	if(cell.containsCompletePrs()){
	    prscells.add(cell.getFullyQualifiedType());
	    //prsinst.put();
	    leafinstance++;
	}

	i = cell.getProductionRuleSet().getProductionRules();
	while(i.hasNext()){
	    ProductionRule prs = (ProductionRule)i.next();
	    result++;
	}
	val = result;
	
	for (i = cell.getSubcellPairs(); i.hasNext(); ) {
	    final Pair p = (Pair) i.next();
	    final CellInterface subcell = (CellInterface) p.getSecond();
	    final HierName name = (HierName)p.getFirst();
	    	    
	    if(isRealCell(subcell) && subcell.containsSubcells()){
		//System.out.println("containsSubcells: "+subcell.getType());
		result += countPrs(subcell, cname+"."+name.toString(), depth+1);	       
	    }
	    else if(isRealCell(subcell) && 
		    !(subcell.containsCompletePrs()
		      ||subcell.containsSubcells())){
		//System.out.println("Unimplemented: "+subcell.getType());
		unimpl.add(subcell.getModuleName()+"."+subcell.getType());
		result += countPrs(subcell, cname+"."+name.toString(), depth+1);
	    }
	    else if(isRealCell(subcell) && subcell.containsCompletePrs()){
		//System.out.println("Implemented: "+subcell.getType());
		result += countPrs(subcell, cname+"."+name.toString(), depth+1);
	    }
	}
	/** Print the number of production Rules in the cells which have 
	    been implemented **/
	if(cell.containsCompletePrs()||cell.containsSubcells())
	    celldata.add(cell.getModuleName()+"."+cell.getType()
			 +": "+result);
	
	return result;
    }

    public void writeSubtypeInfo(PrintStream p, boolean duplicate){
	int index;
	String cellName;
	String subtypeNum;
	Iterator i = subtype.iterator();
	while(i.hasNext()){

	    String subtypeName = (String)i.next();
	    index = subtypeName.lastIndexOf('.');
	    cellName = subtypeName.substring(0, index);
	    subtypeNum = subtypeName.substring(index+1, subtypeName.length());
	    
	    //this is a hack to get around the fact that not all the 
	    //cells in the subtypes block need to refine a cell in the 
	    //subcells block; eg. SLACK_1of2(0) does not make it's way 
	    //into the subtypes block but is still avaliable in the 
	    //subcells block
	    CellInterface c;
	    try {
		CastFileParser cfp = 
		    new CastFileParser(new FileSearchPath(this.path),"2"); 
		System.out.print("Attempting to locate " +cellName);
		c = cfp.getFullyQualifiedCell(cellName);
	    }
	    catch(Exception e){
		System.out.println(": Not Found (probably just a wire)");
		continue;
	    }
	    if(c == null)continue;
	    System.out.println(": Found");
	    String rcellName=null;
	    try {
		rcellName = renameCell(cellName);
	    }
	    catch(Exception e){
		e.printStackTrace();
		System.exit(-1);
	    }
	    if(!duplicate){
		p.println("SPLIT: " +rcellName +" 0 " +subtypeNum);
		p.println("SPLIT: " +rcellName +" 0 0");
	    }
	    else if(duplicate){
		p.println("SPLIT: " +rcellName +" " +subtypeNum
			  +" " +subtypeNum);
	    }
	    
	    p.flush();
	}
    }
    
    /** print the production rule count **/
    public void printPRCount()
    {
	Iterator i = celldata.iterator();
	if(i.hasNext())
	    System.out.println("\nProduction Rule Count:\n");
	while(i.hasNext()){
	    System.out.println("\t"+i.next());
	}
    }

    
    public void printLeafCells()
    {
	Iterator i = prscells.iterator();
	if(i.hasNext())
	    System.out.println("\nLeaf Cells:("+prscells.size()+")\n");
	while(i.hasNext()){
	    System.out.println("\t"+i.next());
	}
	System.out.println("Total number of leaf cells: "+leafinstance);
    }


    /** print the cells that are unimplementable **/
    public void printUnimpl()
    {
	Iterator i = unimpl.iterator();
	if(i.hasNext())
	    System.out.println("\nUnimplemented Cells: ("+unimpl.size()+")\n");
	while(i.hasNext()){
	    System.out.println("\t"+i.next());
	}
    }

    /** Is this a realCell at All? ***/
    public boolean isRealCell(CellInterface cell){
	 return (cell.containsCompletePrs() 
		 || cell.containsCsp() 
		 || cell.containsJava() 
		 || cell.containsSubcells());
     }

    
    /****** get the components of the path *****/
    public static String [] getPath(String pathstr){
        int i=0;
        StringTokenizer pathtok = new StringTokenizer(pathstr, ":");
        String []path = new String[pathtok.countTokens()];      
	while(pathtok.hasMoreTokens()){
            path[i++] = pathtok.nextToken();
        }
        return path;
    }


    /** useage, routine **/
    public static void usage(){
	System.out.println("CountPrs: --cast-path=<cast-path> --cell=<cell> "
			   +"[--tool=getleaf|unimpl|prscount]"
			   +"[--subtype-info=<filename>] [--duplicate]");
	System.exit(-1);
    }

    /**** main routine ****/
    public static void main(String args[]){
	
	CountPrs cp = new CountPrs();
	CellInterface cell = null;
	
	/*** parse command-line arguments here ***/
	String []path =null;
	String module = null, cellName = null;
	String tool = "";
	String filename = null;
	boolean duplicate = false;

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
                path = CountPrs.getPath(pathstr);
                if(path == null){
                    System.out.println("cast-path not specified");
                    System.exit(0);
                } 
            }
	    if((curr.getName()).equals("cell")){
                String fcellName = curr.getValue();
		if(fcellName == null){
		    CountPrs.usage();
		    System.exit(-1);
		}
                int lastdot = fcellName.lastIndexOf('.');
                module = fcellName.substring(0, lastdot);
                cellName = fcellName.substring(lastdot+1, fcellName.length());
	    } 
	    if((curr.getName()).equals("tool")){
	    tool = curr.getValue();
		if(tool == null){
		    CountPrs.usage();
		    System.exit(-1);
		}
	    }
	    if((curr.getName()).equals("subtype-info")){
		filename = curr.getValue();
		if(filename == null){
		    CountPrs.usage();
		    System.exit(-1);
		}
	    }
	    if((curr.getName()).equals("duplicate")){
		System.out.println("dfII_split_subtypes: duplication mode");
		duplicate = true;
	    }
        }
        if(cellName == null || module == null || path == null){
	    CountPrs.usage();
            System.exit(-1);
        }
	
	try {
	    FileSearchPath cFSP = new FileSearchPath(path);
	    cp.setPath(path);
            CastFileParser cfp = new CastFileParser(cFSP,"2"); 
            cell = cfp.getFullyQualifiedCell(module, cellName);
	    
	    cfp = null;
            System.gc();
	    System.out.println("\nPrs Count: "+cp.countPrs(cell, "<top-level>",0));
	    if(tool.equals("") || tool.equals("all")){
		cp.printUnimpl();
		cp.printPRCount();
		cp.printLeafCells();
	    }
	    else if(tool.equals("getleaf"))cp.printLeafCells();
	    else if(tool.equals("prscount"))cp.printPRCount();
	    else if(tool.equals("unimpl"))cp.printUnimpl();
	    
	    if(filename != null){
		PrintStream p =new PrintStream(new FileOutputStream(filename));
		cp.writeSubtypeInfo(p, duplicate);
	    }
	}
	catch(Exception e){
	    e.printStackTrace();
	    System.exit(-1);
	}
    }
}

