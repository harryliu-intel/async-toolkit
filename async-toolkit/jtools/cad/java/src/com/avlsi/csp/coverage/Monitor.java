/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.csp.coverage;

import com.avlsi.util.debug.Debug;

import java.util.*;
import java.io.*;
import java.util.zip.*;

/**
 * Store a list of code coverage probe locations, as well as a table
 * indicating whether or not each probe has been reached by test
 * cases.
 *
 * By "probes" we mean statements which have been inserted into
 * generated code, to see if certain locations are reached. Not to be
 * confused with CSP probes. We call the act of inserting these probes
 * in a body of code "instrumenting" that code.
 * 
 * The important parts of this class are probeTable and hitTable. When
 * a section of code is instrumented, the instrumenter (usually
 * JavaEmitter) will call register() with a list of probe locations
 * and types. Usually, register() will extend probeTable and hitTable
 * with new block of indices, one for each element in this list. It
 * then returns the offset of the whole block, which can be used by
 * the caller to quickly register a hit (e.g. the fifth probe would
 * say Monitor.getDefault().hitTable[offset+5]=1). 
 * 
 * In order to support gathering data over multiple runs of the
 * program, we also keep a table of types which have been registered.
 * This way, if register() is called a second time for the same type
 * name, the original offset is returned. We also check the
 * modification date of the file to make sure that the file hasn't
 * been modified between runs.
 *
 * TODO: Support for combining data from separate files (so that we
 * can speed up testing by running multiple sessions on different
 * computers). Note that this is one reason why not all methods and
 * data members in this class should be static.
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public class Monitor {
    public volatile int[] hitTable;
    ProbeInfo[] probeTable;
    HashMap registeredTypes; // map of String -> RegisteredType
    int totalEntries;
    private int allocEntries;

    public static final String defaultPath="coverage-data.zip";
    String path=defaultPath;

    /** The default global Monitor instance. This has been made a
     * public data member for efficiency **/
    public static Monitor global=new Monitor();

    private static final boolean debug=false;

    public Monitor() {
	initTables();
    }

    private void initTables() {
	totalEntries = 0;
	allocEntries = 128;
	hitTable = new int[allocEntries];
	probeTable = new ProbeInfo[allocEntries];
	registeredTypes = new HashMap();
    }

    /** Holds information for each type which has been registered by
     * Monitor.register(). This includes the offset into the hitTable,
     * which was returned by register() the first time it was called
     * with this type **/
    private static class RegisteredType {
	long lastModified;
	int numProbes;

	// this isn't stored in probes file, it is set when the parser
	// calls register()
	int hitTableOffset;
	
	public RegisteredType(long lastModified, int numProbes, int hitTableOffset) {
	    this.lastModified = lastModified;
	    this.numProbes = numProbes;
	    this.hitTableOffset = hitTableOffset;
	}
	
	public RegisteredType(String s) 
	    throws ParseException {
	    // parse s into a registered type object and a name
	    String words[]=s.split(" ");
	    if(words.length!=3) throw new ParseException("Expected three numbers");
	    String curr=null;
	    try {
		curr=words[0];
		lastModified = new Long(curr).longValue();
		curr=words[1];
		hitTableOffset = new Integer(curr).intValue();
		curr=words[2];
		numProbes = new Integer(curr).intValue();
	    } catch(NumberFormatException e) {
		throw new ParseException("Expected an integer, found "+curr);
	    }
	}
	
	public String toString() {
	    return lastModified+" "+hitTableOffset+" "+numProbes;
	}
    }

    private void checkInit() {
	if(hitTable == null) {
	    Debug.assertTrue(totalEntries==0);
	    initTables();
	}
    }

    /** Register a set of probes for a given cell type. 
     * @param type The fully qualified name of the type
     * @param info An array giving information about each probe
     * @param lastModified The modification time of the file defining
     * that cell type
     * @return An offset into Monitor.hitTable (or to be passed to
     * Monitor.setHit(int))
     **/
    public int register(String type, ProbeInfo[] info, long lastModified) {
	if(debug) System.err.println("Monitor.register("+type+", "+info.length+", "+lastModified+")");
	checkInit();
	if(registeredTypes.containsKey(type)) {
	    RegisteredType rt = (RegisteredType)registeredTypes.get(type);
	    if(rt.lastModified!=lastModified) {
		System.err.println("Warning: source file has changed since last coverage"+
				   " run, registering probes anyway");
	    }
	    Debug.assertTrue(rt.numProbes == info.length);
	    if(debug) System.err.println("Monitor.register: previously registered type, returning "+rt.hitTableOffset);
	    return rt.hitTableOffset;
	} else {
	    int offset=extendTable(info.length);
	    registeredTypes.put(type, new RegisteredType(lastModified, info.length, offset));
	    for(int i=0; i<info.length; i++) {
		probeTable[offset+i] = info[i];
	    }
	    if(debug) System.err.println("Monitor.register: newly registered type, returning "+offset);
	    return offset;
	}
    }

    /** Change the default file from which to load and in which to
     * store coverage information **/
    public void setPath(String path) {
	this.path = path;
    }

    /** @see setPath(String) **/
    public String getPath() {
	return this.path;
    }
    
    public boolean pathExists() {
	return new File(this.path).exists();
    }

    /** reinitialize this object with a persistent copy stored in a
     * file **/
    // XXX: should this handle the parse exception itself?
    // maybe return a boolean indicating failure
    public void load() throws IOException, ParseException {
	if(debug) System.err.println("Monitor.load()");
	initTables();
	/* read data from a zip file */
	File f=new File(path);
	// XXX: check if f exists
	ZipFile zf=new ZipFile(f);
	ZipEntry ze=zf.getEntry("hits");
	InputStream zs;

	zs=zf.getInputStream(ze);
	Debug.assertTrue(ze.getSize()!=-1);
	// set totalEntries to size of stored hitTable and initialize
	// other tables
        //
        // (4 is number of bytes in an int, as defined by
        // DataInputStream)
	extendTableTo((int)ze.getSize()/4);
        DataInputStream dis = new DataInputStream(zs);
        for(int i=0; i<totalEntries; i++) {
            hitTable[i]=dis.readInt();
        }
	zs.close();

	zs=zf.getInputStream(new ZipEntry("probes"));
	BufferedReader r=new BufferedReader(new InputStreamReader(zs));
	for(int i=0; i<totalEntries; i++) {
	    String s=r.readLine();
	    Debug.assertTrue(s!=null);
	    probeTable[i] = new ProbeInfo(s);
	}
	zs.close();

	zs=zf.getInputStream(new ZipEntry("types"));
	r=new BufferedReader(new InputStreamReader(zs));
	while(true) {
	    String s=r.readLine();
	    if(s == null) break;
	    String words[]=s.split(" ", 2);
	    if(words.length!=2) throw new ParseException("Expected at least two words");
	    registeredTypes.put(words[0], 
				new RegisteredType(words[1]));
	}
	zs.close();
    }

    /** save persistent state for this object to a file **/
    public void save() throws IOException {
	if(debug) System.err.println("Monitor.save()");
	if(debug) System.err.println("Monitor.save: totalEntries="+totalEntries);
	
	checkInit();
	/* dump data to a zip file */
	File zf=new File(path);
	zf.delete();
	ZipOutputStream zs=new ZipOutputStream(new FileOutputStream(zf));

	zs.putNextEntry(new ZipEntry("hits"));
        DataOutputStream dos = new DataOutputStream(zs);
        for(int i=0; i<totalEntries; i++) {
            dos.writeInt(hitTable[i]);
        }
//	zs.write(hitTable,0,totalEntries);
	for(int i=0; i<totalEntries; i++) {
	    if(debug) System.err.println("Monitor.save: hitTable["+i+"]="+hitTable[i]);
	}
        dos.flush();
        dos=null;
	zs.closeEntry();

	zs.putNextEntry(new ZipEntry("probes"));
	Writer w=new OutputStreamWriter(zs);
	for(int i=0; i<totalEntries; i++) {
	    if(debug) System.err.println("Monitor.save: writing probe entry "+i);
	    w.write(probeTable[i].toString()+"\n");
	}
	w.flush();
	w=null; // ???: could we use the same writer, after adding a
		// new ZipEntry?
	zs.closeEntry();

	zs.putNextEntry(new ZipEntry("types"));
	w=new OutputStreamWriter(zs);
	Iterator i=registeredTypes.keySet().iterator();
	while(i.hasNext()) {
	    String s=(String)i.next();
	    RegisteredType rt=(RegisteredType)registeredTypes.get(s);
	    w.write(s+" "+rt.toString()+"\n");
	}
	w.flush();
	w=null;
	zs.closeEntry();
	zs.close();
    }

    /** register a hit in location i **/
    public void setHit(int i) {
//	System.err.println("Monitor.setHit("+i+")");
	hitTable[i]++;
    }

    private int extendTable(int entryCount) {
	return extendTableTo(totalEntries+entryCount);
    }
    
    private int extendTableTo(int newTotal) {
	if(debug) System.err.println("Monitor.extendTableTo("+newTotal+")");
	// XXXX: should we add ability to shrink table?
	if(allocEntries<newTotal) {
	    Debug.assertTrue(allocEntries>0);
	    do {
		allocEntries *= 2;
	    } while(allocEntries<newTotal);
	    
	    int[] hitTable = new int[allocEntries];
	    ProbeInfo[] probeTable = new ProbeInfo[allocEntries];
	    for(int i=0; i<totalEntries; i++) {
		hitTable[i] = this.hitTable[i];
		probeTable[i] = this.probeTable[i];
	    }
	    this.hitTable = hitTable;
	    this.probeTable = probeTable;
	}
	int oldTotal = totalEntries;
	totalEntries = newTotal;
	return oldTotal; // becomes offset for added entries
    }

    /** Print out a list of probes which haven't been covered yet **/
    public int listMisses(PrintStream w) {
	int count=0;
	for(int i=0; i<totalEntries; i++) {
	    if(hitTable[i]==0) {
		count++;
		w.println(probeTable[i].missedString());
	    }
	}
	w.println("Missed "+count+" out of "+totalEntries+
		  " coverage probes"+
                  (totalEntries>0 ? " ("+
                   (totalEntries-count)*100/totalEntries+"% covered)" :
                   ""));
        return count;
    }

    /** Print out a list of probes which haven't been covered yet **/
    public void listCounts(PrintStream w) {
	for(int i=0; i<totalEntries; i++) {
            w.println(hitTable[i]+" "+probeTable[i].missedString());
	}
    }

    /** Return the total number of entries **/
    public int totalEntries() {
        return totalEntries;
    }

    /** Reinitialize the object **/
    public void clearAll() {
        initTables();
    }

    /** Zero all the hits **/
    public void clearHits() {
        for(int i=0; i<totalEntries; i++) {
            hitTable[i]=0;
        }
    }
}
