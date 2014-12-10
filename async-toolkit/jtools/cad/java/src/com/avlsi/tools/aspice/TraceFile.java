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

package com.avlsi.tools.aspice;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.File;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.util.Iterator;
import com.avlsi.file.common.HierName;
import com.avlsi.util.debug.Debug;
/**
 * Class for Trace Files in Jaspice
 *
 * @author Dan Daly
 * @version $Date$
 **/

public class TraceFile {

    private static final int ORDER_ORIGINAL = 0;
    private static final int ORDER_REORDERED= 1;
    private static final int ORDER_CHANGING = 2;
    private static final int HEADER_SIZE = 3;
    private static String[] suffixes = { ".names", ".trace" };
    private File namesfile,tracefile;
    private boolean namesOpen = false;
    private DataOutputStream dos;
    private BufferedWriter nameswriter;
    private int order;
    private int timestamp;
    private int num_nodes;

    private boolean defaultEndian = true;//Use Sun's Ordering
    /**
     * Constructor.
     **/
    public TraceFile(String path) {
        /** Check to make sure we have the base name only **/
        for (int loop=0;loop<suffixes.length;loop++) {
            if (path.endsWith(suffixes[loop])) {
                path = 
                    path.substring(0, path.length()-suffixes[loop].length());
            }
        }
        namesfile = new File(path+suffixes[0]);
        tracefile = new File(path+suffixes[1]);
        num_nodes = 0;
        String osname = System.getProperty("os.name");
        String endian = System.getProperty("sun.cpu.endian");
        if (endian.equals("little") ||
            ((osname.startsWith("i") && osname.endsWith("x86")))){
            defaultEndian = false;
        }
    }

    public void addNodeName(String s) 
            throws TraceFileException{
        try {
            nameswriter.write(s+"\n",0,s.length()+1);
        } catch (IOException e) {
            throw new TraceFileException("Could not write to names file: "+
                                         e.getMessage(), e);
        }
        num_nodes++;
    }
        
    /** Function for adding a node to the .names file.  Takes an iterator
     * with all of the names of node as an argument**/
    public void addNodeNames(Iterator names) 
        throws TraceFileException {
        if (names == null) return;
        StringBuffer st = new StringBuffer();
        while(names.hasNext()) {
            st.append(((HierName) names.next()).toString());
            if (names.hasNext())    st.append("=");
        }
        String s = st.toString();
        addNodeName(s);
    }
    /** Function for rewriting a new tracefile.  This allows for
     * sequential writing of the file (not random access)
     **/
    public void startNewTrace() 
        throws TraceFileException{
        try {    
            nameswriter = new BufferedWriter(
                         new FileWriter(namesfile));
            namesOpen = true;
        } catch (IOException e) {
            throw new TraceFileException("Could not open "+
                                         namesfile.getName()+
                                         " : "+e.getMessage(), e);
        }
        try {
            FileOutputStream fos = new FileOutputStream(tracefile);
            dos = new DataOutputStream(
                  new BufferedOutputStream(fos));
        } catch (IOException e) {
            throw new TraceFileException("Could not open "+
                                         tracefile.getName()+
                                         " : "+e.getMessage(), e);
        }
        order = ORDER_ORIGINAL;
        timestamp = 0;
        num_nodes= 1;
        writeNameHeader();
    }

    private void writeNameHeader() 
            throws TraceFileException {
        try {
            nameswriter.write("time\n");
        } catch (IOException e) {
            throw new TraceFileException("Error writing to names file: "+
                e.getMessage(), e);
        }
    } 
    
    private void writeHeader() 
            throws TraceFileException {
                
            writeInt(this.order);
            writeInt(this.timestamp);
            writeInt(this.num_nodes);
    }
    
    public void writeInts(int[] vals) 
            throws TraceFileException {
        for (int loop=0;loop<vals.length;loop++) {
            writeInt(vals[loop]);
        }
    }

    public void writeRecord(float[] vals) 
            throws TraceFileException {
        for (int loop=0;loop<vals.length;loop++)
            writeFloat(vals[loop]);
    }
    
    private void writeInt(int val)
            throws TraceFileException {
        try {
            if (defaultEndian) dos.writeInt(val);
            else { //Switch bytes for Intel x86
                for (int i=0;i<4;i++) {
                    dos.write(val);
                    val = (val >>8);
                }
            }
        } catch (IOException e) {
            throw new TraceFileException("Error writing trace"+
                                         " java.io.IOException: "+
                                         e.getMessage(), e);
        }
    }
    
    public void writeFloat(float val) 
            throws TraceFileException {
        //Switch Bits
        if (defaultEndian) {
            try {
                dos.writeFloat(val);
            } catch (IOException e ) {
                throw new TraceFileException("Error writing trace"+
                                         " java.io.IOException: "+
                                         e.getMessage(), e);
            }
        } else {
            int ival = Float.floatToIntBits(val);
            writeInt(ival);
        }
    }

    public void closeOutNodes() 
        throws TraceFileException{
        writeHeader();
        closeNamesFile();
    }
    
    private void closeNamesFile() 
        throws TraceFileException {
        if (namesOpen) {
            try {
                nameswriter.close();
                namesOpen = false;
            } catch (IOException e) {
                throw new TraceFileException("Error closing names file"+
                                             " java.io.IOException: "+
                                             e.getMessage(), e);
            }
        }
    }
            
    public void close()
            throws TraceFileException {
                
        try {
            dos.close();
        } catch (IOException e) {
            throw new TraceFileException("Error closing trace"+
                                         " java.io.IOException: "+
                                         e.getMessage(), e);
        } finally {
            closeNamesFile();
        }
    }

    public class TraceFileException extends Exception {
        public TraceFileException(String msg) {
            super("[Trace File]: "+msg);
        }
        public TraceFileException(String msg, Exception cause) {
            super("[Trace File]: "+msg, cause);
        }
    }
}

