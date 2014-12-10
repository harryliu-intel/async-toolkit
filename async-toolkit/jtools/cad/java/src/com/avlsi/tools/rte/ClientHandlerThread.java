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

package com.avlsi.tools.rte;

import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.StringTokenizer;
import com.avlsi.cell.CellInterface;

/**
 * This class handlers all communication between the client and the
 * server once a connection is established. It takes the socket and 
 * dedicates the rest of its lifetime servicing the link between the 
 * RTEClient and RTEServer. 
 *
 * This service might be extended from just servicing the RTE to a 
 * more generic clienthandler. 
 * 
 */
public class ClientHandlerThread implements Runnable {
    
    private String clientmachine;
    private RegressionTestEngine rte;
    private Socket sock;
    private Object wait_object;
    
    /**
     *  Constructor. passed an instance of the rte to get the tests
     *  being performed, the socket handles the associated connection.
     *
     *  For now wait object is deprecated.  
     */
    public ClientHandlerThread (Socket sock, 
				RegressionTestEngine rte, Object wo) {
        this.sock = sock;
        this.rte = rte;
	this.wait_object = wo;
    }

    /**
     * spawn off a separate thread for the clienthandler.
     */
    public void start() {
        new Thread(this, sock.getInetAddress() + ":" + sock.getPort()).start();
    }
    
    /**
     *  handle the actual work associated with dispatching jobs, disabling the 
     *  client when there is nothing left or no more cells to test. 
     *
     *  TO DO: Code clean up, fixup communication protocol blah, blah!
     */
    public void run () {
	
	String s;
	ObjectOutputStream out = null;
	ObjectInputStream in = null;
	try {
	    out = new ObjectOutputStream(sock.getOutputStream());
	    
	    in = new ObjectInputStream(sock.getInputStream());
	    
	}
	catch(IOException e) {
	    System.err.println(e);
	}
	
	//establish connection with new machine and then continue running tests
	try {
	    clientmachine = in.readUTF();
	    System.out.println("RTEServer: Established connection with " + clientmachine);
	}
	catch (IOException e ) {
	    System.out.println("Trouble handling socket to " +
			       sock.getInetAddress() + ":" +
			       sock.getPort() + "message: " + e.getMessage());
	} 

        for (;;) {
	    try {
		
		s = in.readUTF();
		if(s.equals(RTEClient.GET_TEST_STRING)){
		    //get the cell here serialize it and sent it to the 
		    //RTEClient for testing to be done
		    CellInterface cell = rte.getTest();
		    
                    if (cell == null) {
		        out.writeUTF(RTEClient.NO_MORE_CELLS);
                        break;  // no more work todo;
                    }

		    String currcell  =
                        rte.getCellPrefix()+cell.getModuleName() 
                        + " " +cell.getType()
			+" " +RegressionTestEngine.get().hashCellNameToFileName(cell.getType()); 
                    out.writeUTF(currcell); 
                    out.flush();
		    
		    //now wait for the results of the test we just sent
		    //return protocol: CELLNAME \n RESULTS\n
		    String rstr = in.readUTF();
		    if(!currcell.equals(rstr)){
			System.err.println("FATAL: Internal Error. Cell Inconsistency " 
					   +rstr +" " +currcell);
                        break;
		    }
		    else {
			/** FIX ME **/
			SimResults result = null;
			try {
			    result = (SimResults)in.readObject();
			}
			catch(ClassNotFoundException e){
			    e.printStackTrace();
			}
			//write the cellInterface and the SimResults object to table
			rte.writeResults(cell, result);
			
		    }
		}
		else {
		    System.out.println("Protocol violation; received: " + s);
                    break;
		}
	    }
	    catch(IOException e){
		System.out.println("Trouble handling socket to " +
				   sock.getInetAddress() + ":" +
				   sock.getPort() + "message: " + e.getMessage());
                break;
	    }
	}
	try {
	    out.close();
	    in.close();
	    sock.close();
	    System.out.println("Closing connection to " +clientmachine);
	    return;
	}
	catch (IOException e) {
	    System.err.println("Fatal Error occured while attempting to close connection ... Exiting");
	    System.exit(-1);
	}
    }
}
