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

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.ServerSocket;
import java.util.ArrayList;
//import java.net.SocketTimeoutException;  //where is this class?????
import java.io.InterruptedIOException;

/**
 *  This class distributes work amoung the clients of the RTE, a link between 
 *  the client and the server is established and maintained by the @link ClientHandlerThread.
 * 
 */
public class RTEServer {
   
    /** 
     **  TO DO: eliminate hardcoded port number
     **         : make the server a little more powerful
     **           (find out which machines are left, which
     **           (cells have not been done simulating, ask it to
     **           (go offline, blah, blah, blah!!!!!
     **         : general code clean up
     **/
 
    /* data members */
    private RegressionTestEngine rte;
    private Object wait_object;
    private int numclients;
    private int stall;
    private ServerSocket portWatcher;
    private boolean status;

    /* Constructor */
    public RTEServer(RegressionTestEngine rte, Object wobject,
		     int nclients, int stall){
	this.rte = rte;
	this.wait_object = wobject;
	this.numclients = nclients;
	this.stall = stall;
	
	/** Grab server port for communication **/
	try {
            this.portWatcher = new ServerSocket(0);
	    this.portWatcher.setSoTimeout(1000); 
	    this.status = true;
        }
        catch (IOException e) {
            System.out.println("FATAL: ServerSocket Failed. "+ e.getMessage());
            this.status = false;
        }
	
    }
    
    public int getPort() { return portWatcher.getLocalPort(); }
    public boolean getStatus(){return this.status; }

    public void launch_jobs() {
	
	int running_clients =0;
        
	while (running_clients < this.numclients && !this.rte.timedOut()) {
	    
	    /** we need to set a timeout on the port watcher ... **/

	    try {
		Socket sock = this.portWatcher.accept();
		ClientHandlerThread handler =
		    new ClientHandlerThread(sock, this.rte, this.wait_object);
		handler.start();
		running_clients++;
	    }
	    catch (InterruptedIOException e){ }
	    catch (IOException e) {
                System.out.println("Exception: " + e.getMessage());
            }
	    if(stall > 0) {
		try {
		    Thread.sleep(RegressionTestEngine.AMINUTE/10); 
		    this.stall -= 1;
		}
		catch (Throwable t){
		    break;
		}
	    }
	    if(stall == 0)break;
	    //essentially if(stall < 0)continue;
	}
	
	System.out.println("RTEServer going offline");
	
	return;
    }
    /** close connection to this server; clients cannot communicate after this **/
    public void close(){
	try {
	    portWatcher.close();
	}
	catch (Exception e){
	    System.out.println("Error closing Socket" +e.getMessage());
	}
    }
}


