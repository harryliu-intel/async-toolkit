package com.avlsi.tools.verification.gen;

import java.io.*;

public class FilePacket extends Packet {

    /** The Buffered Stream used to read the file **/
    protected BufferedInputStream bIn;
    /** Stores the filename so that the stream can be reopened later **/
    protected File fName;
    /** Has a file i/o error been displayed yet? **/
    protected boolean dispError;
    
    /**
     * Constructor creates a buffered stream for <code>filename</code>
     * so that the file can be read.
     *
     * @param filename The File object to be accessed by this packet.
     *
     **/
    
    public FilePacket(File filename) throws FileNotFoundException {
        this.fName = filename;
        this.bIn = new BufferedInputStream(new FileInputStream(this.fName));
        this.dispError = false;
    }

    /**
     * Constructor creates a buffered stream for <code>filename</code>
     * so that the file can be read.
     *
     * @param filename The path of the file to be accessed by this packet.
     *
     **/
    
    public FilePacket(String filename) throws FileNotFoundException {
        this.fName = new File(filename);
        this.bIn = new BufferedInputStream(new FileInputStream(this.fName));
        this.dispError = false;
    }

    /**
     * Generates a stream of byte data and places it into buffer 
     * <code>buffer</code>.
     * 
     * @param buffer The buffer to be populated with the byte stream
     * @param offset The starting position within the buffer
     * @param length The maximum number of bytes to write to the buffer
     * 
     * @return The number of bytes written to the buffer
     **/

    public int generate(byte[] buffer, int offset, int length) {

        int numRead;

        if (offset + length > buffer.length) 
            throw new IndexOutOfBoundsException("Attempting to generate more bytes than the buffer will hold");
        
        try 
        {
            //read from the file
            numRead = bIn.read(buffer,offset,length);        
            //if there was nothing left in the file...
            if (numRead == -1) {
                //...then reset the file and try again
                //close the stream and recreate it
                bIn.close();
                bIn = new BufferedInputStream(new FileInputStream(fName));
                //try to read again
                numRead = bIn.read(buffer,offset,length);
                //if it returns nothing twice in a row, then the file is empty
                //throw an error
                if(numRead == -1) {
                    throw new IOException("Warning: File "+fName+" is empty!");
                }
            }
            //return the number of bytes read
            return numRead;

        }
        catch (IOException e)
        {
            //caught some IOException
            //if this is the first error of this packet...
            if (!dispError) {
                //...then spit out the error message
                System.err.println(e);
                //don't display the error message anymore...
                dispError = true;
            }
            //there was some kind of error during the read and nothing
            //was read, so return 0.
            return 0;
        }
        
    }

    /**
     * Forces a close of the Buffered Stream.
     *
     * @return True if file close was successful, False otherwise.
     *
     **/
    
    public boolean close() {
        try
        {
            //Close the file and return true
            bIn.close();
            return true;
        }
        catch (IOException e)
        {
            //If there was an error while closing the file
            System.err.println("Exception thrown while attempting to close file");
            System.err.println(e);
            //then return false
            return false;
        }
    }

}
