package com.avlsi.tools.verification.gen;

import java.io.*;

public class Packet implements Streamable {
    
    /** Length of the packet **/
    protected int length;
    /** Current position in the packet **/
    protected int position;
    /** Data in the packet **/
    protected byte[] data;
    
    /** Default Constructor, does nothing. **/

    public Packet() {
    }

    
    /**
     * Constructor creates <code>data</code> to <code>length</code> and 
     * fills it with index values.
     * 
     * @param length The length of the packet to be created
     *
     **/    

    public Packet(int length) {
        this.length = length;
        this.position = 0;
        data = new byte[this.length];
        fillIndex();
    }


    /**
     * Constructor takes <code>input</code> and puts it into <code>data</code>
     * 
     * @param input The byte array to turn into a packet
     *
     **/
    
    public Packet(byte[] input) {
        this.length = input.length;
        this.position = 0;
        data = new byte[this.length];
        System.arraycopy(input,0,data,0,this.length);
    }


    /**
     * Constructor takes string <code>s</code>, converts it to bytes, and 
     * puts it into <code>data</code>
     * 
     * @param s The string to be converted to bytes and put into the packet
     *
     **/
    
    public Packet(String s) {
        //convert string into a byte array
        byte[] stringtobytes = s.getBytes();
        
        this.length = stringtobytes.length;
        this.position = 0;
        data = new byte[this.length];
        //copy the array into data
        System.arraycopy(stringtobytes,0,data,0,this.length);
    }

    /**
     * Fills <code>data</code> with the index values
     *
     **/
    
    
    private void fillIndex() {
        for(int i=0; i<this.length; i++) {
            data[i] = (byte) i;
        }
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
        int numtoadd;

        if (offset + length > buffer.length) 
            throw new IndexOutOfBoundsException("Attempting to generate more bytes than the buffer will hold");
        
        //if there is less data remaining in data than space in the buffer...
        if (this.length - this.position < length) {
            //...add everything left in data...
            numtoadd = this.length - this.position;
            //copy from data to buffer
            System.arraycopy(this.data,this.position,buffer,offset,numtoadd);
            //reset position counter, we're finished with this packet
            this.position = 0;
            //return the number added
            return numtoadd;
        } else {
            //...otherwise fill up the buffer
            numtoadd = length;
            //copy from data to buffer
            System.arraycopy(this.data,this.position,buffer,offset,numtoadd);
            //increase position by the number of bytes you just added
            this.position += numtoadd;
            //return the number added
            return numtoadd;
        }
        
    }

       
}
