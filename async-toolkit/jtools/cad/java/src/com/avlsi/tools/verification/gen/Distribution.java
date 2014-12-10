package com.avlsi.tools.verification.gen;

public class Distribution implements Streamable {
    
    /** Position in the distribution currently being read **/
    private int position;
    /** Number of times the distribution will loop **/
    private int loops;
    /** Number of times the distribution has looped so far **/
    private int currentloop;
    /** Is the distribution still looping? **/
    private boolean looping;
    /** Data of the distribution, holds Streamables **/
    private java.util.ArrayList data;
    
    /**
     * Constructor creates a distribution stream that will loop 
     * <code>numloops</code> times
     * 
     * @param numloops Number of times the stream will loop
     *
     **/   
    
    public Distribution(int numloops) {
        this.loops = numloops;
        this.position = 0;
        this.currentloop = 0;
        this.looping = true;
        data = new java.util.ArrayList();
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
        int totaladded = 0;
        int currentadded = 0;        

        if (offset + length > buffer.length) 
            throw new IndexOutOfBoundsException("Attempting to generate more bytes than the buffer will hold");
                
        //while you are still looping through the data stream
        while (this.looping) {
            //get the next streamable from the data stream
            Streamable s = (Streamable) data.get(this.position);
            //generate your buffer
            currentadded = s.generate(buffer,offset,length);
            //find new offset
            offset += currentadded;
            //find new length remaining
            length -= currentadded;
            //find how many bytes have been added so far
            totaladded += currentadded;
            //if we've reached the end of the buffer...
            if (length == 0)
                //...then quit and return total added
                return totaladded;
            //move to nextposition in stream
            nextposition();
        }

        //we've reached the end of the distribution stream
        //reset the looping variable and return the number of bytes added
        looping = true;
        return totaladded;

    }

    /**
     * Increments the the current position of the distribution stream and 
     * causes it to loop if necessary
     *
     **/
    
    private void nextposition() {
        //increment position
        this.position++;
        //if this pushed us past the end of the stream...
        if (this.position == data.size()) {
            //...reset the position to beginning on stream and increment 
            //  loop counter
            this.position = 0;
            this.currentloop++;
        }
        //if we have now looped too many times...
        if (this.currentloop > this.loops) {
            //...reset the loop counter and tell it to stop looping
            this.currentloop = 0;
            this.looping = false;
        }
                
        return;
    }

    /**
     * Appends a streamable <code>s</code> onto the end of the distribution
     * 
     * @param s The Streamable to be added to the distribution stream
     *
     **/
    
    public void add(Streamable s) {
        data.add(s);
    }
    
    

}
