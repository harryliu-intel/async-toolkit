package com.avlsi.tools.verification.gen;

public class LFSRPacket extends Packet {
    
    private LFSR generator;
    
    
    /** 
     * Constructor creates a Packet of length <code>length</code> and 
     * will generate a LFSR sequence of bytes, seeded by <code>seed</code>.
     * 
     * @param length The length of the packet
     * @param seed The seed for the LFSR generation
     *
     **/
    
    public LFSRPacket(int length, long seed) {
        this.length = length;
        this.position = 0;
        generator = new LFSR(seed,-128,127);
    }

    /** 
     * Constructor creates a Packet of length <code>length</code> and 
     * will generate a LFSR sequence of bytes between <code>min</code> and 
     * <code>max</code> and seeded by <code>seed</code>.
     *
     * @param length The length of the packet
     * @param seed The seed for the LFSR generation
     * @param min The minimum value of the LFSR sequence
     * @param max The maximum value of the LFSR sequence
     *
     **/
    
    
    public LFSRPacket(int length, long seed, byte min, byte max) {
        this.length = length;
        this.position = 0;
        if (min <= max) {
            generator = new LFSR(seed,min,max);
        } else {
            System.err.println("Bad min/max values for LFSRPacket, min and max set to 0.");
            generator = new LFSR(seed,0,0);
        }
    }

    /**
     * Constructor creates a LFSRPacket of the appropriate length to generate
     * all of the bytes between <code>min</code> and <code>max</code> once.  
     * The sequence is seeded by <code>seed</code>.
     *
     * @param seed The seed for the LFSR generation
     * @param min The minimum value of the LFSR sequence
     * @param max The maximum value of the LFSR sequence
     *
     **/    

    public LFSRPacket(long seed, byte min, byte max) {
        this.length = max - min + 1;
        this.position = 0;
        
        if (min <= max) {
            generator = new LFSR(seed,min,max);
        } else {
            System.err.println("Bad min/max values for LFSRPacket, min and max set to 0.");
            generator = new LFSR(seed,0,0);
        }
    }
        

    /**
     * Gets the next byte from the LFSR
     *
     * @return The next random byte.
     *
     **/
    
    private byte nextRandom() {
        return (byte) generator.getNext();
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
        
        int numadded = 0;

        if (offset + length > buffer.length) 
            throw new IndexOutOfBoundsException("Attempting to generate more bytes than the buffer will hold");
        
        //if there is less remaining in packet than space in the buffer...
        if (this.length - this.position < length) {
            //...add everything left in packet...
            for(int index = this.position; index < this.length; index++) {
                buffer[offset] = nextRandom();
                offset++;
                numadded++;
            }
            //reset position counter, we're finished with this packet
            this.position = 0;
            //return the number added
            return numadded;
        } else {
            //...otherwise fill up the buffer
            for(int index = offset; index < offset + length; index++) {
                buffer[index] = nextRandom();
                numadded++;
            }
            //increase position by the number of bytes you just added
            this.position += numadded;
            //return the number added
            return numadded;
        }
    }

    
                                                                                                                                                                                                                                             }
