package com.avlsi.tools.verification.gen;

public class RandomPacket extends Packet {
    

    private java.util.Random generator;
    private byte min;
    private byte max;
    
    /**
     * Constructor creates a RandomPacket of length <code>length</code> that
     * will generate random bytes.
     * 
     * @param length The length of the packet to be created
     *
     **/    

    public RandomPacket(int length) {
        this.length = length;
        this.position = 0;

        //default min/max values for bytes
        this.min = -128;
        this.max = 127;

        //initialize random generator
        generator = new java.util.Random(System.currentTimeMillis());
    }


    /**
     * Constructor creates a RandomPacket of length <code>length</code> that 
     * will generate random bytes between <code>min</code> and 
     * <code>max</code>.
     * 
     * @param length The length of the packet to be created
     * @param min The minimum value of data values
     * @param max The maximum value of data values
     *
     **/
    
    public RandomPacket(int length, byte min, byte max) {
                
        int range;
        int fill;
        
        this.length = length;
        this.position = 0;
        if (min <= max) {
            this.min = min;
            this.max = max;
        } else {
            System.err.println("Bad min/max values for RandomPacket, min and max set to 0.");
            this.min = 0;
            this.max = 0;
        }
        

        //initialize random generator
        generator = new java.util.Random(System.currentTimeMillis());
        
    }

    /**
     * Gets another random byte between the min and max.
     *
     * @return A random byte.
     *
     **/
    
    private byte nextRandom() {
        
        int range = (int) max - min + 1;
        int randomByte = min + generator.nextInt(range);

        return (byte) randomByte;
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

    public void setSeed(long seed) {
        generator.setSeed(seed);
    }
     
}
