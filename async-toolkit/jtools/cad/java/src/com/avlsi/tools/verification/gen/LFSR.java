package com.avlsi.tools.verification.gen;

public class LFSR {

    /**All of the arrays of names BITx contain all of the values
     * I have found for the generation of maximum length LFSR's 
     * for x bit values
     * Any one of these values can be used to generate the LFSR,
     * but ONLY ONE value should be used in any generation.  Switching 
     * in the middle of a generation will not create a proper sequence.
     * 
     * To generate the next number in the sequence use the following:
     * seed = (seed >> 1) ^ ((seed % 2) * value)
     * this does a bitwise shift right on the seed and then
     * XOR's that with the value if the bit "shifted off" was a 1
     * otherwise the shifted seed is the new seed
     * Example:
     * seed = 29 (11101)
     * value = 18 (10010)
     * seed = (11101 >> 1) ^ (1 * 10010)
     * seed = (01110)      ^ (10010) = (11100) = 28
     * (next iteration)
     * seed = (11100 >> 1) ^ (0 * 10010)
     * seed = (01110)      ^ (00000) = (01110) = 12
     * etc...
     **/
    
    private static final int[] BIT3 = {5, 6};
    private static final int[] BIT4 = {9, 12};
    private static final int[] BIT5 = {18, 20, 23, 27, 29, 30};
    private static final int[] BIT6 = {33, 45, 48, 51, 54, 57};
    private static final int[] BIT7 = {65, 68, 71, 72, 78, 83, 85, 92, 95, 96, 
        101, 105, 106, 114, 119, 120, 123, 126};
    private static final int[] BIT8 = {142, 149, 150, 166, 175, 177, 178, 180, 
        184, 195, 198, 212, 225, 231, 243, 250};
    
    //For the purposes I have written so far below, I will use only
    //the first value from each of these arrays.  In theory, another
    //LFSR constructor could be created where you could also seed this 
    //list (choose which value you wanted to use)

    /** Most Significant Bit of <code>range</code>, determines which 
     * generator to use **/
    protected int msb = 0;
    /** Minimum value of the LFSR sequence to be returned **/
    protected int min;
    /** Maximum value of the LFSR sequence to be returned **/
    protected int max;
    /** Length of the range of values of the LFSR sequence, used to determine 
     * which generator to use */
    protected int range;
    /** Seed value for the LFSR sequence **/
    protected int seed;
    /** Value being used for generating the next value in the sequence **/
    protected int generator;

    /** Constructor for LFSR sequence, sets the initial seed to 
     * <code>seed</code> and will only return values between <code>min</code> 
     * and <code>max</code>.
     *
     * @param seed Initial seed of LFSR sequence
     * @param min The minimum value to be returned by this LFSR sequence
     * @param max The maximum value to be returned by this LFSR sequence
     *
     **/
    
    public LFSR(long seed, int min, int max) {
        this.min = min;
        this.max = max;
        //because 0 is not a valid number in an LFSR range
        //the "range" is from 1-26 instead of 0-25 (example)
        this.range = max - min + 1;

        //guarantee seed is within the range
        this.seed = (int) (seed % this.range) + 1;
        
        //figure out how many bits the LFSR will need to be
        this.msb = findMSB(this.range);

        //choose the generator based on how many bits there are
        //in the range
        //use the first element from each array, but you could
        //use any element in the array you wanted.
        switch(this.msb) {
          case 1:
          case 2:
          case 3:
            this.generator = BIT3[0];
            break;
          case 4:
            this.generator = BIT4[0];
            break;
          case 5:
            this.generator = BIT5[0];
            break;
          case 6:
            this.generator = BIT6[0];
            break;
          case 7:
            this.generator = BIT7[0];
            break;
          case 8:
            this.generator = BIT8[0];
            break;
          default:
            this.generator = BIT8[0];
            break;
        }

    }

    /** Determines the most significant bit of <code>morebits</code>
     *
     * @param morebits Finds the most significant bit of this number
     *
     * @return The most significant bit of <code>morebits</code>
     *
     **/
    
    private int findMSB(int morebits) {
        int bits = 0;
        //loops through removing one bit at a time from morebits
        //and incrementing msb
        while (morebits != 0) {
            bits++;
            morebits = (morebits >> 1);
        }

        return bits;
    }

    /** Gets the next value in the LFSR sequence.  
     * Checks to make sure it only returns values within the range of this 
     * LFSR.
     *
     * @return The next value in the LFSR sequence
     *
     **/

    public int getNext() {
        //keep getting the next one until you get one that is in the range
        do {
            next();
        } while (seed > range);

        //the seed value is between 1 and range
        //return the corresponding value that is ACTUALLY between min and max
        return min + seed - 1;
    }

    /** Sets seed to the next value in the LFSR sequence based on the 
     * generator value.
     * Probably should not be called, use getNext() to actually return a 
     * value from the LFSR sequence, this will just advance the sequence 
     * to the next value. <br>
     * To generate the next number in the sequence it does the following: <br>
     * seed = (seed >> 1) ^ ((seed % 2) * value) <br>
     * this does a bitwise shift right on the seed and then <br>
     * XOR's that with the value if the bit "shifted off" was a 1 <br>
     * otherwise the shifted seed is the new seed <br>
     * Example: <br>
     * seed = 29 (11101) <br>
     * value = 18 (10010) <br>
     * seed = (11101 >> 1) ^ (1 * 10010) <br>
     * seed = (01110)      ^ (10010) = (11100) = 28 <br>
     * (next iteration) <br>
     * seed = (11100 >> 1) ^ (0 * 10010) <br>
     * seed = (01110)      ^ (00000) = (01110) = 12 <br>
     * etc... <br>
     *
     **/
        
    protected void next() {
        seed = (seed >> 1) ^ ((seed % 2) * generator);
    }

}
