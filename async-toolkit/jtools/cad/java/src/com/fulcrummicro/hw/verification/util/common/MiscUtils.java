package com.fulcrummicro.hw.verification.util.common;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;

import com.fulcrummicro.hw.verification.lib.bitarray.LittleEndianBitArray;
import com.fulcrummicro.hw.verification.util.gen.RandomNumberGenerator;

public class MiscUtils {

    /**
     * picks a random number from 0..N where N is the size of the array of
     * probabilities
     *
     * @param prob The list of probabilities, each in the range 0..100 such that
     *             the sum is equal to or less than 100
     * @param rng  The random number generator to use
     *
     * @author Naru Sundar
     */
    public static int pickRandom(int[] prob, RandomNumberGenerator rng) {
        long r = rng.next(0, 100);

        assert(prob.length > 0) : "pickRandom: no probabilities given";

        int action = 0;
        do {
            r -= prob[action];

            if(r <= 0)
                return action;
            else
                action++;
        } while(action < prob.length);

        return action;
    }

    /**
     * Generates an ArrayList of length numRands of vectorSize bit width with each entry
     * being a unique random vector. This works only if the number of possible values that
     * the vector can represent is larger than the number of random vectors desired. The algorithm 
     * also slows down if the number of vectors desired is close to the maximum.
     * 
     */
    public static ArrayList<LittleEndianBitArray> generateUnique(int vectorSize, int numRands, RandomNumberGenerator rng) {
        HashMap<String, LittleEndianBitArray> map = new HashMap<String, LittleEndianBitArray>();
        assert Math.pow(2, vectorSize) > numRands : "Error: It is impossible to generate this many rands given the input vector size"; 
        while(map.size() <= numRands) {
            LittleEndianBitArray vec = new LittleEndianBitArray(vectorSize);
            vec.randomize(rng);
            // This should overwrite any vectors that have the same value
            map.put(vec.toHexString(), vec);
        }
        ArrayList<LittleEndianBitArray> result = new ArrayList<LittleEndianBitArray>();
        for (String hex : map.keySet()) {
            result.add(map.get(hex));
        }
        return result;
    }
    
    /**
     * Generates an shuffled array of length dictated by max
     */
    public static int[] shuffleInts(int[] inArray, RandomNumberGenerator rng) {
        // Shuffle by exchanging each element randomly
        for (int i=0; i<inArray.length; i++) {
            int randPos = rng.nextInt(inArray.length);
            int temp = inArray[i];
            inArray[i] = inArray[randPos];
            inArray[randPos] = temp;
        }
        return inArray;
    }

    /**
     * Generates an ArrayList of length numRands of vectorSize bit width with each entry
     * being a non-unique random vector. 
     * 
     */
    public static ArrayList<LittleEndianBitArray> generateNonUnique(int vectorSize, int numRands, RandomNumberGenerator rng) {
        ArrayList<LittleEndianBitArray> result = new ArrayList<LittleEndianBitArray>();
        for(int i = 0; i < numRands; i++) {
            LittleEndianBitArray vec = new LittleEndianBitArray(vectorSize);
            vec.randomize(rng);
            // This should overwrite any vectors that have the same value
            result.add(vec);
        }
        return result;
    }

    /**
     * calculate 16-bit one's complement sum for bytes array
     *
     * @param byteArray  The array containning bytes to be added
     *        offset     The starting index of array where the addition starts
     *        len        The length of bytes to be added (must be even)
    **/
    public static int oneComplSum_16 (byte[] byteArray, int offset, int len){

        int sum = 0;
        for(int i=0; i<len; i+=2) {
           sum += byteArray[offset+i]<<8 | byteArray[offset+i+1];

           if(((sum>>16) & 0xffff) != 0) 
               sum = (sum & 0xffff) + 1;
        }

        return sum;
    }
    
  
    
//  pick one from a group of weighted items
	//rate: a [0, 1) value used to decide which item to pick
	//weights: weights of a group of items
	public static int pickItemByWeight(double rate, int[] weights) {
		int totalWeight = 0;
		for(int i=0; i<weights.length; i++)
			totalWeight += weights[i];
		
		
		for(int parWeight=0, i=0; i<weights.length; i++) {
		    parWeight += weights[i];
			if(rate < ((double)parWeight)/totalWeight) {
				return(i);
			}
		}
	
		return(weights.length-1);
	}
	
	
	
	 //pick a destination from destSet
	 //src:  src value
	 //destSet: a set of dest values
	 //allowSameSrcDest: whether to allow same src and dest values
     public static int pickDest(RandomNumberGenerator rng, int src, Set<Integer>destSet, boolean allowSameSrcDest) {
    	
         Object[] destArray = destSet.toArray();    	
    	 int dest;
    	 do {
    	    dest = (Integer)destArray[(int)rng.next(0, destArray.length)];
    	 }
    	 while((!allowSameSrcDest) && (src==dest));
    		
    	 return dest;	
     }

}
