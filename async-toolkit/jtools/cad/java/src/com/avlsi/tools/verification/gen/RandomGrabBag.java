package com.avlsi.tools.verification.gen;

import java.util.*;

public class RandomGrabBag implements GrabBag {

    private Random generator;
    private ArrayList data;
    private ArrayList backup;
    private boolean weighted;
    private boolean replace;
    private double defaultWeight = 10.0;
    private double totalWeight;

    /** 
     * Default Constructor creates a Random Grab Bag that has replacement and 
     * is not weighted.  
     * The random number generator is seeded with the current time.
     *
     **/
    
    public RandomGrabBag() {
        this.data = new ArrayList();
        this.weighted = false;
        this.replace = true;
        this.totalWeight = 0.0;
        generator = new Random(System.currentTimeMillis());
    }

    /** Constructor creates a Random Grab Bag in which weighted and 
     * replacement can be defined.
     * The random number generator is seeded with the current time.
     *
     * @param weighted Is this random weighted?
     * @param replace Does this Grab Bag have replacement?
     *
     **/

    public RandomGrabBag(boolean weighted, boolean replace) {
        this.data = new ArrayList();
        this.backup = new ArrayList();
        this.weighted = weighted;
        this.replace = replace;
        this.totalWeight = 0.0;
        generator = new Random(System.currentTimeMillis());
    }

    /** 
     * Constructor creates a Random Grab Bag that has replacement and 
     * is weighted.  
     * The random number generator is seeded with the current time.
     *
     * @param weight The default weight of elements added to the Grab Bag.
     *
     **/
    
    public RandomGrabBag(double weight) {
        this();
        this.weighted = true;
        this.defaultWeight = weight;
    }

    /** Constructor creates a Random Grab Bag in which weighted and 
     * replacement can be defined.
     * The random number generator is seeded with the current time.
     *
     * @param weighted Is this random weighted?
     * @param replace Does this Grab Bag have replacement?
     * @param weight The default weight of elements add to the Grab Bag.
     *
     **/

    public RandomGrabBag(boolean weighted, boolean replace, double weight) {
        this(weighted,replace);
        this.defaultWeight = weight;
    }
            

    /** Adds streamable <code>s</code> to the grab bag.  
     * It will be added with a default weight.  If you are using a weighted 
     * random grab bag, then you should use <code>add(s,weight)</code> so that 
     * you can set the weight.
     *
     * @param s The streamable to be added to the grab bag
     *
     **/

    public void add(Streamable s) {
        this.add(s,defaultWeight);        
    }

    /** Adds streamable <code>s</code> to the grab bag with weight 
     * <code>weight</code>.
     *
     * @param s The Streamable to be added to the grab bag
     * @param weight The weight of this Streamable in the random selection
     *
     **/        

    public void add(Streamable s, double weight) {
        data.add(new WeightedElement(s,weight));
        //if there is no replacement, save the element in the backup list
        if (!replace) {
            backup.add(new WeightedElement(s,weight));
        }
        //increase the totalWeight of this list
        totalWeight += weight;
    }

    /** Gets the next Streamable from the grab bag according to the 
     * appropriate random selection process.
     *
     * @return The next Streamable.
     *
     **/
    
    public Streamable getNext() {
        //decide which next to call based on the whether or not this is 
        //  weighted-random
        if (weighted) {
            return weightedNext();
        } else {
            return normalNext();
        }
    }

    /** Finds the next streamable from the grab bag under regular 
     * (un-weighted) random selection
     *
     * @return The next streamable in the grab bag.
     *
     **/

    private Streamable normalNext() {
        WeightedElement returnObj;
        
        //if the grab bag is empty...
        if (data.isEmpty()) {
            //...return null...
            return null;
        } else {
            //...otherwise get the next streamable
            //get a number between 0 and the size of the grab bag.
            int index = generator.nextInt(data.size());
            //if you have replacement...
            if (replace) {
                //...get the element from the grab bag...
                returnObj = (WeightedElement) data.get(index);
                //...and return the Streamable...
                return (Streamable) returnObj.getData();
            } else {
                //...otherwise remove the element from the grab bag...
                returnObj = (WeightedElement) data.remove(index);
                //...and return the Streamable
                return (Streamable) returnObj.getData();
            }
        }
    }

    /** Finds the next streamable from the grab bag under weighted random 
     * selection
     *
     * @return The next Streamable in the grab bag.
     *
     **/

    private Streamable weightedNext() {
        double sum = 0.0;
        int currIndex = 0;
        double subtract;
        double selection;
        WeightedElement current;
        
        //if the grab bag is empty...
        //(both arguments should have the same result but just to be safe)
        if (totalWeight == 0 || data.isEmpty()) {
            //...return null...
            return null;
        } else {
            //...otherwise find the next streamable
            
            //random double between 0 and 1
            //this chooses the weight totalWeight*selection as the element
            selection = generator.nextDouble();
            //loop through the grab bag summing the weights of each element 
            //  until you get the the element whose weight covers the 
            //  selected weight
            while (sum < totalWeight) {
                current = (WeightedElement) data.get(currIndex);
                //add the weight of this element to the sum
                sum += current.getWeight();
                //if this addition pushed the ratio over the selected value...
                if ((sum / totalWeight) >= selection) {
                    //...return this one...
                    
                    //if replacement...
                    if (replace) {
                        //...return the Streamable in this element...
                        return (Streamable) current.getData();
                    } else {
                        //...otherwise remove the current element from the list
                        current = (WeightedElement) data.remove(currIndex);
                        //subtract the weight of the removed element
                        subtract = current.getWeight();
                        totalWeight -= subtract;
                        //and return the Streamable
                        return (Streamable) current.getData();
                    }
                } else {
                    //...otherwise increment the index
                    currIndex++;
                }
            }   
        } 
        
        return null;
    }


    /** Recreates a grab bag that was emptied because there was no replacement. 
     *
     **/
    
    public void refresh() {
        WeightedElement current;
        
        //only do this if there is no replacement in this grab bag
        if (!replace) {
            totalWeight = 0.0;
            //copy the grab bag from the backup
            data = (ArrayList) backup.clone();
            //recaluculate the totalWeight
            for (Iterator i = data.iterator(); i.hasNext();) {
                current = (WeightedElement) i.next();
                totalWeight += current.getWeight();
            }
        }
    }

    /** Manually sets the seed of the random generator to <code>seed</code>.
     * The seed is defaulted to the current time in the constructor, so this 
     * should only be needed when you want a specific seed.
     *
     * @param seed The new seed of the random generator.
     *
     **/
    
    public void setSeed(long seed) {
        //set the seed
        generator.setSeed(seed);
    }

    /** Reset the default weight of elements of this grab bag to 
     * <code>weight</code>.
     *
     * @param weight The new default weight of elements of this grab bag.
     *
     **/
    
    public void setDefaultWeight(double weight) {
        this.defaultWeight = weight;
    }

    /** Accessor for defaultWeight.
     *
     * @return The defaultWeight of this RandomGrabBag
     *
     **/
    
    public double getDefaultWeight() {
        return defaultWeight;
    }

}
