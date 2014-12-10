package com.avlsi.tools.verification.gen;

public class OrderedGrabBag implements GrabBag {

    /** The current position of the next element to be returned **/
    private int position;
    /** The list of streamables in this GrabBag **/
    private java.util.ArrayList data;

    /** Constructor creates an empty GrabBag
     *
     **/

    public OrderedGrabBag() {
        this.position = 0;
        data = new java.util.ArrayList();
    }

    /** Adds the streamable <code>s</code> to the OrderedGrabBag
     *
     * @param s The streamable to be added to the OrderedGrabBag
     *
     **/
    
    public void add(Streamable s) {
        data.add(s);
    }


    /** Finds the next streamable in the OrderedGrabBag
     *
     * @return The next streamable in the OrderedGrabBag, null if none remaining
     **/

    public Streamable getNext() {
        Streamable returnObj;
        
        //if the position is past the end of the list...
        if (this.position >= data.size()) {
            //...then return null...
            return null;
        } else {
            //...otherwise return the streamable at the current position
            returnObj = (Streamable) data.get(this.position);
            //and increment the position counter
            this.position++;
            return returnObj;
        }
    }
    

    /** Restores the Ordered Grab Bag to the initial position, to be used when 
     * the Grab Bag reaches its end.
     *
     **/
    
    public void refresh() {
        this.position = 0;
    }
    

}
