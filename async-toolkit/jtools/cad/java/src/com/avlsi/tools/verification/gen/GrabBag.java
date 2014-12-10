package com.avlsi.tools.verification.gen;

public interface GrabBag {


    /** Finds the next streamable in the GrabBag based on they type of GrabBag 
     * and returns it.
     *
     * @return The next streamable in the GrabBag, null if GrabBag empty
     **/
    Streamable getNext();

    /** Recreates or resets the GrabBag.  
     * If getNext() returns null then you probably want to run refresh()
     *
     **/
    
    void refresh();
}
