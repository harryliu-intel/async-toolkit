// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

package com.avlsi.tools.verification.gen;

public class WeightedElement {

    /** The data of this element **/
    private Streamable data;
    /** The weight of this element **/
    private double weight;

    /** Constructor creates the element containing Streamable <code>s</code> 
     * with wieght <code>weight</code>
     *
     * @param s The Streamable to be held by this element
     * @param weight The weight of this element in the list
     *
     **/
    
    public WeightedElement(Streamable s, double weight) {
        this.data = s;
        this.weight = weight;
    }

    /** Accessor for the data of the element (the Streamable)
     *
     * @return Streamable held in this element
     *
     **/
    
    public Streamable getData() {
        return (Streamable) data;
    }

    /** Accessor for the weight of this element
     *
     * @return The weight of this element
     *
     **/
    
    public double getWeight() {
        return weight;
    }

}
