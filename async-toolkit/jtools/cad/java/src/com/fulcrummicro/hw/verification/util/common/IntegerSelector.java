/**
 * @file    IntegerSelector.java
 */

package com.fulcrummicro.hw.verification.util.common;

import java.util.HashSet;

import com.fulcrummicro.hw.verification.util.gen.RandomNumberGenerator;

/**
 * Represents a set of integers from which unique random selections can 
 * be made. 
 */
public class IntegerSelector
{
    final RandomNumberGenerator rng;
    final int minValue;
    final int maxValue;
    final int range;

    final HashSet<Integer> hashSet;

    public IntegerSelector(RandomNumberGenerator rng, int minValue, int maxValue)
    {
        this.rng = rng;
        this.minValue = minValue;
        this.maxValue = maxValue;

        range = maxValue - minValue + 1;
        assert range > 0;

        hashSet = new HashSet<Integer>();
    }

    public IntegerSelector(RandomNumberGenerator rng, int range)
    {
        this(rng, 0, range - 1);
    }

    public int available()
    {
        return range - hashSet.size();
    }

    public void remove(int value)
    {
        hashSet.remove(value);
    }

    public int reserve(int value)
    {
        hashSet.add(value);
        return value;
    }

    public void reset()
    {
        hashSet.clear();
    }

    public int select()
    {
        for (;;)
        {
            int choice = rng.nextInt(range) + minValue;
            if (hashSet.add(choice))
            {
                return choice;
            }
        }
    }

    public int select(int value)
    {
        if (hashSet.add(value))
        {
            return value;
        }
        throw new AssertionError(
            String.format("Value %d already selected", value));
    }

    public int size()
    {
        return hashSet.size();
    }

}   // class IntegerSelector
