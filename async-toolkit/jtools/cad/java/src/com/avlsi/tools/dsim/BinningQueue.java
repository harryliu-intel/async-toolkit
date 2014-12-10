/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.dsim;

import java.util.NoSuchElementException;
import java.util.HashMap;

import com.avlsi.util.debug.Debug;

/**
 * <p> <code>BinningQueue</code> has a heap and a hashmap inside so that events
 * that have the same event time take up only one node within the heap.  This
 * node in the heap represents the head of a linked list which strings together
 * all of the events scheduled at that time.  This allows us to ensure sequence
 * time between the events---that is, events that have the same time will fire
 * in the order in which they are added to the queue.  A heap-based
 * implementation would not normally have this property. </p>
 * 
 * <p> The hashmap is used to match event times to bins.  The hashmap maps times
 * with the last node in the bin for that time.  This is nice because in the
 * case where the bin has already been created, the time for insertion is O(1),
 * instead of O(log n). </p>
 *
 * @author Dan Daly
 * @author <a href="mailto:kiniry@fulcrummicro.com">Joseph Kiniry</a>
 * @version $Date$
 **/

public class BinningQueue implements EventQueueInterface {
    
    // Attributes

    private static final HashMap table = new HashMap();

    private final EventQueue queue = new EventQueue();
                              
    /** Number of events in the queue **/
    private int numEvents = 0;

    // Constructors

    // default constructor only

    // Public Methods

    // documented in interface
    
    /**
     * Add an object to the queue.  Will first check if any other events are
     * scheduled with the same time.  If so, then the event will be put at the
     * end of the linked list for that event time O(1).  If there are no
     * existing events at that time, the event will be put into the heap O(log
     * n) to head up the linked list for that event time. 
     **/
    public void enqueue(final Event e) {
        if (e instanceof SequencedEvent) {
            enqueue((SequencedEvent) e);
        } else {
            //Just a regular event, do not attempt to bin,
            //just toss into the heap
            queue.enqueue(e);
            numEvents++;
        }   
        if (DSimDebug.DEBUG)
            Debug.log("Pending events after enqueue:\n" + pendingList());
    }

    public void enqueue(final SequencedEvent e) {
        Long ltime = new Long(e.getTime());
        SequencedEvent simul = 
            (SequencedEvent) table.remove(ltime);
        if (simul != null) {
            //Simultaneous event exists in the table.  This means
            //that a bin for that event time has already been created.
            //Append the new event to the linked list
            simul.setNextEvent(e);
            e.setIndex(simul.getIndex()); //Used to make remove work right
        } else {
            //We have a new event at this time
            //So put it into the heap to head up the new bin
            queue.enqueue(e);
        }
        assert(e.getIndex() >=0);
        //Then put the new event into the HashMap to update
        //the tail pointer of the linked list
        //This is the insertion point for any new events put
        //into the bin
        table.put(ltime, e);
        numEvents++;
        if (DSimDebug.DEBUG)
            Debug.log("Pending events after enqueue:\n" + pendingList());
    }

    // documented in interface

    public Event next() {
        Event e = (Event) front();
        numEvents--;
        if (e instanceof SequencedEvent) {
            return sequencedNext((SequencedEvent) e);
        } else {
            return queue.next();
        }
    }

    public SequencedEvent getSequencedNext() {
        numEvents--;
        return sequencedNext((SequencedEvent) front());
    }
    
    private SequencedEvent sequencedNext(SequencedEvent top) {
        if (top.hasNextEvent()) {
            assert(top.getIndex() != -1); 
            //This node has simultaneous events
            //Leave the heap alone, and just pop off
            //the head node from the bin
            queue.copyInto(top.getIndex(), top.getNextEvent());
            top.setNextEvent(null);
            top.setIndex(-1);
        } else {
            //Need to remove this tail event from the table
            Long ltime = new Long(top.getTime());
            table.remove(ltime);
            //No other simultaneous events, so remove from heap
            return (SequencedEvent) queue.next();
        }
        return top;
    }

    // documented in interface

    /** 
     * @time-complexity O(1)
     **/
    public int size() { 
        return numEvents; }
  
    // documented in interface

    public Event front() { return queue.front(); }

    // documented in interface

    public boolean isEmpty() { return queue.isEmpty(); }

    // documented in interface

    public void remove(Event e) {
        //queue.remove() will remove the entire bin (bad), 
        //so massage we must do
        if (e.getIndex() < 0) throw new NoSuchElementException(
                "Cannot remove "+e);
        if (e instanceof SequencedEvent) {
            //If the event is a sequence event, we need to traverse
            //the linked-list for that time and find the event.  If
            //the event is at the head of list, we need to update the 
            //event queue.
            //If the event is at the end of list, we need to update the
            //hash table.
            Long ltime = new Long(e.getTime());
            SequencedEvent head = (SequencedEvent) queue.get(e.getIndex());
            SequencedEvent tail = (SequencedEvent) table.get(ltime);
            //If the bin has only one element, just return it
            //If the head of the bin is our guy, just remove it
            //from the bin
            if (head.equals(e)) {
                SequencedEvent newhead = head.getNextEvent();
                if (newhead == null)
                    queue.remove(e);
                else {
                    queue.copyInto(head.getIndex(), newhead);
                    head.setIndex(-1);
                }
                if (tail.equals(e)) {
                    table.remove(ltime);
                }
            } else {
                //The EventQueue will return the head of the bin
                //with the index of the event.  Now its up to us to
                //find this event.
                SequencedEvent last = (SequencedEvent) head;
                SequencedEvent se = (SequencedEvent) head.getNextEvent();
                while (!se.equals(e)) {
                    Debug.assertTrue(se.hasNextEvent());
                    last = se;
                    se = se.getNextEvent();
                }
                //Remove the node from our bin
                last.setNextEvent(se.getNextEvent());
                se.setIndex(-1);
                //Update the hash if its our last entry
                if (e.equals(tail)) {
                    table.remove(ltime);
                    table.put(ltime,last);
                }
            }
        } else {
            queue.remove(e);
        }
        numEvents--;
    }

    // documented in interface

    public String pendingList() {
        return pendingList(false);
    }
    
    public String pendingList(boolean seqEventsOnly) {
        StringBuffer ret = new StringBuffer();
        for (int i=0; i<queue.size(); i++) {
            Event cur = (Event)queue.get(i);
            if (cur!=null) {
                ret.append(cur.getTime()+" : ");
                if (cur instanceof Node) {
                    if (!seqEventsOnly)
                        ret.append(((Node)cur).getEventString()+"\n");
                } else {
                    ret.append(cur.toString()+"\n");
                    if (cur instanceof SequencedEvent) {
                        SequencedEvent se = (SequencedEvent) cur;
                        while (se.hasNextEvent()) {
                            se = se.getNextEvent();
                            ret.append("\t"+se.toString()+"\n");
                          }
                    }
                }
            }
        }
        return ret.toString();
    }

} // end of class BinningQueue

/* 
 * Local Variables:
 * mode:jde
 * fill-column:80
 * End:
 */
