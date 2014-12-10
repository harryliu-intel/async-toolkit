/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2000 Asynchronous Digital Design.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.tools.sigscan;
import com.avlsi.util.container.Pair;
import java.util.Iterator;
import java.util.ArrayList;

/**
 * Class for events that begin transactions
 *
 * @author Dan Daly
 * @version $Date$
 **/

class BeginTranEvent extends LogEvent {

    protected final Sigscan sigscan;
    
    protected final TransactionFiber fiber;
    
    private ArrayList attrs;

    private final String label;
    private final String desc;
    private final TransactionType type;
    private Transaction tran=null;
    /**
     * Constructor.
     **/
    BeginTranEvent(Sigscan sigscan, TransactionFiber fiber, long time,
                          TransactionType type, Transaction tran,
                          String label, String desc) {
        super(time);
        this.fiber = fiber;
        this.sigscan = sigscan;
        this.type = type;
        this.tran = tran;
        this.label = label;
        this.desc = desc;
        attrs = null;
    }

    BeginTranEvent(Sigscan sigscan, TransactionFiber fiber, long time,
                          TransactionType type, 
                          String label, String desc) {
        this(sigscan, fiber, time, type, null, label, desc);
    }
    
    protected void actionPerformed() {
        /*
        try { sigscan.convertAndSetTime(time); } catch (SigscanException e) { 
            fiber.setTimeError( new SigscanException(e.getMessage()+
                                "\n\tbeginTran lab="+label+" desc= "+desc)
            );
        }*/
        if (type == null) {
            System.out.println("Error, BeginTranEvent Error, Tran Type=null"+
                               " (label="+label+" desc= "+desc+")");
            return;
        }
        Transaction tempTran = //fiber.beginTransaction(type, label, desc,time);
                               sigscan.beginTransaction(fiber, type,
                                       label, desc);
        if (tran != null) tran.setHandle(tempTran.getHandle());
        else              tran = tempTran;
        //System.out.println("Firing "+label+", "+desc+" @ "+time);
        if (attrs != null) {
            for (Iterator i = attrs.iterator();i.hasNext();) {
                Pair p = (Pair) i.next();
                ((Attribute) p.getFirst()).set((AttributeValue) p.getSecond());
            }
        }
        //if (notifyFlag) { fiber.doNotify(); }
    }

    public void setAttribute(Attribute attr, AttributeValue value) {
        if (attrs == null) attrs = new ArrayList();
        attrs.add(new Pair(attr, value));
    }

    public Transaction getTransaction() { return tran; }

    public String toString() {
        return "BeginTranEvent at "+time+" on "+fiber.getFullname()+" \""+label+"\" "+desc;
    }
}

