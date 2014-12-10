package com.avlsi.csp.util;

import java.util.Iterator;

import com.avlsi.csp.ast.*;

public class DeclarationProcessor {
    public void process(final DeclarationList list) throws VisitorException {
        for (Iterator i = list.getDeclarations(); i.hasNext(); ) {
            final Declaration d = (Declaration) i.next();
            process(d);
        }
    }

    public void process(final Declaration decl) throws VisitorException {
        process(decl.getDeclaratorList());
    }

    public void process(final DeclaratorList list) throws VisitorException {
        for (Iterator i = list.getDeclarators(); i.hasNext(); ) {
            final Declarator d = (Declarator) i.next();
            process(d);
        }
    }

    public void process(final Declarator d) throws VisitorException { }
}
