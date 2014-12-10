package com.avlsi.csp.util;

import java.util.Iterator;

import com.avlsi.csp.ast.Declaration;
import com.avlsi.csp.ast.Declarator;
import com.avlsi.csp.ast.DeclaratorList;
import com.avlsi.csp.ast.IntegerType;
import com.avlsi.csp.ast.SequentialStatement;
import com.avlsi.csp.ast.StatementInterface;
import com.avlsi.csp.ast.Type;
import com.avlsi.csp.ast.VarStatement;
import com.avlsi.csp.ast.VisitorException;
import com.avlsi.util.functions.UnaryPredicate;

/**
 * A class to remove scalar declarations from the initializers.
 **/
public class RemoveScalars extends DeclarationProcessor {
    /**
     * A unary predicate that evaluates to true for declarators that should be
     * filtered away.
     **/
    private final UnaryPredicate filter;
    private DeclaratorList declList = null;

    public RemoveScalars(final UnaryPredicate filter) {
        this.filter = filter;
    }

    public void process(final Declarator d) throws VisitorException {
        // only attempt to filter away integer constants, because it's more
        // complicated to remove constant arrays
        if (!(d.getTypeFragment() instanceof IntegerType) ||
            !filter.evaluate(d)) {
            if (declList == null) declList = new DeclaratorList();
            declList.addDeclarator(d);
        }
    }

    public static SequentialStatement process(
            final SequentialStatement initStmt,
            final UnaryPredicate filter) {
        if (initStmt == null) return null;

        SequentialStatement result = new SequentialStatement();
        for (Iterator i = initStmt.getStatements(); i.hasNext(); ) {
            StatementInterface stmt = (StatementInterface) i.next();
            if (stmt instanceof VarStatement) {
                final VarStatement var = (VarStatement) stmt;
                RemoveScalars rs = new RemoveScalars(filter);
                try {
                    rs.process(var.getDeclarationList());
                } catch (VisitorException e) {
                    throw new AssertionError("Should never happen: " + e);
                }
                stmt = rs.declList == null ?
                      null
                    : new VarStatement(new Declaration(rs.declList));
            }
            if (stmt != null) {
                result.addStatement(stmt);
            }
        }
        return result;
    }
}
