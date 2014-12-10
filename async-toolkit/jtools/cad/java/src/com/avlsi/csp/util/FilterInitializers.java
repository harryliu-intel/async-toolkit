package com.avlsi.csp.util;

import java.util.Iterator;
import java.util.Set;

import com.avlsi.csp.ast.*;

public class FilterInitializers extends VisitorByCategory {
    private final VariableAnalyzer.Results analyzerResult;
    private final Set<Type> usedTokens;
    private final SequentialStatement result;
    private boolean keep;
    private boolean isTop;
    public FilterInitializers(final VariableAnalyzer.Results analyzerResult,
                              final Set<Type> usedTokens) {
        this.analyzerResult = analyzerResult;
        this.usedTokens = usedTokens;
        this.result = new SequentialStatement();
    }
    public void visitSequentialStatement(SequentialStatement s)
        throws VisitorException {
        boolean oldTop = isTop;
        isTop = false;
        for (Iterator i = s.getStatements(); i.hasNext(); ) {
            final StatementInterface stmt = (StatementInterface) i.next();
            if (oldTop) keep = false;
            stmt.accept(getVisitor());
            if (oldTop && keep) result.addStatement(stmt);
        }
        isTop = oldTop;
    }
    public void visitIdentifierExpression(IdentifierExpression e)
        throws VisitorException {
        final Type ty = analyzerResult.getType(e);
        if (usedTokens.contains(ty)) keep = true;
    }
    public SequentialStatement filter(SequentialStatement s)
        throws VisitorException {
        isTop = true;
        s.accept(this);
        return result;
    }
}
