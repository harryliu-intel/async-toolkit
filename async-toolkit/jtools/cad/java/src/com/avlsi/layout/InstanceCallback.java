package com.avlsi.layout;

import java.io.StringReader;
import java.util.Iterator;

import antlr.RecognitionException;
import antlr.TokenStreamSelector;
import antlr.TokenStreamException;
import antlr.collections.AST;

import com.avlsi.cell.CellInterface;
import com.avlsi.file.common.HierName;
import com.avlsi.cast.impl.ASTWithInfo;
import com.avlsi.cast.impl.ArrayValue;
import com.avlsi.cast.impl.ChainEnvironment;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.NodeValue;
import com.avlsi.cast.impl.TokenWithInfo;
import com.avlsi.cast.impl.Value;
import com.avlsi.cast.impl.InstanceValue;
import com.avlsi.cast2.impl.CastTwoLexer;
import com.avlsi.cast2.impl.CastTwoParser;
import com.avlsi.cast2.impl.CastTwoTreeParser;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.util.container.Pair;

public class InstanceCallback implements DirectiveCallback {
    private static InstanceCallback singleton = null;

    private Value getValue(String value, Environment env) {
        final CastTwoParser castParser =
            CastTwoParser.getParser(value, 0, 0, "<no name>");
       
        try {
            castParser.startPrsNodeExpression();
        } catch (RecognitionException e) {
            System.out.println(e);
            return null;
        } catch (TokenStreamException e) {
            System.out.println(e);
            return null;
        }
        
        final AST port = castParser.getAST();

        final CastTwoTreeParser treeParser = new CastTwoTreeParser();

        Value v = null;

        try {
            v = treeParser.expr(port, env, false);
        } catch (RecognitionException e) {}

        return v;
    }

    private Pair getInstance(String value, Environment env) {
        final Value v = getValue(value, env);
        if (v instanceof InstanceValue) {
            return new Pair(((InstanceValue) v).getCell(),
                            ((InstanceValue) v).getInstanceName());
        } else {
            return null;
        }
    }

    private HierName resolveInstance(String value, Environment env) {
        Pair pair = getInstance(value,env);
        if(pair == null)
            return null;
        CellInterface cell = (CellInterface) pair.getFirst();
        HierName instanceName = (HierName) pair.getSecond();
        return instanceName;
    }    

    private HierName resolveInstances(String value, Environment env) {
        final Value v = getValue(value, env);
        if (v instanceof ArrayValue) {
            final ArrayValue av = (ArrayValue) v;
            for (Iterator i = av.getIterator(); i.hasNext(); ) {
                if (i.next() instanceof InstanceValue)
                    return v.getInstanceName();
                else
                    return null;
            }
        }
        return null;
    }
    
    private HierName resolveChannel(String value, Environment env) {
        Pair pair = getInstance(value,env);
        if(pair == null)
            return null;
        CellInterface cell = (CellInterface) pair.getFirst();
        HierName instanceName = (HierName) pair.getSecond();
        if(cell.isChannel())
            return instanceName;
        else
            return null;
    }

    private InstanceCallback() {
    }

    public Object resolve(String type, String value, Environment env) {
        Object ret = null;
        if (type.equals(DirectiveConstants.CHANNEL_TYPE)) {           
            ret = resolveChannel(value, env);         
        }
        else if(type.equals(DirectiveConstants.INSTANCE_TYPE)) {
            ret = resolveInstance(value, env);
        }
        else if(type.equals(DirectiveConstants.ARRAYED_INSTANCE_TYPE)) {
            ret = resolveInstances(value, env);
        }
        return ret;
    }

    public static InstanceCallback getInstance() {
        if (singleton == null) singleton = new InstanceCallback();
        return singleton;
    }
}
