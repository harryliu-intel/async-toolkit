package com.avlsi.layout;

import java.io.StringReader;

import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveCallback;
import com.avlsi.util.container.Pair;
import com.avlsi.cast.impl.Environment;

import java.util.StringTokenizer;
import java.util.NoSuchElementException;


public class LayerCallback implements DirectiveCallback {

    private class LayerParseException extends Exception {
        public LayerParseException() { 
            super("Error parsing Layer" );
        }
    }

    private static LayerCallback singleton = null;

    //not worth a .g file - must be of form " ( 'layer' 'purpose' ) "
    private Pair resolveLayer(String value, Environment env) {
        final String str1;
        final String str2;

        StringTokenizer st = new StringTokenizer(value, " \n\t'" );
        try {           
            if ( st.nextToken().equals("(") ) {             
                str1 = st.nextToken();
                str2 = st.nextToken();
                if( !st.nextToken().equals(")") )
                    throw new LayerParseException();
            }
            else
                throw new LayerParseException();        
        }
        catch( LayerParseException e) {
            System.out.println(e);
            return null;
        }
        catch( NoSuchElementException e) {
            System.out.println(e);
            return null;
        }
                  
        return new Pair(str1, str2);       
    }

    private LayerCallback() {
    }

    public Object resolve(String type, String value, Environment env) {
        Object ret = null;
        if (type.equals(DirectiveConstants.LAYER_TYPE)) {	   
            ret = resolveLayer(value, env);	 
	}
        return ret;
    }

    public static LayerCallback getInstance() {
        if (singleton == null) singleton = new LayerCallback();
        return singleton;
    }
}
