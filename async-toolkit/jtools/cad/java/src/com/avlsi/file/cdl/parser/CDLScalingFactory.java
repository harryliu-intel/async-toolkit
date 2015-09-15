/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.lang.Double;

import java.util.Map;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.common.HierName;



public final class CDLScalingFactory implements CDLFactoryInterface {
    private final double mMosWidthScaleFactor;
    private final double mMosLengthScaleFactor;
    private final double mResistanceScaleFactor;
    private final double mCapacitanceScaleFactor;
    private final double mDiodeAreaScaleFactor;
    private final double mInductanceScaleFactor;
    private final double mBipolarAreaScaleFactor;
    
    private final CDLFactoryInterface mChained;

    private final static class ScalingToken extends CDLLexer.InfoToken {
        private final CDLLexer.InfoToken mWrapped;
        private final double mScale;
        
        public ScalingToken( final CDLLexer.InfoToken wrapped, double scale ) {
            super( 0, "" );
            mWrapped = wrapped;
            mScale = scale;
        }

        public String getText( Environment env ) {
            final Double value = getValue( env );
           
            if ( value != null ) {
                return value.toString();
            }
            else {
                return Double.toString( mScale ) + "( " + mWrapped.getText( env ) + " )";
            }
        }

        public Double getValue( Environment env ) {
            final Double wrappedValue = mWrapped.getValue( env );
            if ( wrappedValue != null ) {
                return new Double( mScale * wrappedValue.doubleValue() );
            }
            else {
                return null;
            }
        }

    }

    public CDLScalingFactory( final double mosWidthScaleFactor,
                              final double mosLengthScaleFactor,
                              final double resistanceScaleFactor,
                              final double capacitanceScaleFactor,
                              final double diodeAreaScaleFactor,
                              final double inductanceScaleFactor,
                              final CDLFactoryInterface chained ) {
        this(mosWidthScaleFactor, mosLengthScaleFactor, resistanceScaleFactor,
             capacitanceScaleFactor, diodeAreaScaleFactor,
             inductanceScaleFactor, 1, chained);
    }

    public CDLScalingFactory( final double mosWidthScaleFactor,
                              final double mosLengthScaleFactor,
                              final double resistanceScaleFactor,
                              final double capacitanceScaleFactor,
                              final double diodeAreaScaleFactor,
                              final double inductanceScaleFactor,
                              final double bipolarAreaScaleFactor,
                              final CDLFactoryInterface chained ) {
        mMosWidthScaleFactor = mosWidthScaleFactor;
        mMosLengthScaleFactor = mosLengthScaleFactor;
        mResistanceScaleFactor = resistanceScaleFactor;
        mCapacitanceScaleFactor = capacitanceScaleFactor;
        mDiodeAreaScaleFactor = diodeAreaScaleFactor;
        mInductanceScaleFactor = inductanceScaleFactor;
        mBipolarAreaScaleFactor = bipolarAreaScaleFactor;
        mChained = chained;
        
    }
    
    private CDLLexer.InfoToken scale(CDLLexer.InfoToken val, double factor) {
        return val == null ? null : new ScalingToken(val, factor);
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters, Environment env) {
        mChained.makeResistor( name,
                               n1,
                               n2,
                               scale( val, mResistanceScaleFactor ),
                               parameters,
                               env );
    }

    
    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters, Environment env) {
        mChained.makeCapacitor( name,
                                npos,
                                nneg,
                                scale( val, mCapacitanceScaleFactor ),
                                parameters,
                                env );
    }

    
    public void makeTransistor(HierName name, String type, HierName ns, HierName nd,
                               HierName ng, HierName nb, CDLLexer.InfoToken w,
                               CDLLexer.InfoToken l, Map parameters, Environment env) {
        mChained.makeTransistor( name,
                                 type,
                                 ns,
                                 nd,
                                 ng,
                                 nb,
                                 scale( w, mMosWidthScaleFactor ),
                                 scale( l, mMosLengthScaleFactor ),
                                 parameters,
                                 env );
    }

    
    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken area,  Map parameters, Environment env) {
        mChained.makeDiode( name,
                            type,
                            npos,
                            nneg,
                            scale( area, mDiodeAreaScaleFactor ),
                            parameters,
                            env );
                                              
    }

    
    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters, Environment env) {
        mChained.makeInductor( name,
                               npos,
                               nneg,
                               scale( val, mInductanceScaleFactor ),
                               parameters,
                               env );
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne, CDLLexer.InfoToken area,
                            Map parameters, Environment env) {
        mChained.makeBipolar( name,
                             type,
                             nc,
                             nb,
                             ne,
                             scale( area, mBipolarAreaScaleFactor ),
                             parameters,
                             env );
                                              
    }

    
    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        mChained.makeCall( name, subName, args, parameters, env );
    }

    
    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        mChained.beginSubcircuit( subName, in, out, parameters, env );
    }

    
    public void endSubcircuit(String subName, Environment env) {
        mChained.endSubcircuit( subName, env );
    }
}
