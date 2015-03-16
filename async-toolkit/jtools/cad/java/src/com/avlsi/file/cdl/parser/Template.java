/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.file.cdl.parser;

import java.io.File;
import java.io.InputStream;
import java.io.Reader;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.lang.Comparable;

import antlr.RecognitionException;
import antlr.TokenStreamException;

import com.avlsi.cast.impl.ChainEnvironment;
import com.avlsi.cast.impl.Environment;
import com.avlsi.cast.impl.FloatValue;
import com.avlsi.cast.impl.LocalEnvironment;
import com.avlsi.cast.impl.Symbol;
import com.avlsi.cast.impl.SymbolRedeclaredException;
import com.avlsi.file.cdl.parser.CDLFactoryInterface;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.CDLInterfaceSimplifier;
import com.avlsi.file.cdl.parser.CDLSimpleInterface;
import com.avlsi.file.cdl.parser.ReadCDLIntoFactory;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.util.container.Pair;
import com.avlsi.util.container.StringContainerIterator;

public class Template implements CDLFactoryInterface {
    private Map templates;
    private List statements;
    private String[] in, out;
    private String mStreamName;
    private Map subcktParam;

    public static class CellRedefinitionException extends Exception {

        private final String mCellName;
        private final String mFirstLocation;
        private final String mSecondLocation;

        public CellRedefinitionException( final String cellName,
                                          final String firstLocation,
                                          final String secondLocation ) {
            super( cellName + " was defined more than once." );
            mCellName = cellName;
            mFirstLocation = firstLocation;
            mSecondLocation = secondLocation;
        }

        public final String getCellName() {
            return mCellName;
        }

        public final String getFirstLocation( ) {
            return mFirstLocation;
        }

        public final String getSecondLocation( ) {
            return mSecondLocation;
        }
    }

    public interface Visitor {
        void resistorStatement( final HierName name,
                                final HierName n1,
                                final HierName n2,
                                final CDLLexer.InfoToken val,
                                final Map parameters,
                                final Environment env );
        void capacitorStatement( final HierName name,
                                 final HierName npos,
                                 final HierName nneg,
                                 final CDLLexer.InfoToken val,
                                 final Map parameters,
                                 final Environment env );

        void transistorStatement( final HierName name,
                                  final String type,
                                  final HierName ns,
                                  final HierName nd,
                                  final HierName ng,
                                  final HierName nb,
                                  final CDLLexer.InfoToken w,
                                  final CDLLexer.InfoToken l,
                                  final Map parameters,
                                  final Environment env );
        void inductorStatement( final HierName name,
                                final HierName npos,
                                final HierName nneg,
                                final CDLLexer.InfoToken val,
                                final Map parameters,
                                final Environment env );
        void diodeStatement( final HierName name,
                                final String type,
                                final HierName npos,
                                final HierName nneg,
                                final CDLLexer.InfoToken val,
                                final Map parameters,
                                final Environment env );
        void callStatement( final HierName name,
                            final String subName,
                            final HierName[] args,
                            final Map parameters,
                            final Environment env );
        void bipolarStatement( final HierName name,
                               final String type,
                               final HierName nc,
                               final HierName nb,
                               final HierName ne,
                               final CDLLexer.InfoToken val,
                               final Map parameters,
                               final Environment env );
        
    }

    private interface DeviceInterface extends Comparable {
        void execute(final Environment inner, final CDLSimpleInterface visitor);
        void execute(final CDLFactoryInterface factory);
        void execute(final CDLFactoryInterface factory, final Environment inner);
        void accept( final Visitor v );
        
    }

    private abstract static class Device implements DeviceInterface {
        abstract int compareToSame(Object o);
        public int compareTo(Object that) {
            int ret = this.getClass().getName().compareTo(that.getClass().getName());
            if(ret != 0)
                return ret;            
            else
                return compareToSame(that);
        }
    }

    private static class Resistor extends Device {
        public HierName name, n1, n2;
        public CDLLexer.InfoToken val;
        public Map parameters;
        public Environment env;
        public Resistor(HierName name, HierName n1, HierName n2,
                        CDLLexer.InfoToken val, Map parameters,
                        Environment env) {
            this.name = name;
            this.n1 = n1;
            this.n2 = n2;
            this.val = val;
            this.parameters = parameters;
            this.env = env;
        }
        public void execute(final Environment inner,
                            final CDLSimpleInterface visitor) {
            Environment env = new ChainEnvironment(this.env, inner);
            final double res = CDLInterfaceSimplifier.getValue(val, env);
            visitor.makeResistor(name, n1, n2, 1 / res);
        }

        public void execute(final CDLFactoryInterface factory) {
            factory.makeResistor(name, n1, n2, val, parameters, env);
        }

        public void execute(final CDLFactoryInterface factory,
                            final Environment inner
                            ) {
            Environment env = new ChainEnvironment(this.env, inner);
            factory.makeResistor(name, n1, n2, val, parameters, env);
        }
        
        public void accept( final Visitor v ) {
            v.resistorStatement( name,
                                 n1,
                                 n2,
                                 val,
                                 parameters,
                                 env );
        }

        public int compareToSame(Object o) {
            return compareToSame((Resistor)o);
        }

        public int compareToSame(Resistor that) {
            return this.name.compareTo(that.name);
        }
    }

    private static class Capacitor extends Device {
        public HierName name, npos, nneg;
        public CDLLexer.InfoToken val;
        public Map parameters;
        public Environment env;
        public Capacitor(HierName name, HierName npos, HierName nneg,
                         CDLLexer.InfoToken val, Map parameters,
                         Environment env) {
            this.name = name;
            this.npos = npos;
            this.nneg = nneg;
            this.val = val;
            this.parameters = parameters;
            this.env = env;
        }
        public void execute(final Environment inner,
                            final CDLSimpleInterface visitor) {
            Environment env = new ChainEnvironment(this.env, inner);
            final double cap = CDLInterfaceSimplifier.getValue(val, env);
            visitor.makeCapacitor(name, npos, nneg, cap);
        }

        public void execute(final CDLFactoryInterface factory) {
            factory.makeCapacitor(name, npos, nneg, val, parameters, env);
        }

        public void execute(final CDLFactoryInterface factory,
                            final Environment inner
                            ) {
            Environment env = new ChainEnvironment(this.env, inner);
            factory.makeCapacitor(name, npos, nneg, val, parameters, env);
        }

        public void accept( final Visitor v ) {
            v.capacitorStatement( name,
                                  npos,
                                  nneg,
                                  val,
                                  parameters,
                                  env );
        }
        public int compareToSame(Object o) {
            return compareToSame((Capacitor)o);
        }

        public int compareToSame(Capacitor that) {
            return this.name.compareTo(that.name);
        }
    }

    private static class Transistor extends Device {
        public HierName name;
        public String type;
        public HierName ns, nd, ng, nb;
        public CDLLexer.InfoToken w, l;
        public Map parameters;
        public Environment env;
        public Transistor(HierName name, String type, HierName ns,
                          HierName nd, HierName ng, HierName nb,
                          CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                          Map parameters, Environment env) {
            this.name = name;
            this.type = type;
            this.ns = ns;
            this.nd = nd;
            this.ng = ng;
            this.nb = nb;
            this.w = w;
            this.l = l;
            this.parameters = parameters;
            this.env = env;
        }
        public void execute(final Environment inner,
                            final CDLSimpleInterface visitor) {
            Environment env = new ChainEnvironment(this.env, inner);
            final double wid = CDLInterfaceSimplifier.getValue(w, env);
            final double len = CDLInterfaceSimplifier.getValue(l, env);

            int typ;
            if (type.startsWith("p") || type.startsWith("P")) {
                typ = DeviceTypes.P_TYPE;
            } else {
                typ = DeviceTypes.N_TYPE;
            }

            visitor.makeTransistor(name, typ, ns, nd, ng, nb, wid, len);
        }
        public void execute(final CDLFactoryInterface factory) {
            factory.makeTransistor(name, type, ns, nd, ng, nb, w, l, parameters, env);
        }

        public void execute(final CDLFactoryInterface factory,
                            final Environment inner
                            ) {
            Environment env = new ChainEnvironment(this.env, inner);
            factory.makeTransistor(name, type, ns, nd, ng, nb, w, l, parameters, env);
        }

        public void accept( final Visitor v ) {
            v.transistorStatement( name,
                                   type,
                                   ns,
                                   nd,
                                   ng,
                                   nb,
                                   w,
                                   l,
                                   parameters,
                                   env );
        }

        public int compareToSame(Object o) {
            return compareToSame((Transistor)o);
        }

        public int compareToSame(Transistor that) {
            return this.name.compareTo(that.name);
        }
    }

    private static class Inductor extends Device {
        public HierName name, npos, nneg;
        public CDLLexer.InfoToken val;
        public Map parameters;
        public Environment env;
        public Inductor(HierName name, HierName npos, HierName nneg,
                        CDLLexer.InfoToken val, Map parameters,
                        Environment env) {
            this.name = name;
            this.npos = npos;
            this.nneg = nneg;
            this.val = val;
            this.parameters = parameters;
            this.env = env;
        }
        
        public void execute(final Environment inner,
                            final CDLSimpleInterface visitor) {
            Environment env = new ChainEnvironment(this.env, inner);
            final double ind = CDLInterfaceSimplifier.getValue(val, env);
            visitor.makeInductor(name, npos, nneg, ind);
        }

        public void execute(final CDLFactoryInterface factory) {
            factory.makeInductor(name, npos, nneg, val, parameters, env);
        }

        public void execute(final CDLFactoryInterface factory,
                            final Environment inner
                            ) {
            Environment env = new ChainEnvironment(this.env, inner);
            factory.makeInductor(name, npos, nneg, val, parameters, env);
        }
        
        public void accept( final Visitor v ) {
            v.inductorStatement( name,
                                 npos,
                                 nneg,
                                 val,
                                 parameters,
                                 env );
        }
        
        public int compareToSame(Object o) {
            return compareToSame((Inductor)o);
        }

        public int compareToSame(Inductor that) {
            return this.name.compareTo(that.name);
        }
    }

    private static class Diode extends Device {
        public HierName name, npos, nneg;
        public CDLLexer.InfoToken val;
        public Map parameters;
        public Environment env;
        public String type;
        public Diode(HierName name, String type, HierName npos, HierName nneg,
                     CDLLexer.InfoToken val, Map parameters,
                     Environment env) {
            this.name = name;
            this.type = type;
            this.npos = npos;
            this.nneg = nneg;
            this.val = val;
            this.parameters = parameters;
            this.env = env;
        }
        
        public void execute(final Environment inner,
                            final CDLSimpleInterface visitor) {
            Environment env = new ChainEnvironment(this.env, inner);
            final double a = CDLInterfaceSimplifier.getValue(val, env);
            final double w = Math.sqrt(a);
            final double l = w;
            final double p = 2*w + 2*l;

            int typ;
            if (type.equals("DW") || type.equals("dw")) {
                typ = DeviceTypes.N_TYPE;
            } else {
                typ = DeviceTypes.P_TYPE;
            }

            visitor.makeDiode(name, typ, npos, nneg, w,l,a,p);
        }

        public void execute(final CDLFactoryInterface factory) {
            factory.makeDiode(name, type, npos, nneg, val, parameters, env);
        }

        public void execute(final CDLFactoryInterface factory,
                            final Environment inner
                            ) {
            Environment env = new ChainEnvironment(this.env, inner);
            factory.makeDiode(name, type, npos, nneg, val, parameters, env);
        }
        
        public void accept( final Visitor v ) {
            v.diodeStatement( name,
                              type,
                              npos,
                              nneg,
                              val,
                              parameters,
                              env );
        }

        public int compareToSame(Object o) {
            return compareToSame((Diode)o);
        }

        public int compareToSame(Diode that) {
            return this.name.compareTo(that.name);
        }
    }

    private static class Bipolar extends Device {
        public HierName name, nc, nb, ne;
        public CDLLexer.InfoToken val;
        public Map parameters;
        public Environment env;
        public String type;
        public Bipolar(HierName name, String type, HierName nc, HierName nb,
                       HierName ne, CDLLexer.InfoToken val, Map parameters,
                       Environment env) {
            this.name = name;
            this.type = type;
            this.nc = nc;
            this.nb = nb;
            this.ne = ne;
            this.val = val;
            this.parameters = parameters;
            this.env = env;
        }
        
        public void execute(final Environment inner,
                            final CDLSimpleInterface visitor) {
            Environment env = new ChainEnvironment(this.env, inner);
            final double a = CDLInterfaceSimplifier.getValue(val, env);

            int typ;
            if (type.startsWith("p") || type.startsWith("P")) {
                typ = DeviceTypes.P_TYPE;
            } else {
                typ = DeviceTypes.N_TYPE;
            }

            visitor.makeBipolar(name, typ, nc, nb, ne, a);
        }

        public void execute(final CDLFactoryInterface factory) {
            factory.makeBipolar(name, type, nc, nb, ne, val, parameters, env);
        }

        public void execute(final CDLFactoryInterface factory,
                            final Environment inner
                            ) {
            Environment env = new ChainEnvironment(this.env, inner);
            factory.makeBipolar(name, type, nc, nb, ne, val, parameters, env);
        }
        
        public void accept( final Visitor v ) {
            v.bipolarStatement( name,
                                type,
                                nc,
                                nb,
                                ne,
                                val,
                                parameters,
                                env );
        }

        public int compareToSame(Object o) {
            return compareToSame((Bipolar)o);
        }

        public int compareToSame(Bipolar that) {
            return this.name.compareTo(that.name);
        }
    }

    private static class Call extends Device {
        public HierName name;
        public String subName;
        public HierName[] args;
        public Map parameters;
        public Environment env;

        public Call(HierName name, String subName, HierName[] args,
                    Map parameters, Environment env) {
            assert env != null;
            this.name = name;
            this.subName = subName;
            this.args = args;
            this.parameters = parameters;
            this.env = env;
        }
        public void execute(final Environment inner,
                            final CDLSimpleInterface visitor) {
            Environment env = new ChainEnvironment(this.env, inner);
            visitor.makeCall(name, subName, args, CDLInterfaceSimplifier.resolveCallParameter(parameters, env));
        }

        public void execute(final CDLFactoryInterface factory) {
            factory.makeCall(name, subName, args, parameters, env);
        }

        public void execute(final CDLFactoryInterface factory,
                            final Environment inner
                            ) {
            Environment env = new ChainEnvironment(this.env, inner);
            factory.makeCall(name, subName, args, parameters, env);
        }

        public void accept( final Visitor v ) {
            v.callStatement( name,
                             subName,
                             args,
                             parameters,
                             env );
        }

        public int compareToSame(Object o) {
            return compareToSame((Call)o);
        }

        public int compareToSame(Call that) {
            return this.name.compareTo(that.name);
        }
    }

    public static class StatementIterator {
        private final Iterator innerIter;
        
        public StatementIterator( final Iterator iter ) {
            innerIter = iter;
        }

        public boolean hasNext() {
            return innerIter.hasNext();
        }

        /** @throws NoSuchElementException **/
        public void next( Visitor v ) {
            final DeviceInterface curr = 
                ( DeviceInterface ) innerIter.next();

            curr.accept( v );
        }
    }

    

    public Template( final Map templates) {
        this( templates, null );
    }

    public Template( final Map templates,
                     final String streamName ) {
        this( templates,
              new ArrayList(),
              new String[0],
              new String[0],
              Collections.EMPTY_MAP,
              streamName );  
    }

    private Template( final Map templates, 
                      final List statements,
                      final String[] in, 
                      final String[] out, 
                      final Map subcktParam,
                      final String streamName )
    {
        this.templates = templates;
        this.statements = statements;
        this.in = in;
        this.out = out;
        this.subcktParam = subcktParam;
        mStreamName = streamName;
    }

    public void sortStatements() {
        Collections.sort(statements);
    }

    public String getStreamName() {
        return mStreamName;
    }

    public StatementIterator getStatements( ) {
        return new StatementIterator( statements.iterator() );
    }

    public void makeResistor(HierName name, HierName n1, HierName n2,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        statements.add(new Resistor(name, n1, n2, val, parameters, env));
    }
    public void makeCapacitor(HierName name, HierName npos, HierName nneg,
                              CDLLexer.InfoToken val, Map parameters,
                              Environment env) {
        statements.add(new Capacitor(name, npos, nneg, val, parameters, env));
    }

    public void makeTransistor(HierName name, String type, HierName ns,
                               HierName nd, HierName ng, HierName nb,
                               CDLLexer.InfoToken w, CDLLexer.InfoToken l,
                               Map parameters, Environment env) {
        statements.add(new Transistor(name, type, ns, nd, ng, nb, w, l, parameters, env));
    }

    public void makeDiode(HierName name, String type, HierName npos, HierName nneg,
                          CDLLexer.InfoToken val,
                          Map parameters, Environment env) {
        statements.add(new Diode(name, type, npos, nneg, val, parameters, env));
    }

    public void makeInductor(HierName name, HierName npos, HierName nneg,
                             CDLLexer.InfoToken val, Map parameters,
                             Environment env) {
        statements.add(new Inductor(name, npos, nneg, val, parameters, env));
    }

    public void makeBipolar(HierName name, String type, HierName nc,
                            HierName nb, HierName ne, CDLLexer.InfoToken val,
                            Map parameters, Environment env) {
        statements.add(new Bipolar(name, type, nc, nb, ne, val, parameters, env));
    }

    public void makeCall(HierName name, String subName, HierName[] args,
                         Map parameters, Environment env) {
        statements.add(new Call(name, subName, args, parameters, env));
    }

    public void beginSubcircuit(String subName, String[] in, String[] out,
                                Map parameters, Environment env) {
        statements = new ArrayList();
        this.in = new String[in.length];
        System.arraycopy(in, 0, this.in, 0, in.length);
        this.out = new String[out.length];
        System.arraycopy(out, 0, this.out, 0, out.length);
        this.subcktParam = parameters;
    }

    public void endSubcircuit(String subName, Environment env) {
        templates.put(subName, new Template(templates, statements, in, out, subcktParam, mStreamName ));
    }

    public Pair<String[],String[]> getArguments() {
        return new Pair<>(in, out);
    }

    public Map getParameters() {
        return subcktParam;
    }

    private Environment params2env(Map parameters) {
        Environment env = new LocalEnvironment();
        for (Iterator i = parameters.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String key = (String) entry.getKey();
            final Double val = (Double) entry.getValue();
            if (val != null) {
                try {
                    env.bind(Symbol.create(key),
                             FloatValue.valueOf(val.doubleValue()));
                } catch (SymbolRedeclaredException e) {
                    System.err.println("Cannot happen!");
                }
            }
        }
        return env;
    }

    private Environment params2env(Map parameters, Environment inner) {
        Environment env = new LocalEnvironment();
        for (Iterator i = parameters.entrySet().iterator(); i.hasNext(); ) {
            final Map.Entry entry = (Map.Entry) i.next();
            final String key = entry.getKey().toString();
            final CDLLexer.InfoToken tok = (CDLLexer.InfoToken) entry.getValue();
            if (tok != null) {
                try {
                    final Double val = tok.getValue(inner);
                    if (val == null) {
                        throw new RuntimeException(
                            "Cannot evaluate: " + tok.getText() + " at " +
                            tok.getFilename() + ":" + tok.getLine() + ":" +
                            tok.getColumn());
                    } else {
                        env.bind(Symbol.create(key),
                                 FloatValue.valueOf(val.doubleValue()));
                    }
                } catch (SymbolRedeclaredException e) {
                    System.err.println("Cannot happen!");
                }
            }
        }
        return env;
    }

    /** Execute this template with the given parameter mapping using visitor. */
    public void execute(Map params, CDLSimpleInterface visitor) {
        execute(params, visitor, null);
    }

    public void execute(Map params, CDLSimpleInterface visitor,
                        String cellName) {
        execute(params2env(params), visitor, cellName);
    }

    public void execute(Environment env, CDLSimpleInterface visitor) {
        execute(env, visitor, null);
    }

    /**
     * Execute this template with the given environment using visitor.
     * @param env The environment to evaluate expressions with
     * @param visitor The visitor to call
     * @param cellName Call beginSubckt and endSubckt with cellName.  If
     * cellName is <code>null</code> then the calls are not made.
     **/
    public void execute(Environment env, 
                        CDLSimpleInterface visitor,
                        String cellName) {
        if (cellName != null) {
            visitor.beginSubcircuit(cellName, in, out);
        }
        for (Iterator i = statements.iterator(); i.hasNext(); ) {
            final DeviceInterface device = (DeviceInterface) i.next();
            device.execute(env, visitor);
        }
        if (cellName != null) {
            visitor.endSubcircuit(cellName);
        }
    }

    public void execute(CDLFactoryInterface factory,
                        Environment env, 
                        String cellName) {
        execute(factory, subcktParam, env, cellName);
    }

    public void execute(CDLFactoryInterface factory,
                        Map params,
                        Environment env, 
                        String cellName) {
        Environment paramEnv = new ChainEnvironment(env,
                                                    params2env(params, env) );
        if (cellName != null) {
            factory.beginSubcircuit(cellName, in, out, params, paramEnv);
        }
        for (Iterator i = statements.iterator(); i.hasNext(); ) {
            final DeviceInterface device = (DeviceInterface) i.next();

            device.execute(factory, paramEnv);
        }
        if (cellName != null) {
            factory.endSubcircuit(cellName, paramEnv);
        }
    }

    public void execute(CDLFactoryInterface factory) {
        for (Iterator i = statements.iterator(); i.hasNext(); ) {
            final DeviceInterface device = (DeviceInterface) i.next();
            device.execute(factory);
        }
    }

    public static void getTemplatesAllowingRedefinition( final StringContainerIterator filesIter,
                                                         final Map templatesMap,
                                                         final List redefinitionExceptions ) {
    
        while ( filesIter.hasNext() ) {
            final String currFileName = filesIter.next();
            final File currFile = new File( currFileName );
            try {
                final InputStream currInputStream = new FileInputStream( currFile );
                final Reader currReader = new BufferedReader( new InputStreamReader( currInputStream,
                                                                                     "UTF-8" ) );
                final Map currMap = new HashMap();
                ReadCDLIntoFactory.readCDL( currReader,
                                            new Template( currMap, currFileName ) );
            
                final Iterator templateIter = currMap.entrySet().iterator();
                
                while ( templateIter.hasNext() ) {
                    final Map.Entry currTemplateEntry = ( Map.Entry ) templateIter.next();
                    final String currTemplateName = ( String ) currTemplateEntry.getKey();
                    final Template currTemplate = ( Template ) currTemplateEntry.getValue();

                    final Template existingTemplate = ( Template ) templatesMap.put( currTemplateName,
                                                                                     currTemplate );

                    if ( existingTemplate != null ) {
                        
                        final Exception redefException =
                            new CellRedefinitionException( currTemplateName,
                                                           existingTemplate.getStreamName(),
                                                           currFileName );
                        redefinitionExceptions.add( redefException );              
                    }
                }
            }
            catch ( IOException e ) {
                throw new RuntimeException( e );
            }
            catch ( RecognitionException e ) {
                throw new RuntimeException( e );
            }
            catch ( TokenStreamException e ) {
                throw new RuntimeException( e );
            }
        }
    }

    public static void getTemplates( final StringContainerIterator filesIter, 
                                     final Map templatesMap ) throws CellRedefinitionException {
        final List redefinitions = new LinkedList();
        getTemplatesAllowingRedefinition( filesIter,
                                          templatesMap,
                                          redefinitions ) ;
        if ( redefinitions.size() > 0 ) { 
            final CellRedefinitionException redefinition = 
                ( CellRedefinitionException ) redefinitions.get( 0 );
            throw redefinition;
        }
                                          
    }


    public void removeTemplate(final String name ) {
        templates.remove(name);
    }

    public boolean containsTemplate(final String name ) {
        return templates.containsKey(name);
    }

    /** Fill map with entries from cell name (String) to Template */
    public static void getTemplates(final Reader file, final Map cells) {
        try {
            ReadCDLIntoFactory.readCDL(file, new Template(cells));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /** Returns a map from cell name (String) to Template */
    public static Map getTemplates(final Reader file) {
        final Map cells = new HashMap();
        getTemplates(file, cells);
        return cells;
    }

    public Template getTemplate(String name) {
        return (Template)templates.get(name);
    }

    public Map getTemplates() {
        return templates;
    }

    public static Template setInOut(final Template templ, final String[] in,
                                    final String[] out) {
        return new Template(null, templ.statements, in, out, templ.subcktParam,
                            templ.mStreamName);
    }
}
