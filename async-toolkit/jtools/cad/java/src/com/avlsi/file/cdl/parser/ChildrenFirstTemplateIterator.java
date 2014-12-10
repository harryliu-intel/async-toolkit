/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */


package com.avlsi.file.cdl.parser;


import java.util.NoSuchElementException;

import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.HashSet;
import java.util.NoSuchElementException;
import java.util.Iterator;

import com.avlsi.file.common.HierName;

import com.avlsi.cast.impl.Environment;
import com.avlsi.file.cdl.parser.CDLLexer;
import com.avlsi.file.cdl.parser.Template;
import com.avlsi.file.cdl.parser.TemplateIterator;

public class ChildrenFirstTemplateIterator implements TemplateIterator {

    private static class CallIterator {
        
        public static class Call {
            private final String mName;
            private final String mMasterName;
            
            public Call( final String name,
                         final String masterName ) {
                mName = name;
                mMasterName = masterName;
            }

            public final String getName() {
                return mName;
            }

            public final String getMasterName() {
                return mMasterName;
            }
        }

        private final Template.StatementIterator mIter;
        private Call mCurrCall;

        public CallIterator( Template.StatementIterator iter ) {
            mIter = iter;
            mCurrCall = null;
        }

        public boolean hasNext() {
            while ( ( mCurrCall == null ) && ( mIter.hasNext() ) ) {
                mIter.next( new Template.Visitor() {
                        public void resistorStatement( final HierName name,
                                                       final HierName n1,
                                                       final HierName n2,
                                                       final CDLLexer.InfoToken val,
                                                       final Map parameters,
                                                       final Environment env ) {}
                        public void capacitorStatement( final HierName name,
                                                        final HierName npos,
                                                        final HierName nneg,
                                                        final CDLLexer.InfoToken val,
                                                        final Map parameters,
                                                        final Environment env ) {}
                        
                        public void transistorStatement( final HierName name,
                                                         final String type,
                                                         final HierName ns,
                                                         final HierName nd,
                                                         final HierName ng,
                                                         final HierName nb,
                                                         final CDLLexer.InfoToken w,
                                                         final CDLLexer.InfoToken l,
                                                         final Map parameters,
                                                         final Environment env ) {}
                        
                        public void inductorStatement( final HierName name,
                                                       final HierName npos,
                                                       final HierName nneg,
                                                       final CDLLexer.InfoToken val,
                                                       final Map parameters,
                                                       final Environment env ) {}
                        
                        public void diodeStatement( final HierName name,
                                                    final String type,
                                                    final HierName npos,
                                                    final HierName nneg,
                                                    final CDLLexer.InfoToken val,
                                                    final Map parameters,
                                                    final Environment env ) {}
                        public void bipolarStatement( final HierName name,
                                                      final String type,
                                                      final HierName nc,
                                                      final HierName nb,
                                                      final HierName ne,
                                                      final CDLLexer.InfoToken val,
                                                      final Map parameters,
                                                      final Environment env ) {}
                        public void callStatement( final HierName name,
                                                   final String subName,
                                                   final HierName[] args,
                                                   final Map parameters,
                                                   final Environment env ) {
                            mCurrCall = new Call( name.toString(),
                                                  subName );
                        }
                    } );
            }
            return mCurrCall != null;
        }

        public Call next() {
            if ( hasNext() ) {
                final Call ret = mCurrCall;
                mCurrCall = null;
                return ret;
            }
            else {
                throw new NoSuchElementException( );
            }
        }
    }

    private static class StackFrame {
        private final String mTemplateName;
        private final Template mTemplate;
        private final CallIterator mIter;

        public StackFrame( final Template template,
                           final String templateName ) {
            mTemplate = template;
            mIter = new CallIterator( template.getStatements() );
            mTemplateName = templateName;
        }

        public CallIterator getIter() {
            return mIter;
        }
        
        public Template getTemplate() {
            return mTemplate;
        }

        public String getTemplateName() {
            return mTemplateName;
        }
    }


    private final Set mVisitedTemplateNames;
    private final Stack mStack;
    private final Iterator mMapIter;
    private final Map mTemplateMap;

    private Template mNextTemplate;

    private void push( final String templateName,
                       final Template template ) {
        final StackFrame newFrame = new StackFrame( template, templateName );
        mStack.push( newFrame ); 
    }

    private void push( final String templateName ) {
        final Template template = ( Template ) mTemplateMap.get( templateName );
        if ( template != null ) {
            push( templateName, template );
        }
        else {
            throw new RuntimeException( "Encountered reference to undefined cell \"" +
                                        templateName + "\"." );
        }
    }

    private CallIterator getTopIterator() {
        final StackFrame topFrame = ( StackFrame ) mStack.peek();
        return topFrame.getIter();
    }

    private Template getTopTemplate() {
        final StackFrame topFrame = ( StackFrame ) mStack.peek();
        return topFrame.getTemplate();
    }

    private String getTopTemplateName() {
        final StackFrame topFrame = ( StackFrame ) mStack.peek();
        return topFrame.getTemplateName();
    }

    private boolean wasVisited( final String templateName ) {
        return mVisitedTemplateNames.contains( templateName );
    }

    private void  markAsVisited( final String templateName ) {
        mVisitedTemplateNames.add( templateName );
    }

    public ChildrenFirstTemplateIterator( final Map templateMap ) {
        mTemplateMap = templateMap;
        mMapIter = templateMap.entrySet().iterator();
        mStack = new Stack();
        mVisitedTemplateNames = new HashSet();
        mNextTemplate = null;
    }

    public boolean hasNext() {
        while ( ( mNextTemplate == null ) && 
                ( ( mMapIter.hasNext() ) ||
                  ( ! ( mStack.empty() ) ) ) ) {
            if ( mStack.empty() ) {
                final Map.Entry nextTemplateMapEntry =
                    ( Map.Entry ) mMapIter.next();
                final String templateName = 
                    ( String ) nextTemplateMapEntry.getKey();
                final Template template =
                    ( Template ) nextTemplateMapEntry.getValue();
                push( templateName, template );
            }
            while ( ( mNextTemplate == null ) &&
                    ( ! ( mStack.empty() ) ) ) {
                final CallIterator currCallIter = getTopIterator();

                String currCallMasterName = null;
                
                if ( currCallIter.hasNext() ) {
                    do {
                        final CallIterator.Call call =
                            currCallIter.next();
                        currCallMasterName = call.getMasterName();
                    } while ( ( currCallIter.hasNext() ) &&
                              ( wasVisited( currCallMasterName ) ) );
                }
                if ( ( currCallMasterName != null ) &&
                     ( ! wasVisited( currCallMasterName ) ) ) {
                    push( currCallMasterName );
                }
                else {
                    final String topTemplateName = getTopTemplateName();
                    mNextTemplate = getTopTemplate();
                    mStack.pop();
                    markAsVisited( topTemplateName );
                }
            }
        }
        return mNextTemplate != null;
    }

    public Template next() {
        if ( hasNext() ) {
            final Template ret = mNextTemplate;
            mNextTemplate = null;
            return ret;
        }
        else {
            throw new NoSuchElementException();
        }
    }


}
