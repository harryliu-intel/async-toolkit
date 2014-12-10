/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.util.cmdlineargs.defimpl;


import java.util.Stack;
import java.util.NoSuchElementException;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;

import com.avlsi.util.debug.Debug;

import com.avlsi.util.container.StringContainerIterator;

import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.CommandLineArgsIterator;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsBaseImpl;


public  class CommandLineArgsWithConfigFiles extends CommandLineArgsBaseImpl {

    private CommandLineArgs m_InnerArgs;

    public CommandLineArgsWithConfigFiles( CommandLineArgs innerArgs ) {
	m_InnerArgs = innerArgs;
    }

    public CommandLineArgsIterator iterator() {
	return new CommandLineArgsIterator() {
		private Stack m_IterStack = new Stack();
		private Stack m_FileStack = new Stack();
		
		CommandLineArgsIterator m_CurrIterator=m_InnerArgs.iterator();
		
		CommandLineArg m_nextArg=null;

        private CommandLineArgsIterator getIterForFile( String fileName ) {
            File theFile = new File( fileName );

            try {
                FileInputStream inputStream = new FileInputStream( theFile );

                InputStreamReader inputReader = new InputStreamReader( inputStream, "UTF-8" );

                final BufferedReader bufferedReader = new BufferedReader( inputReader );

                return new CommandLineArgsIterator() {
                    private BufferedReader m_Reader = bufferedReader;

                    CommandLineArg m_nextArg = null;

                    public boolean hasNext() {

                        String currLine = null;
                        try {
                            do {
                                currLine = m_Reader.readLine();

                                if ( ( currLine != null ) && ( currLine.length() > 0 ) &&
                                     !isCommentLine( currLine ) ) {
                                    
                                    if(currLine.charAt(currLine.length() - 1) == '\\') {
                                        String lineFrag = "";
                                        do {
                                            currLine = currLine.substring(0, currLine.length() - 1) + " ";
                                            lineFrag = m_Reader.readLine();
                                            int pos;
                                            for(pos = 0; pos < lineFrag.length(); pos++) {
                                                if((lineFrag.charAt(pos) != ' ') &&
                                                   (lineFrag.charAt(pos) != '\t'))
                                                    break; 
                                            }
                                            currLine = currLine + lineFrag.substring(pos);
                                        } while(currLine.charAt(currLine.length() - 1) == '\\');
                                    }

                                    StringBuffer argName = new StringBuffer();
                                    int currCharIndex = 0;
                                    while ( ( currCharIndex < currLine.length() ) &&
                                            ( currLine.charAt( currCharIndex ) != '=' ) ) {
                                        argName.append( currLine.charAt( currCharIndex ) );
                                        ++currCharIndex;
                                    }

                                    if ( currCharIndex < currLine.length() ) {
                                        ++currCharIndex;
                                        String argValue = currLine.substring( currCharIndex );
                                        m_nextArg = new CommandLineArgDefImpl( argName.toString(),
                                                                               argValue );
                                    }
                                    else {
                                        m_nextArg = 
                                            new CommandLineArgWithNoValueDefImpl(argName.toString());
                                    }
                                }
                            } while ( ( m_nextArg == null ) && ( currLine != null ) );

                            if ( m_nextArg == null ) {
                                m_Reader.close();
                            }
                        }
                        catch ( Exception e ) {
                            m_nextArg = null;
                        }



                        return m_nextArg != null;
                    }

                    public CommandLineArg next() {
                        CommandLineArg ret = m_nextArg;
                        if ( ret == null ) {
                            if ( hasNext() ) {
                                ret = m_nextArg;
                                m_nextArg = null;
                            }
                            else {
                                throw new NoSuchElementException();
                            }
                        }
                        else {
                            m_nextArg = null;
                        }
                        return ret;
                    }

                };

            }
            catch( Exception e ) {
                includeFileError( fileName, e );
                return new CommandLineArgsIterator() {
                    public boolean hasNext() {
                        return false;
                    }

                    public CommandLineArg next() {
                        throw new NoSuchElementException();
                    }
                };
            }


        }

		public boolean hasNext() {
		    
		    while ( ( ( m_nextArg == null     ) || ( m_nextArg.getName().equals("config") ) ) && 
                            ( ( ! m_IterStack.empty() ) || ( m_CurrIterator.hasNext()             ) ||
                              ( m_nextArg != null ) ) ) {  

			if ( m_nextArg != null ) {
			    Debug.assertTrue( m_nextArg.getName().equals("config"));
			    String fileName = transformIncludeFilename( m_nextArg.getValue() );

			    if ( m_FileStack.search( fileName ) <= 0 ) {
				m_IterStack.push( m_CurrIterator );
				m_FileStack.push( fileName );
				m_CurrIterator = getIterForFile( fileName );
			    }
			    m_nextArg = null;
			   
			}
			else {
			    //m_nextArg == null
			    if ( m_CurrIterator.hasNext() ) {
				m_nextArg = m_CurrIterator.next();
			    }
			    else {
				if ( ! ( m_IterStack.empty() ) ) {
				    m_CurrIterator = ( CommandLineArgsIterator ) m_IterStack.pop();
				    m_FileStack.pop();
				}
			    }
			}
		    }

		    return m_nextArg != null;
		}

		public CommandLineArg next() {
		    CommandLineArg ret = m_nextArg;
		    if ( ret == null ) {
			if ( hasNext() ) {
			    ret = m_nextArg;
			    m_nextArg = null;
			}
			else {
			    throw new NoSuchElementException();
			}
		    }
		    else {
			m_nextArg = null;
		    }
		    return ret;
		}

	    };
    }

    public StringContainerIterator nonParsedArgumentsIterator() {
	return m_InnerArgs.nonParsedArgumentsIterator();
    }

    /**
     * This is used by the default implementation of isCommentLine()
     * to keep track of whether we are currently inside a multiline
     * comment.  If you override isCommentLine() and don't call
     * the superclass method, then this doesn't mean anything special.
     */
    protected boolean insideMultiLineComment = false;

    /**
     * Determines whether the given line should be treated as a comment.
     * You can override this method to support different
     * comment styles in a subclass.
     *
     * The default implementation supports the following single-line
     * comments:
     *
     * "!", "#", or "//" at the beginning of the line
     *
     * The default implementation supports the following multi-line
     * comments:
     *
     * slash-star at the beginning of a line begins a comment, and
     * star-slash at the end of a line ends the comment.
     *
     * @param  line    the line to check
     * @return true iff the line is a comment
     */
    protected boolean isCommentLine(String line) {
        boolean result = false;

        if (!insideMultiLineComment) {
            if (line.startsWith("!") || line.startsWith("#") ||
                line.startsWith("//"))
                result = true;
            else if (line.startsWith("/*"))
                insideMultiLineComment = true;
        }

        if (insideMultiLineComment) {
            result = true;
            if (line.endsWith("*/"))
                insideMultiLineComment = false;
        }

        return result;
    }

    /**
     * Performs a transformation on a filename specified with "config=",
     * before actually trying to access the file.  The default implementation
     * returns the string unchanged, but you can override this method
     * in a subclass to perform variable substitution, search path
     * lookup, etc.
     *
     * @param  filename   the filename to transform
     * @return the transformed filename
     */
    protected String transformIncludeFilename(String filename) {
        return filename;
    }

    /**
     * This method is called when an exception happens when trying
     * to read a config file.  The default implementation does
     * nothing, which means that missing config files are silently ignored.
     * You can override this method to do something, such as set a flag,
     * throw a RuntimeException, or print a message and terminate.
     *
     * @param  filename   the name of the file on which the error occurred
     * @param  e          the exception which was thrown when reading the file
     */
    protected void includeFileError(String filename, Exception e) {
    }

    public static void main( String args[] ) {
	CommandLineArgs parsedArgs = 
	    new com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl( args );

	CommandLineArgs argsWithConfigs =
	    new com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles( parsedArgs ); 

	CommandLineArgs cachedArgs = 
	    new com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs( argsWithConfigs );

	CommandLineArgs theArgs = argsWithConfigs;

	CommandLineArgsIterator i = theArgs.iterator();
	
	while ( i.hasNext() ) {
	    CommandLineArg curr = i.next();
	    System.out.println("Name:");
	    System.out.println(curr.getName() );
	    System.out.println("Value:");
	    System.out.println(curr.getValue() );
	    System.out.println();
	}

	CommandLineArg testArg = theArgs.getArg( "foo" );
	if ( testArg != null ) {
	    System.out.println("--foo was an argument!");
	    if ( testArg.getValue() != null ) {
		System.out.println("--foo's value is: \"" + testArg.getValue() + "\"");
	    }
	}
	StringContainerIterator j = theArgs.nonParsedArgumentsIterator();
	System.out.println( "Nonparsed arguments");
	System.out.println( "-------------------" );
	while ( j.hasNext() ) {
	    String curr = j.next();
	    System.out.println( curr );
	}
	System.out.println( "-------------------" );
	System.out.println( cachedArgs.toString() );
	

    }

}
