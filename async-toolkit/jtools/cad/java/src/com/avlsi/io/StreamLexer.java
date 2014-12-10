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

package com.avlsi.io;

import java.io.IOException;
import java.io.Reader;
import java.util.Stack;

import com.avlsi.util.debug.Debug;

/**
 * XXX write docs
 * The implementation is unsynchronized.
 *
 * @design jmr The line and column number functionality could be factored
 *      out into another reader, perhaps an extension of
 *      PositionStackReader.
 * @design jmr Comment stripping could also be factored out.
 *
 * @todo jmr Add support for simultaneous multiple comment styles
 *   ie //, --, ;, #, and % all at once like svasm
 *
 * @todo jmr Way to make eol significant, to do line oriented parsing
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class StreamLexer {

    /**
     * Wrapped reader to be tokenized, wrapped by a PositionStackReader
     * to support pushing and popping of positions.  Null to indicate 
     * that the StreamLexer has been closed.
     **/
    private PositionStackReader psr;

    /**
     * stack to keep track of line and column number for
     * {@link #restorePosition}.
     **/
    private final Stack lineColStack = new Stack();

    /**
     * The current line number.
     **/
    private int lineNumber = 1;

    /**
     * The current column number.
     **/
    private int columnNumber = 0;

    /**
     * Determines if eol's are just whitespace, or are a token.
     **/
    // private boolean eolIsSignificantP = false;

    //
    // character types
    //

    /**
     * Characters in this string are considered whitespace.
     **/
    private String whitespaceChars;

    /**
     * Characters in this string begin and end quoted strings.
     **/
    private String quoteChars;

    //
    // single line comments
    //

    /**
     * Whether single line comments should be recognized.
     **/
    private boolean singleLineCommentsP;

    /**
     * String that starts a single-line comment.
     **/
    private String singleLineCommentsBegin;

    /**
     * Whether a number is allowed to start an identifier
     **/
    private boolean digitStartsIdentifierP;



    //
    // multi line comments
    //

    /**
     * Whether multi line comments should be recognized.
     **/
    private boolean multiLineCommentsP;

    /**
     * whether multi-line comments are nested
     **/
    private boolean multiLineCommentsNestedP;

    /**
     * String that starts a multi-line comment.
     **/
    private String beginMultiLineComment;

    /**
     * String that ends a multi-line comment.
     **/
    private String endMultiLinesComment;


    //
    // Constructor
    //

    /**
     * Create a lexer that operates on the given character stream.
     *
     * @exception  NullPointerException if r is null
     **/
    public StreamLexer(final Reader r) {
        this();

        if (r == null)
            throw new NullPointerException();

        psr = new PositionStackReader(r);
    }

    /**
     * Private constructor that initializes everything except the reader.
     **/
    private StreamLexer() {
        // whitespace
        final StringBuffer sb = new StringBuffer();
        for (char ch = '\0'; ch <= ' '; ++ch)
            sb.append(ch);
        setWhitespaceChars(sb.toString());

        // quote chars
        quoteChars = "\"";

        // multi line comments
        multiLineCommentsP = false;
        beginMultiLineComment = null;
        endMultiLinesComment = null;
        multiLineCommentsNestedP = false;

        // single line comments
        singleLineCommentsP = false;
        singleLineCommentsBegin = null;

        // numbers cannot start identifiers
        digitStartsIdentifierP = false;
    }


    /**
     * Check to make sure that the stream has not been closed,
     * throw IOException if it has.
     *
     * @throws IOException  if the stream has been closed
     **/
    private void ensureOpen() throws IOException {
        if (psr == null)
            throw new IOException("Stream closed");
    }


    //
    // Behavior control methods.
    //

    /**
     * Specifies that any character appearing in the string
     * <code>whitespaceChars</code> is a white space character.
     **/
    public void setWhitespaceChars(final String whitespaceChars) {
        this.whitespaceChars = whitespaceChars;
    }

    /**
     * Specifies the beginning and ending strings for multi-line 
     * comments, and whether or not they are nested.  A C style
     * comment would be specified by
     * <code>lex.setMultiLineComments("&#47;*", "*&#47;",  false);</code>
     **/
    public void setMultiLineComments(final String begin,
                                     final String end,
                                     final boolean nested) {
        multiLineCommentsP = true;
        beginMultiLineComment = begin;
        endMultiLinesComment = end;
        multiLineCommentsNestedP = nested;
    }

    /**
     * Specifies the beginning string for single-line
     * comments.  A C++ style * comment would be specified by
     * <code>lex.setSingleLineComments("//");</code>
     **/
    public void setSingleLineComments(final String begin) {
        singleLineCommentsBegin = begin;
    }

    /**
     * Specifies that matching pairs of this character delimit string 
     * constants.
     **/
    public void addQuoteChar(char ch) {
        quoteChars += ch;
    }

    /**
     * Specifies whether a number is allowed to start an identifier.
     **/
    public void setDigitStartsIdentifier(
            final boolean digitStartsIdentifierP) {
        this.digitStartsIdentifierP = digitStartsIdentifierP;
    }

    //
    // private character type checking methods
    //

    /**
     * Returns whether a character is considered whitespace.
     **/
    private boolean charIsWhitespace(final int ch) {
        return whitespaceChars.indexOf(ch) != -1;
    }

    /**
     * Returns whether a character is considered a digit.
     * @todo jmr, extend this for other arbitrary radix.
     **/
    private boolean charIsDigit(final int ch) {
        return '0' <= ch && ch <= '9';
    }

    /**
     * Returns whether a character is considered a quote character.
     **/
    private boolean charIsQuote(final int ch) {
        return quoteChars.indexOf(ch) != -1;
    }

    /**
     * Returns whether a character is considered an alphabetic character.
     **/
    private boolean charIsAlpha(final int ch) {
        return ch == '_'
            || ('a' <= ch && ch <= 'z')
            || ('A' <= ch && ch <= 'Z');
    }


    //
    //
    // private parsing methods
    //
    //

    //
    // char parsing: readChar, peekChar
    //

    /**
     * Returns the next character on the stream, and advances the
     * the current position. Adjusts lineNumber and columnNumber.
     *
     * @return  The character read, or -1 if the end of the stream has
     *   been reached.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    private int readChar() throws IOException {
        final int ch = psr.read();

        // XXX handle EOF line / col no
        // XXX do we want to handle \n\r?
        // XXX: this handling isn't quite right for EOL chars
        if (ch == '\n' || ch == '\r') {
            ++lineNumber;
            columnNumber = 0;
        } else
            ++columnNumber;

        return ch;
    }

    /**
     * Returns the next character on the stream, without changing
     * the current position.
     *
     * @return  The character read, or -1 if the end of the stream has
     *   been reached.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    private int peekChar() throws IOException {
        savePosition();

        try {
            return readChar();
        } finally {
            restorePosition();
        }
    }

    // we DO need separate do and read methods:
    //   do: just read it
    //   read: first skip whitespace, then read
    //       actually: read, then read whitespace
    //         or something, see above
    // or something to that effect.


    //
    // whitespace parsing:
    //   readWhitespace, peekWhitespace, isWhitespace, readIfWhitespace
    //


    /**
     * If the string at the head of the stream is whitespace, 
     * return a string of the maximum amount of whitespace possible
     * and advance the position to after the whitespace,
     * otherwise throw TokenNotWhitespaceException and leave the 
     * position unchanged. 
     * Comments are also considered whitespace.
     *
     * @exception  TokenNotWhitespaceException  If there is no
     *     whitespace at the head of the stream
     * @exception  IOException  If an I/O error occurs
     **/
    private String readWhitespace()
        throws TokenNotWhitespaceException, IOException
    {
        savePosition();
        boolean success = false;

        try {
            final StringBuffer sb = new StringBuffer();

            for (;;) {
                if (charIsWhitespace(peekChar()))
                    sb.append((char) readChar());
                else if (multiLineCommentsP
                        && isLiteral(beginMultiLineComment)) {
                    // multi line comment
                    sb.append(readLiteral(beginMultiLineComment));
                    while (!isLiteral(endMultiLinesComment)) {
                        final int ch = readChar();

                        if (ch == -1)
                            throw new TokenNotWhitespaceException(
                                    "EOF in comment");

                        sb.append((char) ch);
                    }
                    sb.append(readLiteral(endMultiLinesComment));
                } else if (singleLineCommentsP
                        && isLiteral(singleLineCommentsBegin)) {
                    // single line comment

                    // read comment chars
                    sb.append(readLiteral(singleLineCommentsBegin));

                    // read until end of line

                    for (;;) {
                        int ch = readChar();

                        if (ch == -1)
                            throw new TokenNotWhitespaceException(
                                    "single line comment at end of file,"
                                    + " no trailing newline");

                        sb.append((char) ch);

                        if (ch == '\n' || ch == '\r')
                            break;
                    }

                    // XXX handle '\n\r'?
                }
                else
                    break;

            }

            if (sb.length() == 0)
                throw new TokenNotWhitespaceException();

            final String s = sb.toString();
            success = true;
            return s;
        } finally {
            if (success)
                discardPosition();
            else
                restorePosition();
        }
    }

    /**
     * If the string at the head of the stream is whitespace, 
     * return a string of the maximum amount of whitespace possible,
     * otherwise throw TokenNotWhitespaceException. Leave the 
     * position unchanged. 
     * Comments are also considered whitespace.
     *
     * @exception  TokenNotWhitespaceException  If there is no
     *     whitespace at the head of the stream
     * @exception  IOException  If an I/O error occurs
     **/
    private String peekWhitespace()
        throws TokenNotWhitespaceException, IOException
    {
        savePosition();

        try {
            return readWhitespace();
        } finally {
            restorePosition();
        }
    }

    /**
     * Returns whether a call to readWhitespace or peekWhitespace would
     * succeed.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    private boolean isWhitespace() throws IOException {
        try {
            peekWhitespace();
            return true;
        } catch (TokenNotWhitespaceException e) {
            return false;
        }
    }

    /**
     * If {@link #isWhitespace} would return true, return 
     * what {@link #readWhitespace} would return, otherwise return null.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    private String readIfWhitespace() throws IOException {
        try {
            return readWhitespace();
        } catch (TokenNotWhitespaceException e) {
            return null;
        }
    }

    //
    // literal parsing:
    //   readLiteral, peekLiteral, isLiteral, readIfLiteral
    //

    /**
     * Returns the literal and advances position if it is at the head of
     * the stream.  If not, throws TokenNotLiteralException and
     * position will be unchanged.
     * Does not read any whitespace first, thus it can be used
     * to check for comment beginning and ending.
     *
     * @exception  TokenNotLiteralException  if the literal was not
     *   at the head of the stream
     * @exception  IOException  If an I/O error occurs
     **/
    private String readLiteral(final String lit)
        throws TokenNotLiteralException, IOException
    {
        savePosition();
        boolean success = false;

        try {
            readIfWhitespace();

            for (int i = 0; i < lit.length(); ++i)
                if (readChar() != lit.charAt(i))
                    throw new TokenNotLiteralException();

            success = true;
            return lit;
        } finally {
            if (success)
                discardPosition();
            else
                restorePosition();
        }
    }

    /**
     * Returns the literal if it is at the head of
     * the stream.  If not, throw TokenNotLiteralException.
     * Position will be unchanged.
     * Does not read any whitespace first, thus it can be used
     * to check for comment beginning and ending.
     *
     * @exception  TokenNotLiteralException  if the literal was not
     *   at the head of the stream
     * @exception  IOException  If an I/O error occurs
     **/
    private String peekLiteral(final String lit)
        throws TokenNotLiteralException, IOException
    {
        savePosition();

        try {
            return readLiteral(lit);
        } finally {
            restorePosition();
        }
    }

    /**
     * Returns whether a call to readLiteral or peekLiteral would
     * succeed.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    private boolean isLiteral(final String lit) throws IOException {
        try {
            peekLiteral(lit);
            return true;
        } catch (TokenNotLiteralException e) {
            return false;
        }
    }

    /**
     * If {@link #isLiteral} would return true, return
     * what {@link #readLiteral} would return, otherwise return null.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    private String readIfLiteral(final String lit) throws IOException {
        try {
            return readLiteral(lit);
        } catch (TokenNotLiteralException e) {
            return null;
        }
    }



    //
    //
    // public parsing methods
    //
    //

    //
    // int parsing:
    //   readInt, peekInt, isInt, readIfInt
    //

    /**
     * First reads whitespace, if any, then returns the next token
     * if it is an integer, advancing the position to after the integer.
     * If the token after optional whitespace is not an integer,
     * throws TokenNotIntException and leaves position unchanged.
     *
     * @return  the integer represented by the next token
     *
     * @exception  TokenNotIntException if the token is not an integer
     * @exception  IOException  If an I/O error occurs
     **/
    public int readInt() throws TokenNotIntException, IOException {
        savePosition();
        boolean success = false;

        try {
            readIfWhitespace();

            final StringBuffer sb = new StringBuffer();

            if (peekChar() == '+')  // parseInt can't handle +
                readChar();
            else if (peekChar() == '-')
                sb.append((char) readChar());

            if (charIsDigit(peekChar())) {
                do {
                    sb.append((char) readChar());
                } while (charIsDigit(peekChar()));
            } else
                throw new TokenNotIntException("no digits");

            final int i = Integer.parseInt(sb.toString());
            success = true;
            return i;
        } catch (NumberFormatException e) {
            throw new TokenNotIntException(e);
        } finally {
            if (success)
                discardPosition();
            else
                restorePosition();
        }

    }

    /**
     * First reads whitespace, if any, then returns the next token
     * if it is an integer.  If the token after optional whitespace is
     * not an integer, throws TokenNotIntException.
     * Always leaves position unchanged.
     *
     * @return  the integer represented by the next token
     *
     * @exception  TokenNotIntException if the token is not an integer
     * @exception  IOException  If an I/O error occurs
     **/
    public int peekInt() throws TokenNotIntException, IOException {
        savePosition();

        try {
            return readInt();
        } finally {
            restorePosition();
        }
    }

    /**
     * Returns whether a call to readInt or peekInt would
     * succeed.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public boolean isInt() throws IOException {
        try {
            peekInt();
            return true;
        } catch (TokenNotIntException e) {
            return false;
        }
    }

    /**
     * If {@link #isInt} would return true, return a java.lang.Integer
     * representing what {@link #readInt} would return,
     * otherwise return null.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public Integer readIfInt() throws IOException {
        try {
            return new Integer(readInt());
        } catch (TokenNotIntException e) {
            return null;
        }
    }

    //
    // double:
    //   readDouble, peekDouble, isDouble, readIfDouble
    //

    /**
     * First reads whitespace, if any, then returns the next token
     * if it is a double, advancing the position to after the double.
     * If the token after optional whitespace is not an double,
     * throws TokenNotDoubletException and leaves position unchanged.
     *
     * @return  the double represented by the next token
     *
     * @exception  TokenNotDoubletException if the token is not a double
     * @exception  IOException  If an I/O error occurs
     **/
    public double readDouble() throws TokenNotDoubleException, IOException {
        savePosition();
        boolean success = false;

        try {
            readIfWhitespace();

            // formats parsed are
            // double ::=
            //     [ sign ]  digits .  [ digits ] [ exponent ]
            //     [ sign ]         .    digits   [ exponent ]
            //     [ sign ]  digits               [ exponent ]
            // exponent ::=
            //     ( e | E ) signed_integer
            // signed_integer ::= 
            //     [ sign ] digits
            // sign ::= 
            //     + | -

            final StringBuffer sb = new StringBuffer();

            // we depend on Double.parseDouble to find errors

            // sign
            if (peekChar() == '+' || peekChar() == '-')
                sb.append((char) readChar());

            // pre-decimal digits
            while (charIsDigit(peekChar()))
                sb.append((char) readChar());

            // decimal
            if (peekChar() == '.')
                sb.append((char) readChar());

            // post-decimal digits
            while (charIsDigit(peekChar()))
                sb.append((char) readChar());

            // exponent indicator
            if (peekChar() == 'E' || peekChar() == 'e')
                sb.append((char) readChar());

            // exponent sign
            if (peekChar() == '+' || peekChar() == '-')
                sb.append((char) readChar());

            // exponent digits
            while (charIsDigit(peekChar()))
                sb.append((char) readChar());

            final double d = Double.parseDouble(sb.toString());

            // we were successful
            success = true;
            return d;
        } catch (NumberFormatException e) {
            throw new TokenNotDoubleException(e);
        } finally {
            if (success)
                discardPosition();
            else
                restorePosition();
        }
    }

    /**
     * First reads whitespace, if any, then returns the next token
     * if it is a double.  If the token after optional whitespace is
     * not a double, throws TokenNotDoubleException.
     * Always leaves position unchanged.
     *
     * @return  the integer represented by the next token
     *
     * @exception  TokenNotDoubleException if the token is not an integer
     * @exception  IOException  If an I/O error occurs
     **/
    public double peekDouble()
        throws TokenNotDoubleException, IOException
    {
        savePosition();

        try {
            return readDouble();
        } finally {
            restorePosition();
        }
    }

    /**
     * Returns whether a call to readDouble or peekDouble would
     * succeed.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public boolean isDouble() throws IOException {
        try {
            peekDouble();
            return true;
        } catch (TokenNotDoubleException e) {
            return false;
        }
    }

    /**
     * If {@link #isDouble} would return true, return a java.lang.Double
     * representing what {@link #readDouble} would return,
     * otherwise return null.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public Double readIfDouble() throws IOException {
        try {
            return new Double(readDouble());
        } catch (TokenNotDoubleException e) {
            return null;
        }
    }


    //
    // identifier parsing:
    //   readIdentifier, peekIdentifier, isIdentifier, readIfIdentifier
    //

    /**
     * First reads whitespace, if any, then returns the next token
     * if it is an identifier, advancing the position to after the
     * identifier.
     * If the token after optional whitespace is not an identifier,
     * throws TokenNotIdentifierException and leaves position unchanged.
     * An identifier is a word character (or a number character if
     * <code>setDigitStartsIdentifier(true)</code> was called)
     * followed by one or more
     * word or number characters.
     *
     * @return  the identifier represented by the next token
     *
     * @exception  TokenNotIdentifierException if the token is not an
     *   identifier
     * @exception  IOException  If an I/O error occurs
     **/
    public String readIdentifier()
        throws TokenNotIdentifierException, IOException
    {
        savePosition();
        boolean success = false;

        // ident ::= alpha [ alpha | digit ]*

        try {
            readIfWhitespace();

            final StringBuffer sb = new StringBuffer();

            if (charIsAlpha(peekChar())
                    || (digitStartsIdentifierP && charIsDigit(peekChar()))) {
                sb.append((char) readChar());

                while (charIsAlpha(peekChar()) || charIsDigit(peekChar()))
                    sb.append((char) readChar());
            } else
                throw new TokenNotIdentifierException(
                        "must start with alpha");

            // we were successful
            final String s = sb.toString();
            success = true;
            return s;
        } finally {
            if (success)
                discardPosition();
            else
                restorePosition();
        }
    }

    /**
     * If readIdentifier would successfully return, then return its value.
     * If readIdentifier would throw an exception, throw that same exception.
     * Always leaves position unchanged.
     *
     * @return  the identifier represented by the next token
     *
     * @exception  TokenNotIdentifierException if the token is not an integer
     * @exception  IOException  If an I/O error occurs
     **/
    public String peekIdentifier()
        throws TokenNotIdentifierException, IOException
    {
        savePosition();

        try {
            return readIdentifier();
        } finally {
            restorePosition();
        }
    }

    /**
     * Returns whether a call to readIdentifier or peekIdentifier would
     * succeed.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public boolean isIdentifier() throws IOException {
        try {
            peekIdentifier();
            return true;
        } catch (TokenNotIdentifierException e) {
            return false;
        }
    }

    /**
     * If {@link #isIdentifier} would return true, return
     * what {@link #readIdentifier} would return,
     * otherwise return null.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public String readIfIdentifier() throws IOException {
        try {
            return readIdentifier();
        } catch (TokenNotIdentifierException e) {
            return null;
        }
    }


    //
    // quoted string parsing:
    //   readQuotedString, peekQuotedString, isQuotedString, readIfQuotedString
    //   
    //

    /**
     * First read optional whitespace.  Then, read one of quote 
     * characters, and return string between quotes until matching
     * close quote, including comments and newlines.  On success,
     * position will advance, on failure, it will be unchanged.
     *
     * XXX: Escapes for octal, escaped quote chars, and c style
     *   escapes are not implemented yet.
     *
     * @exception TokenNotQuotedStringException  if the next token
     *    is not a quoted string.
     *
     **/
    public String readQuotedString()
        throws TokenNotQuotedStringException, IOException
    {
        savePosition();
        boolean success = false;

        try {
            readIfWhitespace();

            final int openQuote = readChar();

            if (!charIsQuote(openQuote))
                throw new TokenNotQuotedStringException();

            final StringBuffer sb = new StringBuffer();

            while (peekChar() != openQuote && peekChar() != -1)
                sb.append((char) readChar());

            final int closeQuote = readChar();

            if (closeQuote != openQuote)   // EOF, etc
                throw new TokenNotQuotedStringException();

            final String s = sb.toString();
            success = true;
            return s;
        } finally {
            if (success)
                discardPosition();
            else
                restorePosition();
        }
    }

    /**
     * If readQuotedString would successfully return, then return its value.
     * If readQuotedString would throw an exception, throw that same exception.
     * Always leaves position unchanged.
     *
     * @return  the quoted string represented by the next token
     *
     * @exception  TokenNotQuotedStringException if the token is not an integer
     * @exception  IOException  If an I/O error occurs
     **/
    public String peekQuotedString()
        throws TokenNotQuotedStringException, IOException
    {
        savePosition();

        try {
            return readQuotedString();
        } finally {
            restorePosition();
        }
    }

    /**
     * Returns whether a call to readQuotedString or peekQuotedString would
     * succeed.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public boolean isQuotedString() throws IOException {
        try {
            peekQuotedString();
            return true;
        } catch (TokenNotQuotedStringException e) {
            return false;
        }
    }

    /**
     * If {@link #isQuotedString} would return true, return
     * what {@link #readQuotedString} would return,
     * otherwise return null.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public String readIfQuotedString() throws IOException {
        try {
            return readQuotedString();
        } catch (TokenNotQuotedStringException e) {
            return null;
        }
    }


    //
    // symbol parsing:
    //   readSymbol, peekSymbol, isSymbol, readIfSymbol
    //
    //

    /**
     * First reads whitespace, if any, then returns the next token
     * if it is the specified symbol, advancing the position to after the
     * symbol.  If the token after optional whitespace is not the specified
     * symbol, throws TokenNotSymbolException and leaves position unchanged.
     *
     * @return  the symbol represented by the next token
     *
     * @exception  TokenNotSymbolException if the token is not an integer
     **/
    public String readSymbol(final String sym)
        throws TokenNotSymbolException, IOException
    {
        try {
            return readLiteral(sym);
        } catch (TokenNotLiteralException e)  {
            throw new TokenNotSymbolException(e);
        }
    }

    /**
     * If readSymbol would successfully return, then return its value.
     * If readSymbol would throw an exception, throw that same exception.
     * Always leaves position unchanged.
     *
     * @return  the symbol represented by the next token
     *
     * @exception  TokenNotSymbolException if the token is not an integer
     * @exception  IOException  If an I/O error occurs
     **/
    public String peekSymbol(final String sym)
        throws TokenNotSymbolException, IOException
    {
        savePosition();

        try {
            return readSymbol(sym);
        } finally {
            restorePosition();
        }
    }

    /**
     * Returns whether a call to readSymbol or peekSymbol would
     * succeed.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public boolean isSymbol(final String sym) throws IOException {
        try {
            peekSymbol(sym);
            return true;
        } catch (TokenNotSymbolException e) {
            return false;
        }
    }

    /**
     * If {@link #isSymbol} would return true, return <code>sym</code>
     * (to save memory) otherwise return null.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public String readIfSymbol(final String sym) throws IOException {
        try {
            final String s = readSymbol(sym);
            Debug.assertTrue(s.equals(sym));
            return sym;
        } catch (TokenNotSymbolException e) {
            return null;
        }
    }

    //
    // EOF
    //

    /**
     * First reads whitespace, if any, then returns if the next
     * character is EOF, leaving positino at eof.  
     * If not, throw TokenNotEOFException, and leave position unchanged.
     *
     * @exception  TokenNotEOFException if the token is not EOF
     * @exception  IOException  If an I/O error occurs
     **/
    public void readEOF() throws TokenNotEOFException, IOException {
        savePosition();
        boolean success = false;

        try {
            readIfWhitespace();

            if (readChar() == -1)
                success = true;
            else
                throw new TokenNotEOFException();

        } finally {
            if (success)
                discardPosition();
            else
                restorePosition();
        }
    }

    /**
     * First reads whitespace, if any, then returns if the next
     * character is EOF.  If not, throw TokenNotEOFException.  Always
     * leaves position unchanged.
     *
     * @exception  TokenNotEOFException if the token is not EOF
     * @exception  IOException  If an I/O error occurs
     **/
    public void peekEOF() throws TokenNotEOFException, IOException {
        savePosition();

        try {
            readEOF();
        } finally {
            restorePosition();
        }
    }


    /**
     * Returns whether a call to readEOF or peekEOF would succeed.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public boolean isEOF() throws IOException {
        try {
            peekEOF();
            return true;
        } catch (TokenNotEOFException e) {
            return false;
        }
    }
    
    /**
     * If {@link #isEOF} would return true, then call {@link #readEOF}.
     * If {@link #isEOF} would return false, then return false.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public boolean readIfEOF() throws IOException {
        try {
            readEOF();
            return true;
        } catch (TokenNotEOFException e) {
            return false;
        }
    }


    //
    // close
    //

    /**
     * Close the stream.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public void close() throws IOException {
        if (psr != null) {
            psr.close();
            psr = null;
        }
    }


    //
    // stack ops
    //

    /**
     * Pushes current position onto position stack.
     *
     * @throws IOException  if the stream has been closed
     **/
    public void savePosition() throws IOException {
        ensureOpen();
        psr.savePosition();

        // save line and column
        lineColStack.push(new int[]{getLineNumber(), getColumnNumber()});
    }

    /**
     * Pops position off stack, and moves to that position.
     *
     * @throws EmptyPositionStackException  if there are no saved positions
     * @throws IOException  if the stream has been closed
     **/
    public void restorePosition()
        throws EmptyPositionStackException, IOException
    {
        ensureOpen();
        psr.restorePosition();

        // restore line and column
        final int[] lineCol = (int[]) lineColStack.pop();
        lineNumber = lineCol[0];
        columnNumber = lineCol[1];
    }

    /**
     * Pops position off stack, discarding it.
     *
     * @throws EmptyPositionStackException  if there are no saved positions
     * @throws IOException  if the stream has been closed
     **/
    public void discardPosition()
        throws EmptyPositionStackException, IOException
    {
        ensureOpen();
        psr.discardPosition();

        // discard line and column
        lineColStack.pop();
    }


    //
    // info methods
    //

    /**
     * Return the current line number.
     *
     * @return  the current line number of this stream lexer.
     */
    public int getLineNumber() {
        return lineNumber;
    }

    /**
     * Return the current column number within the line.
     *
     * @return  the current column number of this stream lexer.
     */
    public int getColumnNumber() {
        return columnNumber;
    }

    // toString
}
