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

import java.io.FilterReader;
import java.io.IOException;
import java.io.Reader;
import java.util.EmptyStackException;
import java.util.Stack;

import com.avlsi.util.debug.Debug;

/**
 * A character-stream reader that allows saving and restoring of positions
 * on a stack.
 *
 * @design jmr This could be implemented with a single StringBuffer,
 *   keeping track of indices.  That would be a little more complicated,
 *   but no characters would be stored twice as they are now when
 *   chars are copied from head to the top StringBuffer.
 *
 * @author Jesse Rosenstock
 * @version $Name:  $ $Date$
 **/
public class PositionStackReader extends FilterReader {

    /**
     * Stack of StringBuffers storing characters saved for later recall
     * in case of a {@link #restorePosition} or {@link #discardPosition}
     * call.
     * If the stream looked like this: <blockquote><pre>
     * a b c d e f g h i 
     *  ^ ^   ^   ^
     *  p p   p   cur
     *  u u   u   p
     *  s s   s   o
     *  h h   h   s
     * </pre></blockquote>, then the stack would look like:
     * <blockquote><pre>
     * e f
     * c d
     * b
     * </pre></blockquote>
     **/
    private final Stack saveStack = new Stack();

    /**
     * The string from which characters should be fetched,
     * if headPos < head.length.  If headPos == head.length,
     * characters should come from the wrapped reader.  
     **/
    private char[] head = new char[0];

    /**
     * The position in head from which the next character should come,
     * if headPos == head.length, then the next character should come 
     * from the wrapped reader.
     **/
    private int headPos = head.length;

    /**
     * Flag indicating whether the reader has been closed.
     * @see #ensureOpen
     **/
    private boolean closedP = false;

    /**
     * Create a new postion stack reader.
     **/
    public PositionStackReader(final Reader in) {
        super(in);
    }

    /**
     * Check to make sure that the stream has not been closed,
     * throw IOException if it has.
     *
     * @throws IOException  if the stream has been closed
     **/
    private void ensureOpen() throws IOException {
        if (closedP)
            throw new IOException("Stream closed");
    }


    //
    // extends FilterReader
    //

    /**
     * Read a single character.
     * <p>
     * If !saveStack.empty(), we are currently saving characters,
     * and so must add the character to the StringBuffer on top of the
     * stack.
     *
     * @return     The character read, or -1 if the end of the stream has
     *             been reached
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public int read() throws IOException {
        synchronized (lock) {
            ensureOpen();
            final int ch;
            if (headPos < head.length)
                ch = head[headPos++];
            else
                ch = super.read();

            // update StringBuffer on top of stack.
            if (!saveStack.empty() && ch != -1) {
                Debug.assertTrue(ch == (char) ch);
                saveChar((char) ch);
            }

            return ch;
        }
    }

    /**
     * Read characters into a portion of an array.
     *
     * @param      cbuf  Destination buffer
     * @param      off   Offset at which to start writing characters
     * @param      len   Maximum number of characters to read
     *
     * @return     The number of characters read, or -1 if the end of the
     *             stream has been reached
     *
     * @exception  IOException  If an I/O error occurs
     */
    public int read(char cbuf[], int off, int len) throws IOException {
        synchronized (lock) {
            ensureOpen();
            if (len < 0)
                throw new IndexOutOfBoundsException();
            else if (len == 0) {
                if ((off < 0) || (off > cbuf.length))
                    throw new IndexOutOfBoundsException();
                else
                    return 0;
            } else {
                // the number of characters we have in head
                int avail = head.length - headPos;

                // first, get rid of the chars we have
                if (avail > 0) {
                    if (avail > len)
                        avail = len;
                    System.arraycopy(head, headPos, cbuf, off, avail);
                    
                    // update StringBuffer on top of stack
                    if (!saveStack.empty())
                        saveChars(cbuf, off, avail);

                    headPos += avail;
                    off += avail;
                    len -= avail;
                }

                // if they want more chars, get rid of chars from underlying
                // reader
                if (len > 0) {
                    len = super.read(cbuf, off, len);

                    // update StringBuffer on top of stack
                    if (!saveStack.empty() && len > 0)
                        saveChars(cbuf, off, len);

                    if (len == -1)
                        return (avail == 0) ? -1 : avail;
                    return avail + len;
                }

                return avail;
            }
        }
    }

    /**
     * Skips over and discards <code>n</code> characters of data from this
     * input stream. The <code>skip</code> method may, for a variety of
     * reasons, end up skipping over some smaller number of characters,
     * possibly zero.  If <code>n</code> is negative, no characters are
     * skipped.
     *
     * <p> The <code>skip</code> method of <code>PositionStackReader</code>
     * first skips over the characters in the pushback buffer, if any.  It
     * then calls the <code>skip</code> method of the underlying reader if
     * more characters need to be skipped.  The actual number of characters
     * skipped is returned.
     *
     * @param      n   the number of characters to be skipped.
     * @return     the actual number of characters skipped.
     * @exception  IOException  If an I/O error occurs
     **/
    public long skip(long n) throws IOException {
        synchronized (lock) {
            ensureOpen();

            if (n <= 0)
                return 0;

            if (saveStack.empty()) {
                // we're not remembering, so just skip

                final int avail = head.length - headPos;
                final long skipped;

                if (n < avail) {
                    skipped = n;
                    headPos += n;
                    n = 0;
                } else {
                    skipped = avail;
                    headPos = head.length;
                    n -= avail;
                }

                return skipped + super.skip(n);
            } else {
                // we are remembering, so we need to memorize
                // the chars

                if (n > Integer.MAX_VALUE)
                    n = Integer.MAX_VALUE;

                final int nn = (int) n;

                // read will take care of remembering
                final int cnt = read(new char[nn], 0, nn);

                return cnt == -1 ? 0 : cnt;
            }
        }
    }

    /**
     * Tell whether this stream is ready to be read.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public boolean ready() throws IOException {
        synchronized (lock) {
            ensureOpen();
            return headPos < head.length || super.ready();
        }
    }

    /**
     * Tell whether this stream supports the mark() operation, which it does
     * not.
     **/
    public boolean markSupported() {
        return false;
    }

    /**
     * Mark the present position in the stream. The <code>mark</code>
     * for class <code>PushbackReader</code> always throws an exception.
     *
     * @exception  IOException  Always, since mark is not supported
     **/
    public void mark(int readAheadLimit) throws IOException {
        throw new IOException("mark/reset not supported");
    }

    /**
     * Reset the stream. The <code>reset</code> method of
     * <code>PushbackReader</code> always throws an exception.
     *
     * @exception  IOException  Always, since reset is not supported
     **/
    public void reset() throws IOException {
        throw new IOException("mark/reset not supported");
    }

    /**
     * Close the stream.
     *
     * @exception  IOException  If an I/O error occurs
     **/
    public void close() throws IOException {
        super.close();
        closedP = true;
    }



    //
    // push / pop functionality
    //

    /**
     * Saves the current position onto the stack.  
     * <p>
     * Characters read after a call to pushPosition will be appended to
     * the StringBuffer on top of the stack.
     *
     * @throws IOException  if the stream has been closed
     **/
    public void savePosition()
        throws IOException
    {
        synchronized(lock) {
            ensureOpen();
            saveStack.push(new StringBuffer());
        }
    }

    /**
     * Pops position off stack, discarding it.  The current position will
     * be unchanged.  The ability to restore to the discarded position
     * will be lost, but the characters that were read after the position
     * will not be lost.
     *
     * @throws EmptyPositionStackException  if there are no saved positions
     * @throws IOException  if the stream has been closed
     **/
    public void discardPosition()
        throws IOException
    {
        synchronized(lock) {
            ensureOpen();
            try {
                final StringBuffer sb = (StringBuffer) saveStack.pop();

                // append characters stored after the discarded position
                // to the position before
                if (!saveStack.empty()) {
                    final StringBuffer sb2
                        = (StringBuffer) saveStack.peek();

                    sb2.append(sb.toString());
                }
            } catch (EmptyStackException e) {
                throw new EmptyPositionStackException(e);
            }
        }
    }

    /**
     * Pops position off stack, restoring it.  Any characters
     * read hereafter will be those that were read after the push.
     *
     * @throws EmptyPositionStackException  if there are no saved positions
     * @throws IOException  if the stream has been closed
     **/
    public void restorePosition()
        throws IOException
    {
        synchronized(lock) {
            ensureOpen();
            try {
                final StringBuffer sb = (StringBuffer) saveStack.pop();
                sb.append(head, headPos, head.length - headPos);
                head = sb.toString().toCharArray();
                headPos = 0;
            } catch (EmptyStackException e) {
                throw new EmptyPositionStackException(e);
            }
        }
    }

    /**
     * Saves one char to the StringBuffer on the top of the stack.
     *
     * @param ch  the character to save
     **/
    private void saveChar(final char ch) {
        final StringBuffer sb = (StringBuffer) saveStack.peek();
        sb.append(ch);
    }

    /**
     * Saves multiple chars to the StringBuffer on top of the stack.
     *
     * @param      stf   Source buffer
     * @param      off   Offset at which to start reading characters
     * @param      len   Number of characters to read
     *
     * @exception  NullPointerException  If <code>str</code> is
     *   <code>null</code>
     * @exception  IndexOutOfBoundsException  If <code>offset</code> is
     *   negative, or <code>count</code> is negative, or
     *   <code>offset+count</code> is larger than <code>data.length</code>
     **/
    private void saveChars(final char[] str, final int off, final int len) {
        final StringBuffer sb = (StringBuffer) saveStack.peek();
        sb.append(str, off, len);
    }
}
