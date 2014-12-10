/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 *
 * $Id$
 */

package com.avlsi.util.ext;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import com.avlsi.util.text.StringUtil;

/**
 * Convenience methods to run a sub-process and redirect its standard
 * output and standard error to the provided streams.
 *
 * @author Frederik Eaton
 * @version $Revision$ $Date$
 **/
public class Exec {
    /**
     * This class should not be instantiated.
     **/
    private Exec() { }

    /**
     * Starts a sub-process, and optionally copies its standard out
     * and standard error to the provided streams. 
     *
     * <p>This should almost always be used instead of Runtime.exec,
     * because <code>Runtime.exec</code> requires that the subprocess
     * output is read "promptly", and this method ensures that such reading
     * is done.
     * 
     * <p>The first three arguments are the same as for 
     * {@link Runtime#exec(String[], String[], File)}
     * (in particular, <code>envp</code> and <code>dir</code> can be
     * <code>null</code>).  The last two are the streams. Either stream
     * argument can be <code>null</code>, signifying that the data from the
     * corresponding subprocess file descriptor should be discarded.
     *
     * @throws NullPointerException  if cmdArray is null.
     * @throws IOException  if an I/O error occurs
     * @throws IndexOutOfBoundsException  if cmdarray has length 0.
     * @throws InterruptedException  If another thread interrupts the
     *   current thread
     * @throws SecurityException  if a security manager exists and its
     *   <code>checkExec</code> method doesn't allow creation of a subprocess.
     *
     * @return The exit status of the process.
     *
     * <pre><jml>
     *   public normal_behavior
     *     requires cmdArray != null && cmdArray.length > 0;
     *   also
     *   public exceptional_behavior
     *     requires cmdArray == null;
     *     signals (NullPointerException);
     *   also
     *   public exceptional_behavior
     *     requires cmdArray != null && cmdArray.length == 0;
     *     signals (IndexOutOfBoundsException);
     * </jml></pre>
     *
     * @see Runtime#exec(String[], String[], File)
     **/
    public static int exec(String[] cmdArray,
                           String[] envp,
                           File dir,
                           InputStream inSrc,
                           OutputStream outDest,
                           OutputStream errDest) 
            throws IOException, InterruptedException {
        Runtime rt = Runtime.getRuntime();
        Process p = rt.exec(cmdArray, envp, dir);
        InputStream si = p.getInputStream();
        InputStream se = p.getErrorStream();
        ArrayList l=new ArrayList();
        l.add(copy(si,outDest));
        l.add(copy(se,errDest));
        if (inSrc != null) {
            OutputStream so = p.getOutputStream();
            l.add(copy(inSrc,so,true));
        }
        for(int i=0; i<l.size(); i++) 
            ((Thread)l.get(i)).join();
        return p.waitFor();
    }

    public static int exec(String[] cmdArray,
                           String[] envp,
                           File dir,
                           OutputStream outDest,
                           OutputStream errDest) 
            throws IOException, InterruptedException {
        return exec(cmdArray, envp, dir, null, outDest, errDest);
    }

    /**
     * Starts a sub-process, and optionally copies its standard out
     * and standard error to the provided streams. 
     *
     * <p>This method is a convenience method.  The <code>cmd</code> string
     * is broken into tokens and a new array <code>cmdArray</code> containing
     * the tokens in the order that they were produced by the string
     * tokenizer; it then calls <code>exec(cmdArray, null, null,
     * outDest, errDest)</code>. The token parsing is done by calling
     * {@link StringUtil#tokenize}.
     *
     * <p> In the tradition of <code>Runtime.exec(String)</code>,
     * the lexing doesn't honor quotation marks or single quotes. If you
     * do <code>Exec.exec("bash -c 'echo hi'")</code>, then you'll be
     * confused.  If you do <code>Exec.exec("rm -rf \""+file+"\"")</code>,
     * and file happens to be ". foo", then you'll also be confused. In
     * general, you'll be better off with
     * {@link #exec(String[], String[], OutputStream, OutputStream}.
     *
     * <pre><jml>
     *   public normal_behavior
     *     requires cmd != null && cmd.length() > 0;
     *   also
     *   public exceptional_behavior
     *     requires cmd == null;
     *     signals (NullPointerException);
     *   also
     *   public exceptional_behavior
     *     requires cmd != null && cmd.length() == 0;
     *     signals (IndexOutOfBoundsException);
     * </jml></pre>
     *
     * @throws NullPointerException  if cmd is null
     * @throws IOException  if an I/O error occurs
     * @throws IndexOutOfBoundsException  if cmd is the empty string
     * @throws InterruptedException  If another thread interrupts the
     *   current thread
     * @throws SecurityException  if a security manager exists and its
     *   <code>checkExec</code> method doesn't allow creation of a subprocess.
     * 
     * @return The exit status of the process.
     * 
     * @see Runtime#exec(String, String[], File),
     *      #exec(String[], String[], File, OutputStream, OutputStream),
     *      StringUtil#tokenize
     **/
    public static int exec(String cmd, 
                           InputStream inSrc,
                           OutputStream outDest, 
                           OutputStream errDest)
            throws IOException, InterruptedException {
        final String[] cmdArray = StringUtil.tokenize(cmd);
        return exec(cmdArray,null,null,inSrc,outDest,errDest);
    }

    public static int exec(String cmd, 
                           OutputStream outDest, 
                           OutputStream errDest)
            throws IOException, InterruptedException {
        return exec(cmd, null, outDest, errDest);
    }

    /** 
     * Starts a new thread which copies data from one stream to
     * another, and returns the thread.
     **/
    private static Thread copy(final InputStream in, final OutputStream out,
                               final boolean close) {
        Thread t = new Thread(new Runnable() {
                public void run() {
                    byte[] b=new byte[1024];
                    int l;
                    try {
                        while(true) {
                            l=in.read(b,0,b.length);
                            if(l<=0) break;
                            if(out!=null) {
                                out.write(b,0,l);
                                out.flush();
                            }
                        }
                        if (close) out.close();
                    } catch(IOException e) {
                        // Jesse says we should do nothing, rather
                        // than printing on stderr
                    }
                }
                // Feel free to improve on this thread name if you
                // need more information for debugging
            }, "subprocess output copier");
        t.start();
        return t;
    }
    private static Thread copy(final InputStream in, final OutputStream out) {
        return copy(in, out, false);
    }
}
