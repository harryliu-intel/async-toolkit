package com.avlsi.cast2.util;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.InputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.OutputStream;
import java.security.Permission;
import java.util.StringTokenizer;

import com.avlsi.cast.CastCacheManager;
import com.avlsi.cast.CastFileParser;
import com.avlsi.cast2.util.StandardParsingOption;
import com.avlsi.io.FileSearchPath;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;

public class CastFileServer {
    private boolean exitOkay;

    private static class NoCloseOutputStream extends FilterOutputStream {
        public NoCloseOutputStream(OutputStream os) {
            super(os);
        }
        public void close() throws IOException {
            flush();
        }
    }

    private void installSM() {
        final SecurityManager sm = new SecurityManager() {
            public void checkExit(int status) {
                if (status != 0 && !exitOkay) {
                    super.checkExit(status);
                    throw new SecurityException(Integer.toString(status));
                }
            }
            public void checkPermission(Permission perm) { }
        };
        System.setSecurityManager(sm);
    }

    public CastFileServer() {
        exitOkay = false;
        installSM();
    }

    private void processLine(final String line) throws Exception {
        final StringTokenizer tokenizer = new StringTokenizer(line);
        if (tokenizer.countTokens() < 4) {
            throw new IllegalArgumentException("Invalid line: " + line);
        }

        // Where to redirect stdout, and stderr of the program
        final String output = tokenizer.nextToken();
        final String error = tokenizer.nextToken();

        // Use reflection to get main(String[])
        final String className = tokenizer.nextToken();
        final Class app = Class.forName(className);
        final Method mainRoutine =
            app.getMethod("main", new Class[] {
                Class.forName("[Ljava.lang.String;")
            });

        // Setup arguments to the program
        final String realargs[] = new String[tokenizer.countTokens()];
        int i = 0;
        while (tokenizer.hasMoreTokens()) {
            realargs[i++] = tokenizer.nextToken();
        }

        // Redirect stdout
        final OutputStream fout;
        if (output.equals("-")) {
            fout = new NoCloseOutputStream(System.out);
        } else {
            fout = new FileOutputStream(output);
        }
        System.setOut(new PrintStream(fout));

        // Redirect stderr
        final OutputStream ferr;
        if (error.equals("-")) {
            ferr = new NoCloseOutputStream(System.err);
        } else {
            ferr = new FileOutputStream(error);
        }
        System.setErr(new PrintStream(ferr));

        // Invoke program
        mainRoutine.invoke(null, new Object[] { realargs });

        // Flush the streams
        System.out.flush();
        System.err.flush();

        // Close any open file handles
        System.out.close();
        System.err.close();
    }

    private static void status(final String x) {
        System.err.println("CastFileServer: " + x);
    }

    public static void main(String[] args) throws Exception {
        final CommandLineArgs parsedArgs = new CommandLineArgsDefImpl( args );
        final CommandLineArgs argsWithConfigs =
            new CommandLineArgsWithConfigFiles( parsedArgs );

        final CommandLineArgs cachedArgs =
            new CachingCommandLineArgs( argsWithConfigs );

        final CommandLineArgs theArgs = cachedArgs;
        final String castRoot = theArgs.getArgValue("cast-path", ".");
        final String castVersion = theArgs.getArgValue("cast-version", "2");

        final CastFileParser castParser =
            CastCacheManager.getDefault().getCastFileParser(
                    new FileSearchPath(castRoot), castVersion,
                    new StandardParsingOption(theArgs));
        CastCacheManager.getDefault().setCastFileParser(castParser);

        final BufferedReader br =
            new BufferedReader(new InputStreamReader(System.in));

        final CastFileServer server = new CastFileServer();
        String line;
        final PrintStream out = System.out;
        final PrintStream err = System.err;
        while ((line = br.readLine()) != null) {
            server.exitOkay = false;
            String exited = null;
            Exception error = null;
            AssertionError assertion = null;
            try {
                server.processLine(line);
            } catch (InvocationTargetException e) {
                final Throwable cause = e.getCause();
                if (cause instanceof SecurityException) {
                    exited = cause.getMessage();
                } else if (cause instanceof Exception) {
                    error = (Exception) cause;
                } else if (cause instanceof AssertionError) {
                    assertion = (AssertionError) cause;
                } else {
                    throw new Error(cause);
                }
            } catch (Exception e) {
                error = e;
            }
            System.setOut(out);
            System.setErr(err);
            if (exited != null) {
                status("EXIT " + exited);
            } else if (error != null) {
                status("EXCEPTION");
                error.printStackTrace();
            } else if (assertion != null) {
                status("EXCEPTION");
                assertion.printStackTrace();
            } else {
                status("SUCCESS");
            }
            status("READY");
            System.out.flush();
            System.err.flush();
        }
        server.exitOkay = true;
    }   
}
