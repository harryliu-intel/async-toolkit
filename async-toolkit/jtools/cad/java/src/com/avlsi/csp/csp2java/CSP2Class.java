/*
 * Copyright 2001 Asynchronous Digital Design.  
 * Copyright 2002, 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.csp.csp2java;

import com.avlsi.csp.csp2java.CSP2Java;
import com.avlsi.csp.csp2java.SemanticException;
import com.avlsi.cell.CellInterface;
import com.avlsi.util.ext.Exec;
import com.avlsi.util.text.StringUtil;
import com.avlsi.util.debug.Debug;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.FileReader;
import java.io.FilenameFilter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.IOException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Vector;

import com.avlsi.io.StringBuilderWriter;
import java.io.ByteArrayOutputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.ToolProvider;
import static javax.tools.JavaFileObject.Kind.CLASS;
import static javax.tools.JavaFileObject.Kind.SOURCE;

/**
 * Compiles CSP into a java Class object.
 *
 * @author David Hilvert
 * @version $Revision$ $Date$
 **/
public class CSP2Class {

    PrintWriter warningWriter;
    PrintWriter errorWriter;
    PrintWriter debugWriter;
    boolean emitCoverageProbes;
    boolean emitSnoopStatements;
    
    public CSP2Class(PrintWriter warningWriter, PrintWriter errorWriter,
                     PrintWriter debugWriter, boolean emitCoverageProbes) {
        this(warningWriter, errorWriter, debugWriter, emitCoverageProbes,
             false);
    }

    public CSP2Class(PrintWriter warningWriter, PrintWriter errorWriter,
                     PrintWriter debugWriter, boolean emitCoverageProbes,
                     boolean emitSnoopStatements) {
        this.warningWriter = warningWriter;
        this.errorWriter = errorWriter;
        this.debugWriter = debugWriter;
        this.emitCoverageProbes = emitCoverageProbes;
        this.emitSnoopStatements = emitSnoopStatements;
    }

    /* Establish CSP2Class temporary filename conventions.  */

    private static int uniqId = 0;
    private static File commPfx = new File(getTemporaryDirectory());
    private static String javaSfx = ".java";
    private static final int INITIAL_CLASS_SIZE = 16 * 1024;
    private static final int INITIAL_SOURCE_SIZE = 32 * 1024;

    private static String getTemporaryDirectory() {
        final String prop =
            System.getProperty("com.avlsi.csp.csp2java.CSP2Class.temp");
        return prop == null ? "/scratch" : prop;
    }

    /**
     * Class loader used to load classes from a filename according to 
     * CSP2Class temporary filename conventions.  Use 
     *
     *      FileClassLoader.defineClass(className);
     **/

    private static class FileClassLoader extends URLClassLoader {
        private static FileClassLoader singleton = null;

        public static Class defineClass 
            (String className, PrintWriter debugWriter ) 
                throws IOException {

            /* make a singleton for accessing ClassLoader non-static
             * methods */

            try {
                if (singleton == null) 
                    singleton = new FileClassLoader(commPfx);

                debugWriter.println("Defining class: " + className);

                return singleton.loadClass (className);
            } catch (ClassNotFoundException e) {
                throw new AssertionError(e);
            } catch (MalformedURLException e) {
                throw new AssertionError(e);
            }
        }

        private FileClassLoader(File path) throws MalformedURLException {
            // First convert to URI because that escapes any characters
            // that are illegal in an URL
            super(new URL[]{path.toURI().toURL()});
        }
    }

    private static URI toURI(final String n, final JavaFileObject.Kind k) {
        return URI.create("string:///" + n.replace('.', '/') + k.extension);
    }

    private static class ByteStream extends ByteArrayOutputStream {
        public ByteStream() {
            super();
        }
        public ByteStream(int size) {
            super(size);
        }
        public byte[] getBuffer() {
            return buf;
        }
        public int getSize() {
            return count;
        }
    }

    private static class MemoryFileManager extends ForwardingJavaFileManager {
        private final Map<String,ByteStream> classes;

        private class ByteArrayFile extends SimpleJavaFileObject {
            private final String name;
            private ByteArrayOutputStream out;
            public ByteArrayFile(String name) {
                super(toURI(name, CLASS), CLASS);
                this.name = name;
            }

            public OutputStream openOutputStream() throws IOException {
                final ByteStream out = new ByteStream(INITIAL_CLASS_SIZE);
                classes.put(name, out);
                return out;
            }
        }

        public MemoryFileManager(JavaFileManager m) {
            super(m);
            classes = new HashMap<String,ByteStream>();
        }

        public JavaFileObject getJavaFileForOutput(Location location,
                                                   String className,
                                                   JavaFileObject.Kind kind,
                                                   FileObject sibling)
            throws IOException
        {
            return new ByteArrayFile(className);
        }

        public ByteStream getClass(String className) {
            return classes.get(className);
        }
    }
    
    private static class MemoryClassLoader extends ClassLoader {
        private final MemoryFileManager manager;
        public MemoryClassLoader(final MemoryFileManager manager) {
            this.manager = manager;
        }

        protected Class findClass(String name) throws ClassNotFoundException {
            final ByteStream code = manager.getClass(name);
            if (code == null) {
                throw new ClassNotFoundException(name);
            } else {
                return defineClass(name, code.getBuffer(), 0, code.getSize());
            }
        }
    }

    private Class memoryCompile(CellInterface cell)
        throws IOException, SemanticException {
        Class result = null;

        final String className =
            "CSP2ClassTemp_" + (uniqId++) + "_" +
            escapeType(cell.getFullyQualifiedType());

        final StringBuilderWriter srcCode =
            new StringBuilderWriter(INITIAL_SOURCE_SIZE);
        new CSP2Java(warningWriter, errorWriter, debugWriter, 
                     emitCoverageProbes, emitSnoopStatements).
            convert(cell, className, srcCode);

        final DiagnosticCollector<JavaFileObject> diagnostics =
            new DiagnosticCollector<JavaFileObject>();

        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        final StandardJavaFileManager stdManager =
            compiler.getStandardFileManager(diagnostics, null, null);
        final MemoryFileManager fileManager = new MemoryFileManager(stdManager);

        final JavaFileObject inputFile =
            new SimpleJavaFileObject(toURI(className, SOURCE), SOURCE) {
            public CharSequence getCharContent(boolean ignoreEncodingErrors) {
                return srcCode.getBuffer();
            }
        };

        final String classpath = System.getProperty("java.class.path", ".");
        final String[] opts = new String[] { "-g", "-classpath", classpath };

        final JavaCompiler.CompilationTask task =
            compiler.getTask(null, fileManager, diagnostics, Arrays.asList(opts),
                             null, Collections.singletonList(inputFile));

        boolean okay = false;
        Exception except = null;
        try {
            okay = task.call().booleanValue();
        } catch (RuntimeException e) {
            except = e;
        }

        if (!okay) {
            final FileWriter fw = new FileWriter (className + ".err");
            writeErrorHeader(fw);
            for (Diagnostic d : diagnostics.getDiagnostics()) {
                fw.write(" * " + d.getKind().toString() + ": " +
                         d.getMessage(null) + "\n");
            }
            if (except != null) {
                fw.write(" * " + except.getMessage() + "\n");
            }
            fw.write(" *" + "/\n");
            fw.write(srcCode.toString());
            fw.close();
            throw new SemanticException ("Error compiling java source.");
        }

        try {
            return new MemoryClassLoader(fileManager).loadClass(className);
        } catch (ClassNotFoundException e) {
            throw new AssertionError(e);
        }
    }

    public Class compile(CellInterface cell)
        throws IOException, SemanticException {
        final String diskProp =
            ""; // System.getProperty("com.avlsi.csp.csp2java.disk");
        if (diskProp == null)
            return memoryCompile(cell);
        else
            return diskCompile(cell);
    }

    /**
     * Compiles CSP into a java Class object.
     **/
    public Class diskCompile( CellInterface cell ) 
            throws IOException, SemanticException {
        Class result = null;

        // Construct a unique class name

        final File javaFile = File.createTempFile("CSP2ClassTemp_" +
                escapeType(cell.getFullyQualifiedType()),
                javaSfx, commPfx);
        javaFile.deleteOnExit();

        final String className = javaFile.getName().substring(0,
                javaFile.getName().length() - javaSfx.length());

        // Perform conversion to java source text.

        new CSP2Java(warningWriter, errorWriter, debugWriter, 
                     emitCoverageProbes, emitSnoopStatements).
            convert (cell, className, new FileWriter(javaFile));

        // Perform compilation

        Runtime runtime = Runtime.getRuntime();
        String classpath = System.getProperty("java.class.path", ".");

        // Save standard error of the compilation to a temporary file instead
        // of in memory, in case it is very long
        final File javacErrFile = File.createTempFile("CSP2ClassTemp_" +
                escapeType(cell.getFullyQualifiedType()),
                ".javac", commPfx);
        javacErrFile.deleteOnExit();

        final int exitValue;
        final PrintStream oldErr = System.err;
        try {
            final PrintStream err =
                new PrintStream(new FileOutputStream(javacErrFile));
            final String forkProp =
                ""; // System.getProperty("com.avlsi.csp.csp2java.fork");
            if (forkProp == null) {
                System.setErr(err);
                exitValue = com.sun.tools.javac.Main.compile(
                                new String[] { "-g", "-classpath", classpath,
                                               javaFile.getPath() });
            } else {
                // Wait for compilation to complete, and obtain an exit value
                exitValue = Exec.exec ("javac -g -classpath " + classpath + " "
                                       + javaFile.getPath(), null, err);
            }
            err.close();
        } catch (InterruptedException e) {
            throw new AssertionError(e);
        } finally {
            System.setErr(oldErr);
        }

        if (exitValue != 0) {

            // Handle compilation failure

            handleCompilationError (javacErrFile, className, javaFile);

        } else {

            // Read in the main compiled class

            result = FileClassLoader.defineClass (className, debugWriter);

        }

        // Delete files on exit

        runtime.addShutdownHook(new Thread(new Runnable() {
            public void run() {
                final File[] files = commPfx.listFiles(
                    new FilenameFilter() {
                        public boolean accept(File dir, String name) {
                            return name.startsWith(className);
                        }
                    });
                for (int i = 0; i < files.length; i++)
                    files[i].delete();
            }
        }));


        return result;
    }

    private void writeErrorHeader(Writer fw) throws IOException {
        fw.write("/* CSP2Class CSP compiler error file.\n");
        fw.write(" * \n");
        fw.write(" * An error occurred while compiling the java output\n");
        fw.write(" * of CSP2Java.  The text of the compiler error\n");
        fw.write(" * is contained at the end of this comment.  The comment\n");
        fw.write(" * is followed by the text of the program.\n");
        fw.write(" * \n");
    }

    /**
     * Handle an error in compilation.
     **/
    private void handleCompilationError (File javacErrFile, String className, 
            File javaFile) throws SemanticException, IOException {
        // An error occurred in compilation, so produce an error file

        FileWriter fw = new FileWriter (className + ".err");
        BufferedReader br = new BufferedReader(new FileReader(javacErrFile));
        FileReader fr = new FileReader (javaFile);
        int c;

        // Write a header
        writeErrorHeader(fw);

        // Write the error message

        String line;
        while((line = br.readLine()) != null) {
            fw.write(" * " + line + "\n");
        }
        br.close();
        fw.write(" */\n");

        // Write the java file

        while ((c = fr.read()) != -1) {
            fw.write(c);
        }

        // Close the files

        fw.close();
        fr.close();

        // Throw an exception

        throw new SemanticException ("Error compiling java source.");
    }

    private String escapeType(final String typeName) {
        final StringBuffer sb = new StringBuffer(typeName.length());

        for (int i = 0; i < typeName.length(); ++i) {
            final char ch = typeName.charAt(i);

            if (('0' <= ch && ch <= '9') ||
                ('a' <= ch && ch <= 'z') ||
                ('A' <= ch && ch <= 'Z')) {
                sb.append(ch);
            } else if (ch == '_')
                sb.append("__");
            else
                sb.append("_" + ((int) ch) + "_");
        }

        final int maxLength = 100;
        return sb.substring(0, Math.min(sb.length(), maxLength));
    }
}
