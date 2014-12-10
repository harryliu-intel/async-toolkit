/*
 * Copyright 2002 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

/*
 * Copyright 2001 Asynchronous Digital Design.  All rights reserved.
 * $Id$
 */

package com.avlsi.file.aspice;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.io.File;
import java.io.FileReader;
import java.io.PrintStream;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Class representing the cast file test environment for the 
 * C version of aspice.
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **/
public final class CastTestEnvironment {

    /** List of imports (Strings) **/
    private final HashSet importSection = new HashSet();

    /** Node/channel declarations **/
    private final ArrayList declarationSection = new ArrayList();

    /** Instantiations **/
    private final ArrayList instantiationSection = new ArrayList();

    /** asp{} section **/
    private final ArrayList aspSection = new ArrayList();

    /** Copy constructor **/
    public CastTestEnvironment(final CastTestEnvironment src) {
        importSection.addAll(src.importSection);
        declarationSection.addAll(src.declarationSection);
        instantiationSection.addAll(src.instantiationSection);
        aspSection.addAll(src.aspSection);
    }

    /** Constructor from initialization file **/
    public CastTestEnvironment(final String initFile) {
        File f = new File(initFile);
        if (!f.canRead()) return;
        try {
            FileReader fr = new FileReader(f);
            int ch, lineNum = 1;
            String line = "";
            while ((ch = fr.read()) != -1) {
                if (ch != '\n') {
                    line += (char)ch;
                    lineNum++;
                }
                else {
                    //
                    // Complete line.  Parse into
                    // "SECTION: str" fields.
                    //
                    int i = line.indexOf(':');
                    if (i == -1) {
                        // Default to instantiation section
                        // (for no particular reason)
                        instantiationSection.add(line);
                    }
                    else {
                        String section = line.substring(0,i);
                        String x = line.substring(i+1);
                        if (section.equals("IMPORT"))
                            importSection.add(x);
                        else if (section.equals("DECLARATION"))
                            declarationSection.add(x);
                        else if (section.equals("INSTANTIATION"))
                            instantiationSection.add(x);
                        else if (section.equals("ASP"))
                            aspSection.add(x);
                        else {
                            System.err.println("CastTestEnvironment: Ignoring "+
                                               "unknown section '"+x+"' at line "+
                                               lineNum+" in "+initFile+".");
                        }
                    }
                    line = "";
                }
            }
            fr.close();
        }
        catch (FileNotFoundException e) {
            System.err.println("Couldn't find file "+initFile+".");
            return;
        }
        catch (IOException e) {
            System.err.println("Error reading "+initFile+":");
            System.err.println(e+": "+e.getMessage());
        }
    }

    /** Add a line to the import section. **/
    public void addImport(final String line) {
        importSection.add(line);
    }
    
    /** Add a line to the type declaration section. **/
    public void addDeclaration(final String line) {
        declarationSection.add(line);
    }
    
    /** Add a line to the instantiation section. **/
    public void addInstantiation(final String line) {
        instantiationSection.add(line);
    }

    /** Add a line to the asp section. **/
    public void addAspLine(final String line) {
        aspSection.add(line);
    }

    /** Print the entire cast test environment file to the PrintStream. **/
    public void printToStream(PrintStream os) {
        Iterator it = importSection.iterator();
        while (it.hasNext()) 
            os.println("import \""+(String)it.next()+"\";");
        int i;
        for (i=0; i < declarationSection.size(); i++)
            os.println(declarationSection.get(i));
        for (i=0; i < instantiationSection.size(); i++)
            os.println(instantiationSection.get(i));
        os.println("asp {");
        for (i=0; i < aspSection.size(); i++)
            os.println(aspSection.get(i));
        os.println("}");
    }
}
