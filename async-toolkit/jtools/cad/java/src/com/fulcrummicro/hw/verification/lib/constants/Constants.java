package com.fulcrummicro.hw.verification.lib.constants;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;

public class Constants {
    protected LinkedHashMap<String, ConstantsCell> cells;
    protected ArrayList<String> importFiles;
    protected ArrayList<Constants> imports;
    protected String module;
    
    /** DIGESTER ONLY */
    @Deprecated
    public Constants() {
        cells = new LinkedHashMap<String, ConstantsCell>();
        importFiles = new ArrayList<String>();
        imports = new ArrayList<Constants>();
    }
    
    public String getModule() {
        return module;
    }

    public void setModule(String module) {
        this.module = module;
    }

    public ConstantsCell getCell(String name) {
        ConstantsCell cell = cells.get(name);
        Iterator<Constants> i = imports.iterator();
        while((cell == null) && i.hasNext()) {
            cell = i.next().getCell(name);
        }
        return cell;
    }
    
    public void addImport(String importFile) {
        importFiles.add(importFile);
    }
    
    public Collection<Constants> getImports() {
        return imports;
    }

    /** DIGESTER ONLY */
    @Deprecated
    public void addCell(ConstantsCell cell) {
        cell.setConstantsTop(this);
        cells.put(cell.getName(), cell);
    }
        
    protected void process(File file) {
        for(String importFile : importFiles) {
            System.err.println("importing "+importFile);
            File f = new File(file.getParentFile(), importFile);
            Constants c = ConstantsDigester.parse(f);
            imports.add(c);
        }
        for (ConstantsCell cell : cells.values()) {
            boolean result = cell.process();
            result = cell.check() && result;
            if(!result) {
                ConstantsDigester.error(String.format("Cell %s did not complete processing successfully.", cell.getName()));
            }
        }
    }
    
    public String toText() {
        String result = "Constants\n";
        for (ConstantsCell cell : cells.values()) {
            result += cell.toText();
        }
        return result;
    }

    public String toCast(String module) {
        String result = "/* vim: nowrap:ts=2:et:sw=2:ro\n" +
                        " * Copyright 2009 Fulcrum Microsystems.  All rights reserved.\n" +
                        " * DO NOT EDIT! Automatically generated from constant xml. " +
                        " */\n\n" +
                        "module " + module + ";\n\n";
        for (ConstantsCell cell : cells.values()) {
            result += cell.toCast();
        }
        return result;
    }
    
    public String toVerilog() {
        String result = "/* vim: nowrap:ts=2:et:sw=2:ro\n" +
                        " * Copyright 2009 Fulcrum Microsystems.  All rights reserved.\n" +
                        " * DO NOT EDIT! Automatically generated from constant xml. " +
                        " */\n\n" ;
        for (ConstantsCell cell : cells.values()) {
            result += cell.toVerilog();
        }
        return result;
    }
}
