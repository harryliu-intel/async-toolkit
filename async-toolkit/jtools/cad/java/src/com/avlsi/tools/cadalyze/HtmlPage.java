/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadalyze;

import java.io.IOException;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.avlsi.util.container.Pair;
import com.avlsi.util.htmlWriter.HtmlWriter;


/**
 * HTML Page Generation Utility Class 
 * (builds on com.avlsi.util.htmlWriter.HtmlWriter)
 *
 * @author Mike Davies
 * @version $Revision$ $Date$
 **/
public final class HtmlPage {

    /***************************** INNER CLASSES *****************************/
    

    /***************************** DATA MEMBERS ******************************/

    private static final String indexPage = "index.html";
    private static final String pageHeaderBgColor = "#dddddd";
    private static final String reportHeaderBgColor = "#eeeedd";
    private static final String reportIndentedRowColor = "#eeeeee";

    /** Low-level HTML tag interface **/
    HtmlWriter writer = null;

    /** Top-level design name **/
    private String designName;

    /** HTML base directory **/
    private String baseDir;

    /** HTML page filename (under baseDir) **/
    private String pageName;

    /** Directory to which all output report files are written **/
    private String outputDir;

    /** Array of boolean values specifying whether to center each columnn **/
    boolean centerReportColumns[] = null;

    /***************************** CLASS METHODS *****************************/

    /** Opens the page and writes the header **/
    public HtmlPage(String designName, String baseDir, String pageName,
                    String pageTitle) 
                                                        throws IOException {
        this.designName = designName;
        this.baseDir = baseDir;
        this.pageName = pageName;
        writer = new HtmlWriter(baseDir + "/" + pageName, null);
        pageHeader(pageTitle);
    }

    /** For a file "path/name.html", returns "path/name" **/
    public String getPagePath() {
        String pagePath = baseDir + "/" + pageName;
        if (pagePath.endsWith(".html"))
            pagePath = pagePath.substring(0,pagePath.length()-5);
        return pagePath;
    }

    private void pageHeader(String title) {
        writer.html(); writer.head(); writer.title();
        writer.println(title);
        writer.titleEnd();
        writer.headEnd(); writer.body();
        writer.table(0,"25%",4,0);
        writer.tr(); 
        writer.print("<TD NOWRAP BGCOLOR=\""+pageHeaderBgColor+"\">");
        writer.bold();
        writer.print("Design: "+designName);
        writer.boldEnd();
        writer.tdEnd(); writer.trEnd();
        writer.tableEnd();
        writer.h1(); writer.println(title); writer.h1End();
    }

    public void close() {
        writer.hr();
        if (!pageName.equals(indexPage)) {
            writer.print("[");
            writer.aHref("index.html");
            writer.print("Main Report Page");
            writer.aEnd();
            writer.println("]");
        }
        else {
            writer.println("Report generated on "+new Date());
        }
        writer.bodyEnd();
        writer.htmlEnd();
        writer.close();
    }

    public void section(String title) {
        writer.h2(); writer.println(title); writer.h2End();
    }

    /**
     * Begins a simple report table.  headColumns is a list of 
     * Pair<String,Boolean> types; first element is the column string,
     * second specifies whether the column text should be centered.
     **/
    public void reportTable(final List headColumns) {
        writer.table(1,"100%",2,2);
        writer.tr();
        centerReportColumns = new boolean[headColumns.size()];
        int col=0;
        for (Iterator hpi=headColumns.iterator(); hpi.hasNext(); col++) {
            writer.print("<TD BGCOLOR=\""+reportHeaderBgColor+"\"");
            Pair colPair = (Pair) hpi.next();
            String text = (String)colPair.getFirst();
            Boolean centered = (Boolean)colPair.getSecond();
            if (centered.equals(Boolean.TRUE)) {
                writer.print(" ALIGN=\"center\"");
                centerReportColumns[col] = true;
            }
            else centerReportColumns[col] = false;
            writer.print(">");
            writer.bold();
            writer.println(text);
            writer.boldEnd();
            writer.tdEnd();
        }
        writer.trEnd();
        indentedTable = false;
    }

    public void reportTableIndented(final List headColumns) {
        reportTable(headColumns);
        indentedTable = true;
    }

    /** Are we in an indented report table? **/
    private boolean indentedTable;

    public void reportTableEnd() { writer.tableEnd(); }

    /** Prints a line of a report table. **/
    public void reportTableLine(final List columns) {
        assert columns.size() == (centerReportColumns.length + 
                                            (indentedTable ? 1 : 0));
        Iterator ci = columns.iterator();
        if (indentedTable) {
            if (((Boolean) ci.next()).equals(Boolean.TRUE))
                writer.print("<TR BGCOLOR=\""+reportIndentedRowColor+"\">");
            else
                writer.tr();
        }
        else {
            writer.tr();
        }
        for (int col=0; ci.hasNext() && col<centerReportColumns.length; col++) {
            if (centerReportColumns[col]) writer.tdAlign("center");
            else writer.td();
            writer.print((String)ci.next());
            writer.tdEnd();
        }
        writer.trEnd();
    }

    public void summaryTable() { writer.table(1,"100%",2,2); }
    public void summaryTableEnd() { writer.tableEnd(); }

    public void summaryTableLine(String desc, String val) {
        writer.tr(); writer.td();
        writer.println(desc);
        writer.tdEnd(); writer.td();
        writer.println(val);
        writer.tdEnd(); writer.trEnd();
    }

    public void summaryTableLine(String desc, int val) {
        summaryTableLine(desc,String.valueOf(val));
    }

    /** Produces a simple ordered/unordered list of URL links **/
    public void listOfLinks(final List linkList, boolean ordered) {
        if (ordered) writer.ol();
        else writer.ul();
        for (Iterator li=linkList.iterator(); li.hasNext();) {
            Pair pr = (Pair)li.next();
            writer.li();
            writer.aHref((String)pr.getFirst());
            writer.print((String)pr.getSecond());
            writer.aEnd();
        }
        if (ordered) writer.olEnd();
        else writer.ulEnd();
    }

    /***************************** STATIC METHODS *****************************/

}
