/*
 * Copyright 2004 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.tools.cadalyze;

import java.io.IOException;
import java.io.File;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.StringTokenizer;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;

import com.avlsi.cell.CellInterface;
import com.avlsi.fast.ports.PortDefinition;
import com.avlsi.file.common.DeviceTypes;
import com.avlsi.file.common.HierName;
import com.avlsi.file.common.InvalidHierNameException;
import com.avlsi.util.container.Pair;

/**
 * Maintains a binned distribution of double values (Double objects)
 * @author Mike Davies
 * @version $Revision$ $Date$
 **/
public final class DoubleDistribution {

    /****************************** DATA MEMBERS ******************************/

    private double min;

    private double max;

    private int numBins;

    private double binSize;

    private int[] count;

    private double[] total;

    /******************************** METHODS ********************************/

    DoubleDistribution(double min, double max, int numBins) {
        this.min = min;
        this.max = max;
        this.numBins = numBins;
        binSize = (max - min) / numBins;
        count = new int[numBins+2];
        total = new double[numBins+2];
        for (int i=0; i<numBins+2; i++) {
            count[i] = 0;
            total[i] = 0.0;
        }
    }

    DoubleDistribution(DoubleDistribution dist) {
        this.min = dist.min;
        this.max = dist.max;
        this.numBins = dist.numBins;
        binSize = dist.binSize;
        count = new int[numBins+2];
        total = new double[numBins+2];
        for (int i=0; i<numBins+2; i++) {
            count[i] = dist.count[i];
            total[i] = dist.total[i];
        }
    }

    void addValue(double value) {
        double binVal = ((value - min) / binSize);
        int bin;
        if (binVal < 0.0) bin = 0;
        else if (binVal > numBins) bin = numBins+1;
        else bin = (int)binVal + 1;
        count[bin]++;
        total[bin] += value;
    }

    void addDistribution(int cnt, final DoubleDistribution dist) {
        assert min == dist.min && max == dist.max && numBins == dist.numBins;
        for (int i=0; i<numBins+2; i++) {
            count[i] += cnt * dist.count[i];
            total[i] += cnt * dist.total[i];
        }
    }

    /** Returns the total number of value data points in the distribution **/
    int getTotalValues() {
        int t = 0;
        for (int i=0; i<numBins+2; i++) t += count[i];
        return t;
    }

    double getAverage() {
        double totalVal = 0.0;
        int totalCount = 0;
        for (int i=0; i<numBins+2; i++) {
            totalVal += total[i];
            totalCount += count[i];
        }
        return totalVal / totalCount;
    }

    void printHtml(HtmlPage page, double scale) {
        DecimalFormat formFloat = (DecimalFormat)DecimalFormat.getInstance();
        formFloat.applyPattern("#0.00");
        page.writer.table(1,2,2);
        page.writer.tr();
        page.writer.td();
        page.writer.println("<"+formFloat.format(scale*min));
        page.writer.tdEnd();
        for (int i=0; i<numBins; i++) {
            page.writer.td();
            page.writer.println(formFloat.format(scale*(min+i*binSize))+".."+
                                formFloat.format(scale*(min+(i+1)*binSize)));
            page.writer.tdEnd();
        }
        page.writer.td();
        page.writer.println(">"+formFloat.format(scale*max));
        page.writer.tdEnd();
        page.writer.trEnd();
        page.writer.tr();
        for (int i=0; i<numBins+2; i++) {
            page.writer.td();
            page.writer.println(count[i]);
            page.writer.tdEnd();
        }
        page.writer.trEnd();
        page.writer.tableEnd();
    }

    /** baseFileName should not include a suffix (".png" will be added) **/
    void plotData(String baseFileName, double scale) {
        /** Setup **/
        int w = 800;
        int h = 100;
        int numYLevels = 4;
        int fontSize = 10;
        Color bgColor = new Color(0xffffff);
        Color scaleColor = new Color(0x222222);
        Color ylevelColor = new Color(0xeeeeee);
        Color barColor = new Color(0xee6666);
        Color annotateColor = new Color(0xbbbb99);
        BufferedImage img = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);
        Graphics2D g = img.createGraphics();

        /** Draw the distribution **/
        g.setColor(bgColor);
        g.fillRect(0,0,w,h);
        g.setColor(scaleColor);
        int b = h-(fontSize+5);
        int x = 1;
        int dx = (w-2)/(numBins+2);
        g.drawRect(x,1,(numBins+2)*dx,b);
        for (int i=0; i<numBins+3; i++) {
            g.drawLine(x,b,x,b+3);
            x += dx;
        }
        // X-axis scale
        g.setFont(new Font("Sans-serif", Font.BOLD, 10));
        DecimalFormat formFloat = (DecimalFormat)DecimalFormat.getInstance();
        formFloat.applyPattern("#0");
        x = 1+dx-fontSize/2; 
        for (int i=0; i<numBins+1; i++) {
            g.drawString(formFloat.format(scale*(min+i*binSize)),
                         x,b+4+fontSize);
            x += dx;
        }
        x = 2;
        int hh = b-1;
        // Y-axis level lines
        g.setColor(ylevelColor);
        int y = 1+hh/numYLevels;
        for (int i=0; i<3; i++) {
            g.drawLine(2,y,(numBins+2)*dx-1,y);
            y += hh/numYLevels;
        }
        // Histogram bars
        formFloat.applyPattern("#0.0");
        g.setFont(new Font("Sans-serif", Font.PLAIN, 10));
        int t = getTotalValues(); 
        t = t > 0 ? t : 1;
        g.setColor(barColor);
        for (int i=0; i<numBins+2; i++) {
            int ph = hh*count[i]/t;
            g.fillRect(x,1+b-ph,dx-1,ph);
            if ((i==0 || i==numBins+1) && count[i]>0) {
                g.setColor(annotateColor);
                g.drawString("("+formFloat.format(scale*(total[i]/count[i]))+
                             ")", x + 4, 1+b-ph-4);
                g.setColor(barColor);
            }
            x += dx;
        }
        // Vertical average value marker
        g.setColor(annotateColor);
        x = 1 + dx + (int) (numBins*dx * (getAverage()-min)/(max-min));
        if (x < 2) x = 2;
        if (x > 1+dx+numBins*dx) x = (numBins+2)*dx;
        g.drawLine(x,2,x,1+b);

        /** Write the image **/
        Iterator writers = ImageIO.getImageWritersBySuffix("png");
        try {
            ImageWriter iw = (ImageWriter) writers.next();
            ImageOutputStream ios = 
                ImageIO.createImageOutputStream(new File(baseFileName+".png"));
            iw.setOutput(ios);
            iw.write(img);
            iw.dispose();
            ios.close();
            img.flush();
        }
        catch (Exception e) {
            System.err.println("Could not write image "+baseFileName+".png:");
            System.err.println(e);
        }
    }
}
