package com.avlsi.tools.difflayout;

import org.xml.sax.Attributes;
import java.io.BufferedReader;
import java.util.Collection;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.InputSource;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Iterator;
import java.util.LinkedList;
import java.io.Reader;
import java.awt.Rectangle;
import javax.xml.parsers.SAXParserFactory;
import java.util.TreeSet;
import org.xml.sax.XMLReader;

public class GDSIIFile extends DefaultHandler {
    /**
     * A little utility function which feeds the given stream to this
     * handler.
     */
    public void parseXml(InputStream istr)
        throws Exception {
        XMLReader xr;
        xr = SAXParserFactory.newInstance().newSAXParser().getXMLReader();
        xr.setContentHandler(this);
        xr.setErrorHandler(this);

        Reader r = new BufferedReader(new InputStreamReader(istr));
        xr.parse(new InputSource(r));
        r.close();
    }

    /** Polygon currently being parsed. */
    private LayeredPolygon poly;

    /** Polygons which have been parsed. */
    private LinkedList polygons = new LinkedList();

    /** bounding box */
    int minx = Integer.MAX_VALUE, miny = Integer.MAX_VALUE;
    int maxx = Integer.MIN_VALUE, maxy = Integer.MIN_VALUE;

    /** Layers which have been seen */
    private TreeSet layers = new TreeSet();

    public void startElement (String uri, String name,
                              String qName, Attributes atts) {
        if (qName.equals("boundary")) {
            int layer = Integer.parseInt(atts.getValue("layer"));
            poly = new LayeredPolygon(layer);
            layers.add(new Integer(layer));
        } else if (qName.equals("xy") && poly != null) {
            int x = Integer.parseInt(atts.getValue("x"));
            int y = Integer.parseInt(atts.getValue("y"));
            if (x < minx)
                minx = x;
            if (x > maxx)
                maxx = x;
            if (y < miny)
                miny = y;
            if (y > maxy)
                maxy = y;
            poly.addPoint(x,y);
        }
    }
                
    public void endElement (String uri, String name,
                            String qName) {
        if (qName.equals("boundary")) {
            polygons.add(poly);
            poly = null;
        }
    }

    public Collection getPolygonsForLayer(int layer) {
        Collection c = new LinkedList();
        for (Iterator it = polygons.iterator(); it.hasNext(); ) {
            LayeredPolygon p = (LayeredPolygon) it.next();
            if (p.layer == layer)
                c.add(p);
        }

        return c;
    }

    public Collection getLayers() {
        return layers;
    }

    public Rectangle getBoundingBox() {
        return new Rectangle(minx, miny, maxx-minx, maxy-miny);
    }
}
