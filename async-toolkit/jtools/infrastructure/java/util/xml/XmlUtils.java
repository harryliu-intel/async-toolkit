package com.fulcrummicro.util.xml;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.Reader;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.XMLReader;
import org.xml.sax.InputSource;
import javax.xml.parsers.SAXParserFactory;

import com.avlsi.util.exception.ExceptionUtils;

import com.fulcrummicro.util.properties.BadPropertyException;

/**
 * XmlUtils - a collection of XML helper functions
 *
 * @author Naru Sundar
 */
public class XmlUtils {

    /**
     * parses an XML file referenced by the given stream and handles
     * it using the given handler
     *
     * @param src     the source stream
     * @param handler the handler for it
     */
    public static void parseXml(InputStream src, DefaultHandler handler)
        throws BadPropertyException {

        XMLReader xr;

        try {
            xr = SAXParserFactory.newInstance().newSAXParser().getXMLReader();
            xr.setContentHandler(handler);
            xr.setErrorHandler(handler);
        } catch (Exception e) {
            throw new BadPropertyException(ExceptionUtils.getStackTraceString(e));
        }

        if (src == null)
            throw new BadPropertyException("can't open source stream");
        try {
            Reader r = new BufferedReader(new InputStreamReader(src));
            xr.parse(new InputSource(r));
            r.close();
        } catch (Exception e) {
            throw new BadPropertyException(ExceptionUtils.getStackTraceString(e));
        }
    }
}
