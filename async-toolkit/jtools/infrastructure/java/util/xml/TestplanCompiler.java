package com.fulcrummicro.util.xml;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

import com.sun.org.apache.xerces.internal.dom.ChildNode;

public class TestplanCompiler {

    static String root = "";
    static Document destDocument;
    
    public TestplanCompiler() {
    }
    
    private static Element childElement(Node parent, String childName, int childNum) {
        int num = 0;
        Node current = parent.getFirstChild();
        while(current != null) {  
            if((current.getNodeType() == Node.ELEMENT_NODE) && (current.getNodeName() == childName)) {
                if (childNum == num++) {
                    return (Element)current;
                }
            }
            current = current.getNextSibling();
        }
        
        //  not found
        return null;
    }   

    private static Element childElement(Node parent, String childName) {
        return childElement(parent, childName, 0);
    }

    private static Element childElementWithAttribute(Node parent, String childName, String attribute, String value) {
        Element child;
        for(int i=0; (child = childElement(parent, childName, i)) != null; i++) {
            if(child.getAttribute(attribute).equals(value)) {
                return child;
            }
        }
        return null;
    }
    
    private static Document loadDocument(String filename) {
        Document document;
        DocumentBuilderFactory factory =
            DocumentBuilderFactory.newInstance();
        //System.out.println("Factory: " + factory.getClass().getName());
        factory.setValidating(false);   
        factory.setNamespaceAware(true);
        factory.setXIncludeAware(true);
        File file = new File(filename);
        if(!file.exists()) {
            System.err.println("Error: file does not exist: " + filename);
            return null;
        }
        try {
           DocumentBuilder builder = factory.newDocumentBuilder();
           //System.out.println("Builder: " + builder.getClass().getName());
           document = builder.parse(file);   
           //System.out.println("Document: " + document.getClass().getName());  
        } catch (SAXException sxe) {
           // Error generated during parsing)
           Exception  x = sxe;
           if (sxe.getException() != null)
               x = sxe.getException();
           x.printStackTrace();
           return null;
        } catch (ParserConfigurationException pce) {
            // Parser with specified options can't be built
            pce.printStackTrace();
            return null;
        } catch (IOException ioe) {
           // I/O error
           ioe.printStackTrace();
           return null;
        }            
        return document;
    }
    
    private static void printDocument(Document d, String xsl, String filename) {
        PrintWriter pw = null;

        try {
            pw = new PrintWriter(
                    new BufferedWriter(new FileWriter(filename)));
        } catch(IOException e) {
            System.out.println("Error opening " + filename + ": " + e);
            return;
        }

        
        try {           
            TransformerFactory transformerFactory = TransformerFactory.newInstance();
            transformerFactory.setAttribute("indent-number", new Integer(4));
            Transformer transformer;
            if(xsl.equals("-")) {
                transformer = transformerFactory.newTransformer();
                transformer.setOutputProperty(OutputKeys.METHOD, "xml");
                transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/xml");
            } else {
                transformer = transformerFactory.newTransformer(new StreamSource(new File(xsl)));
                transformer.setOutputProperty(OutputKeys.METHOD, "html");
                transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, "text/html");
            }
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.ENCODING, "ISO-8859-1");
            
            transformer.transform(new DOMSource(d), new StreamResult(pw));
            //transformer.transform(new DOMSource(d), new StreamResult(new OutputStreamWriter(System.out)));
        } catch (Exception e) {
            System.out.println("Error outputting XML document: " + e);
            return;            
        }
        pw.close();
    }
    
    private static void processTests(Element tests) {
        Node child;
        processFileAttributeAppend(tests, "tests");
        for(int i=0; (child = tests.getChildNodes().item(i)) != null; i++) {
            if(child.getNodeType() != Node.ELEMENT_NODE) {
                continue;
            }
            Element childElement = (Element)child;
            if(childElement.getTagName().equals("test")) {
                processTest(childElement);
            } else if(childElement.getTagName().equals("tests")) {
                processTests(childElement);
            } else {
                System.out.println(childElement.getTagName());
            }
        }
    }

    private static Node processTest(Element test) {
        System.out.println("test: " + test.getAttribute("file"));
        
        processFileAttributeAppend(test, "test");
        
        return test;
    }
    
    private static void processFileAttributeAppend(Element test, String topNodeName) { 
        String file = test.getAttribute("file");
        if(file.equals("")) {
            return;
        }
        Document document = loadDocument(root + file);
        if(document == null) {
            return;
        }
        Node docTest = childElement(document, topNodeName);
        if(docTest == null) {
            docTest = childElement(document.getLastChild(), topNodeName);
            if(docTest == null) {
                Node testplan = childElement(document, "testplan");
                if(testplan != null) docTest = childElement(testplan, topNodeName);
            }
            if(docTest == null) {
                System.out.println("ERROR: Could not find " + topNodeName + " in " + file);
                return;
            }
        }
        Node replacement = destDocument.importNode(docTest,true);
        
        while(replacement.getFirstChild() != null) {
            test.appendChild(replacement.removeChild(replacement.getFirstChild()));
        }
        
        for(int i = 0; i < replacement.getAttributes().getLength(); i++) {
            Attr attr = (Attr)replacement.getAttributes().item(i);
            if(!test.hasAttribute(attr.getName())) { // allow testplan to override
                test.setAttribute(attr.getName(), attr.getValue());
            }
        }        
    }
    
    private static void processFileAttributeReplace(Node parent, String nodeName, String topNodeName) { 
        Node child;
        for(int i=0; (child = parent.getChildNodes().item(i)) != null; i++) {
            if(child.getNodeType() != Node.ELEMENT_NODE) {
                continue;
            }
            Element childElement = (Element)child;
            if(childElement.getTagName().equals(nodeName)) {
                System.out.println("processFileAttribute found " + nodeName);
                String file = childElement.getAttribute("file");
                if(file.equals("")) {
                    return;
                }
                Document document = loadDocument(root + file);
                if(document == null) {
                    return;
                }
                Node replacement = childElement(document, topNodeName);
                if(replacement == null) {
                    System.out.println("processFileAttribute could not find " + topNodeName +
                        " in " + file);
                    return;
                }
                replacement = destDocument.importNode(replacement, true);
                parent.replaceChild(replacement, child);
            }
        }
    }
    
    /**
     * @param args
     */
    public static void main(String[] args) {
        assert args.length >= 3: "Usage: TestplanCompiler source xsl dest";
        Document document = loadDocument(args[0]);
        destDocument = document;
        
        Element testplan = childElement(document, "testplan");
        assert testplan != null : "No testplan element in source";
        //System.out.println(testplan.getTextContent());
        //System.out.println(document.getLastChild().getTextContent());
        
        processFileAttributeReplace(testplan, "features", "feature-list");
        processTests(testplan);  
        
        printDocument(document, args[1], args[2]);
    }

}
