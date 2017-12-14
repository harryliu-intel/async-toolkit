package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.fulcrummicro.hw.verification.lib.constants.ConstantsEnum;
import com.fulcrummicro.hw.verification.lib.constants.ConstantsEnumValue;
import com.fulcrummicro.hw.verification.lib.testcase.ResetType.ResetDomain;
import com.fulcrummicro.util.misc.Utility;

public class RegisterXmlParser {
    private final RegisterInfo regInfo;
    private final boolean excludeOldName;
    private boolean failed = false;

    public RegisterXmlParser(RegisterInfo regInfo, boolean excludeOldName) {
        this.regInfo = regInfo;
        this.excludeOldName = excludeOldName;
    }

    private static String escape(String in) {
        return in.replaceAll("[\\[\\]{}.:,]", "_").replaceFirst("^_","");
    }

    private Node childNode(Node parent, String childName) {
        return childNode(parent, childName, 0);
    }

    // childnum is 0 indexed
    private Node childNode(Node parent, String childName, int childNum) {
        int num = 0;
        Node current = parent.getFirstChild();
        while(current != null) {
            if(current.getNodeName() == childName) {
                if (childNum == num++) {
                    return current;
                }
            }
            current = current.getNextSibling();
        }

        //  not found
        return null;
    }

    private Node childNodeDimension(Node parent, String childName, int dimension) {
        String dimensionAttribute;
        Node current = parent.getFirstChild();
        while(current != null) {
            if(current.getNodeName() == childName) {
                dimensionAttribute = ((Element)current).getAttribute("dimension");
                if (((dimensionAttribute == "") && (dimension == 0)) ||
                    ((dimensionAttribute != "") && (dimension == Integer.parseInt(dimensionAttribute)))) {
                    return current;
                }
            }
            current = current.getNextSibling();
        }

        //  not found
        return null;
    }

    private Node fieldInsertionPoint(Node fields, int newPosition) {
        Node current = fields.getFirstChild();
        while(current != null) {
            Node nameNode;
            for(int i = 0; (nameNode = childNode(current, "position", i)) != null; i++) {
                if(Integer.parseInt(nameNode.getTextContent().trim()) > newPosition) {
                    return current;
                }
            }
            current = current.getNextSibling();
        }

        //  not found
        return null;
    }

    // TODO: create AccessMethods class like ResetDomains
    Vector<String> accessMethods(Node accessMethodsNode) {
        Vector<String> accessMethods = null;
        if(accessMethodsNode != null) {
            accessMethods = new Vector<String>();
            int numberAccessMethods = numberChildNodes(accessMethodsNode, "accessMethod");
            for(int i=0; i<numberAccessMethods; i++) {
                accessMethods.add(childNode(accessMethodsNode, "accessMethod", i).getTextContent());
            }
        }
        return accessMethods;
    }

    // Needed to covert negative numbers back to int
    // The java Integer.parseInt requires a - for negative numbers
    // and will throw an exception for numbers like ffffffff (-1)
    private int hexToInt(String hex) throws NumberFormatException {
        int len = hex.length();
        if (len > 8)
            throw new NumberFormatException();

        int l = 0;
        for (int i = 0; i < len; i++) {
            l <<= 4;
            int c = Character.digit(hex.charAt(i), 16);
            if (c < 0)
                throw new NumberFormatException();
            l |= c;
        }
        return l;
    }

    // Needed to covert negative numbers back to int
    // The java Integer.parseInt requires a - for negative numbers
    // and will throw an exception for numbers like ffffffff (-1)
    private long hexToLong(String hex) throws NumberFormatException {
        int len = hex.length();
        if (len > 8)
            throw new NumberFormatException();

        long l = 0;
        for (int i = 0; i < len; i++) {
            l <<= 4;
            long c = Character.digit(hex.charAt(i), 16);
            if (c < 0)
                throw new NumberFormatException();
            l |= c;
        }
        return l;
    }
    
    private HashMap<String, String> importImplementation(Node n) {
        HashMap<String, String> implementation = new HashMap<String, String>();
        Node implNode = childNode(n, "implementation");
        if(implNode != null) {
            for (Node implParam : childNodes(implNode, "implParm")) {
                implementation.put(implParam.getAttributes().getNamedItem("name").getNodeValue(),
                                   implParam.getTextContent());
            }
        }
        return implementation;
    }

    private void importRegisterSet(Node registerSet, RegisterBase parentBase) {
            HashMap<Register, String> aliases = new HashMap<Register, String>();

            //System.out.println(i + ": " + registerSets.item(i).getNodeName());
            Node registerSetName = childNode(registerSet, "name");
            if(registerSetName == null) return;

            //System.out.println(registerSetName.getNodeName() + " " + registerSetName.getTextContent() + " " + registerSetName.getFirstChild().getNodeValue());

            int addressBase= hexToInt(childNode(registerSet, "addressBase").getTextContent().substring(2));
            int size = 0;
            if(childNode(registerSet, "addressBits") != null) {
                size = 1<<(Integer.parseInt(childNode(registerSet, "addressBits").getTextContent().trim()));
            }
            if(childNode(registerSet, "addressSize") != null) {
                size = hexToInt(childNode(registerSet, "addressSize").getTextContent().substring(2));
            }
            RegisterBase currentBase = parentBase.add(registerSetName.getTextContent(), addressBase, size);

            System.out.println(registerSetName.getTextContent() + " 0x" + Integer.toHexString(addressBase) +
                " absolute 0x"+ Integer.toHexString(currentBase.getAbsoluteAddress()) +
                " under " + currentBase.parent.getFullName());

            RegisterCrossbarPort currentCrossbarPort = null;
            if(childNode(registerSet, "crossbarPort") != null) {
                String crossbarPortName = childNode(registerSet, "crossbarPort").getTextContent();
                currentCrossbarPort = regInfo.crossbarPortMap.get(crossbarPortName);
                if (currentCrossbarPort == null) {
                    currentCrossbarPort = new RegisterCrossbarPort(crossbarPortName);
                    regInfo.crossbarPortMap.put(crossbarPortName, currentCrossbarPort);
                }
                currentCrossbarPort.addRegisterBase(currentBase);
                if (childNode(registerSet,"crossbarPortStride")!=null) {
                    currentCrossbarPort.stride = hexToInt(childNode(registerSet,"crossbarPortStride").getTextContent().substring(2));
                }
            }
            RegisterBusStop currentBusStop = null;
            if(childNode(registerSet, "busStopNum") != null) {
                String busStopName = childNode(registerSet, "busStopNum").getTextContent();
                currentBase.busStopId = busStopName;
                currentBusStop = currentCrossbarPort.busStopMap.get(busStopName);
                if (currentBusStop == null) {
                    currentBusStop = new RegisterBusStop(busStopName);
                    currentCrossbarPort.busStopMap.put(busStopName, currentBusStop);
                }
                currentBusStop.bases.add(currentBase);
                if (childNode(registerSet,"busStopStride") != null) {
                    currentBusStop.stride = hexToInt(childNode(registerSet,"busStopStride").getTextContent().substring(2));
                }
            }
            
            if(childNode(registerSet, "atomic") != null) {
                currentBase.atomic_width = Integer.parseInt(childNode(registerSet, "atomic").getTextContent().trim())/32;
            }

            currentBase.accessMethods = accessMethods(childNode(registerSet, "accessMethods"));

            int numberChlindRegisterSets = numberChildNodes(registerSet, "registerSet");
            for(int i=0; i<numberChlindRegisterSets; i+=1) {
                Node childRegisterSet = childNode(registerSet, "registerSet", i);
                if(childRegisterSet != null) {
                    importRegisterSet(childRegisterSet, currentBase);
                }
            }

            Node typedefs = childNode(registerSet, "typedefs");
            if(typedefs != null) {
                int numberTypedefs = numberChildNodes(typedefs, "enum");
                for (int i=0; i<numberTypedefs; i++) {
                    Node typedef = childNode(typedefs, "enum", i);
                    String name = childNode(typedef, "name").getTextContent();
                    int length = Integer.parseInt(childNode(typedef, "length").getTextContent().trim());
                    ConstantsEnum e = importEnumTypedef(typedef, name, length);
                    regInfo.sharedEnumTypedefs.add(e);
                    currentBase.enums.add(e);
                }
                numberTypedefs = numberChildNodes(typedefs, "struct");
                for (int i=0; i<numberTypedefs; i++) {
                    Node typedef = childNode(typedefs, "struct", i);
                    String name = childNode(typedef, "name").getTextContent();
                    int width = Integer.parseInt(childNode(typedef, "width").getTextContent().trim());
                    HashMap<String,RegisterField> fields = new HashMap<String,RegisterField>();
                    BigInteger defaults[] = importRegisterFields(name, typedef, fields);
                    currentBase.structs.add(new RegisterStruct(name, width, fields, defaults));
                }
            }

            Node registers = childNode(registerSet, "registers");
            if(registers == null) return;

            for(int j = 0; j < registers.getChildNodes().getLength(); j++) {
                Node register = registers.getChildNodes().item(j);
                if(register.getNodeName() != "register") continue;
                String name = childNode(register, "name").getTextContent();
                String oldName = "";
                if(childNode(register, "oldName") != null) {
                    oldName = childNode(register, "oldName").getTextContent();
                }
                String registerName = (name == "") ? oldName : name;
                //System.out.println("name = " + name);
                String brief = childNode(register, "brief").getTextContent();
                String description = childNode(register, "description").getTextContent();

                if(childNodeDimension(register, "entries", 0) == null) {
                    System.err.println("Missing entries tag (dimension 0) in register " + name);
                    failed = true;
                    continue;
                }
                int entries = Integer.parseInt(childNodeDimension(register, "entries", 0).getTextContent().trim());

                int entries_1 = 1;
                if(childNodeDimension(register, "entries", 1) != null) {
                    entries_1 = Integer.parseInt(childNodeDimension(register, "entries", 1).getTextContent().trim());
                }
                int entries_2 = 1;
                if(childNodeDimension(register, "entries", 2) != null) {
                    entries_2 = Integer.parseInt(childNodeDimension(register, "entries", 2).getTextContent().trim());
                }
                int baseEntry = 0;
                if(childNodeDimension(register, "baseEntry", 0) != null) {
                    baseEntry = Integer.parseInt(childNodeDimension(register, "baseEntry", 0).getTextContent().trim());
                }
                int baseEntry_1 = 0;
                if(childNodeDimension(register, "baseEntry", 1) != null) {
                    baseEntry_1 = Integer.parseInt(childNodeDimension(register, "baseEntry", 1).getTextContent().trim());
                }
                int baseEntry_2 = 0;
                if(childNodeDimension(register, "baseEntry", 2) != null) {
                    baseEntry_2 = Integer.parseInt(childNodeDimension(register, "baseEntry", 2).getTextContent().trim());
                }

                if(childNode(register, "width") == null) {
                    System.err.println("Missing width tag in register " + name);
                    failed = true;
                }
                int width = Integer.parseInt(childNode(register, "width").getTextContent().trim());

                int bits = 0;
                if(childNode(register, "bits") != null) {
                    bits = Integer.parseInt(childNode(register, "bits").getTextContent().trim());
                }

                boolean implemented = Boolean.parseBoolean(childNode(register, "implemented").getTextContent());

                boolean atomic = false;
                int atomic_width = 1;
                if(childNode(register, "atomic") != null) {
                    String a = childNode(register, "atomic").getTextContent();
                    try {
                        atomic_width = Integer.parseInt(a);
                        if((atomic_width % 32 != 0) || (atomic_width <= 0)) {
                            System.err.println("Invalid atomic width " + atomic_width + " in register " + name);
                            failed = true;
                        }
                        atomic_width /= 32;
                        if(atomic_width != 1) {
                            atomic = true;
                        }

                    } catch (NumberFormatException e) {
                        atomic = Boolean.parseBoolean(a);
                        if(atomic) {
                            atomic_width = width;
                        } else {
                            atomic_width = 1;
                        }
                    }
                }

                boolean suppressStructGeneration = (((Element)register).getAttribute("suppressStructGeneration").equals("true"));

                // Either alias or addressOffset must be present
                String alias = null;
                if(childNode(register, "alias") != null) {
                    alias = childNode(register, "alias").getTextContent();
                }

                int address = 0;
                if(alias == null) {
                    // address is hex, so remove the "0x"
                    //int addressUnit = hexToInt(childNode(registerSets.item(i), "addressUnit").getTextContent().substring(2));
                    address = hexToInt(childNode(register, "addressOffset").getTextContent().substring(2));
                }

                //int extraSpacing = hexToInt(childNode(register, "extraSpacing").getTextContent().substring(2));//*addressUnit;
                int extraSpacing = 0;
                if(childNodeDimension(register, "stride", 0) != null) {
                    extraSpacing = hexToInt(childNodeDimension(register, "stride", 0).getTextContent().substring(2)) - width;//*addressUnit;
                }
                int extraSpacing_1 = 0;
                if(childNodeDimension(register, "stride", 1) != null) {
                    extraSpacing_1 = hexToInt(childNodeDimension(register, "stride", 1).getTextContent().substring(2)) - width;//*addressUnit;
                }
                int extraSpacing_2 = 0;
                if(childNodeDimension(register, "stride", 2) != null) {
                    extraSpacing_2 = hexToInt(childNodeDimension(register, "stride", 2).getTextContent().substring(2)) - width;//*addressUnit;
                }

                HashMap<String,RegisterField> fields = new HashMap<String,RegisterField>();
                BigInteger defaults[] = importRegisterFields(registerName, register, fields);

                Node resetDomainsNode = childNode(register, "resetDomains");
                int numberResetDomains = numberChildNodes(resetDomainsNode, "resetDomain");
                ResetDomain resetDomains[] = new ResetDomain[numberResetDomains];
                for(int k=0; k < numberResetDomains; k++) {
                    String rd = childNode(resetDomainsNode, "resetDomain", k).getTextContent();
                    try {
                        resetDomains[k] = Enum.valueOf(ResetDomain.class, rd);
                    } catch(IllegalArgumentException e) {
                        resetDomains[k] = null;
                        System.err.println(String.format("    WARNING: %s ResetDomain %s does not exist in " +
                                "com.fulcrummicro.hw.verification.lib.testcase.ResetType", name, rd));
                    }
                }

                Vector<String> accessMethods = accessMethods(childNode(register, "accessMethods"));

                Integer structId = null;
                if(childNode(register, "id") != null) {
                    structId = new Integer(Integer.valueOf(childNode(register,"id").getTextContent()));
                }

                HashMap<String, String> implementation = importImplementation(register);

                ArrayList<String> names = new ArrayList<String>();
                if(name != "") names.add(name);
                if(!excludeOldName && (oldName != "")) names.add(oldName);

                for (String currentName : names) {
                    Register currentRegister =
                        new Register(regInfo, currentName, entries, width, implemented, defaults, fields, extraSpacing, resetDomains);
                    currentRegister.address_unit = 1;//addressUnit;
                    currentRegister.bits = bits;
                    currentRegister.base_entry = baseEntry;
                    currentRegister.entries_1 = entries_1;
                    currentRegister.entries_2 = entries_2;
                    currentRegister.base_entry_1 = baseEntry_1;
                    currentRegister.base_entry_2 = baseEntry_2;
                    currentRegister.extra_spacing_1 = extraSpacing_1;
                    currentRegister.extra_spacing_2 = extraSpacing_2;
                    currentRegister.structId = structId;
                    currentRegister.atomic = atomic;
                    currentRegister.atomic_width = atomic_width;
                    currentRegister.suppressStructGeneration = suppressStructGeneration;
                    currentRegister.accessMethods = accessMethods;
                    currentRegister.implementation = implementation;
                    currentRegister.briefDescription = brief;
                    currentRegister.description = description;
                    if(currentName == oldName) currentRegister.oldName = true;
                    if(regInfo.regMap.containsKey(currentRegister.name)) {
                        System.err.println("    ERROR: duplicate register named " + currentRegister.name + " in " +
                                           regInfo.regMap.get(currentRegister.name).base.name);
                        this.failed = true;
                    }
                    currentBase.add(currentRegister);
                    if(alias == null) {
                        currentRegister.address = address;
                        if(currentRegister.checkForErrors()) {
                            failed = true;
                        } else {
                            if(regInfo.verbose) System.out.println(currentRegister.name);
                            regInfo.regMap.put(currentRegister.name, currentRegister);
                        }
                        currentRegister.checkForWarnings();
                    } else {
                        currentRegister.alias = alias;
                        aliases.put(currentRegister, alias);
                    }
                }
            }

            for (Register currentRegister : aliases.keySet()) {
                String alias = aliases.get(currentRegister);
                Register aliasRegister = regInfo.regMap.get(alias);
                if(aliasRegister == null) {
                    System.err.println(String.format("    ERROR: register %s's alias %s not found.", currentRegister.name, alias));
                    this.failed = true;
                    continue;
                }
                currentRegister.address = aliasRegister.address;
                if(currentRegister.checkForErrors()) {
                    failed = true;
                } else {
                    if(currentRegister.getTotalSize() != aliasRegister.getTotalSize()) {
                        System.err.println(String.format("    ERROR: register %s's size %x does not match its alias %s's size %x",
                                                         currentRegister.name, currentRegister.getTotalSize(),
                                                         aliasRegister.name, aliasRegister.getTotalSize()));
                        this.failed = true;
                    }

                    if(regInfo.verbose) System.out.println(currentRegister.name + " (" + alias + ")");
                    regInfo.regMap.put(currentRegister.name, currentRegister);
                }
                currentRegister.checkForWarnings();
            }

    }

    private BigInteger[] importRegisterFields(String registerName, Node parentNode,
                                              HashMap<String,RegisterField> fields) {
        Node fieldsNode = childNode(parentNode, "fields");

        BigInteger defaults[];
        int numberDefaults = 0;
        int numberFields = numberChildNodes(fieldsNode, "field");
        for(int k=0; k < numberFields; k++) {
            numberDefaults = Math.max(numberDefaults, numberChildNodes(childNode(fieldsNode, "field", k), "default"));
            Element defaultNode = (Element)childNode(childNode(fieldsNode, "field", k), "default");
            if((defaultNode != null) && (defaultNode.hasAttribute("entryEnd"))) {
                numberDefaults = Math.max(numberDefaults, Integer.parseInt(defaultNode.getAttribute("entryEnd"))+1);
            }
        }
        defaults = new BigInteger[Math.max(1,numberDefaults)];
        defaults[0] = new BigInteger("0");

        for(int k=0; k < numberFields; k++) {
            Node currentFieldNode = childNode(fieldsNode, "field", k);
            String originalFieldName = childNode(currentFieldNode, "name").getTextContent();
            String fieldName = escape(originalFieldName);
            String oldFieldName = "";
            if(childNode(currentFieldNode, "oldName") != null) {
                oldFieldName = escape(childNode(currentFieldNode, "oldName").getTextContent());
            }
            int position = Integer.parseInt(childNode(currentFieldNode, "position").getTextContent().trim());
            int length = Integer.parseInt(childNode(currentFieldNode, "length").getTextContent().trim());
            RegisterType fieldType = Enum.valueOf(RegisterType.class, childNode(currentFieldNode, "type").getTextContent());
            String description = childNode(currentFieldNode, "description").getTextContent();

            int entries = 1;
            int baseEntry = 0;
            if(childNode(currentFieldNode, "entries") != null) {
                entries = Integer.parseInt(childNode(currentFieldNode, "entries").getTextContent().trim());
                if(childNode(currentFieldNode, "baseEntry") != null) {
                    baseEntry = Integer.parseInt(childNode(currentFieldNode, "baseEntry").getTextContent().trim());
                }
            }

            if(numberDefaults == 1) {
                if(defaults[0] == null) defaults[0] = new BigInteger("0");
                // defaults are in hex, so strip off the "0x"
                BigInteger defBi = getDefault(childNode(currentFieldNode, "default"), length, registerName, fieldName);
                if(defBi == null) failed = true;
                else for (int i = 0; i < entries; i++)
                    defaults[0] = defaults[0].add(defBi.shiftLeft(length*i + position));
            } else {
                for(int m=0; m < numberChildNodes(currentFieldNode, "default"); m++) {
                    Node currentDefaultNode = childNode(currentFieldNode, "default", m);
                    // defaults are in hex, so strip off the "0x"
                    int start = Integer.parseInt(((Element)currentDefaultNode).getAttribute("entryStart"));
                    int end = Integer.parseInt(((Element)currentDefaultNode).getAttribute("entryEnd"));
                    BigInteger defBi = getDefault(currentDefaultNode, length, registerName, fieldName);
                    if(defBi == null) failed = true;
                    else for(int n=start; n<=end; n++) {
                        if(defaults[n] == null) defaults[n] = new BigInteger("0");
                        for (int i = 0; i < entries; i++)
                            defaults[n] = defaults[n].add(defBi.shiftLeft(length*i + position));
                    }
                }
            }

            String typedefName;
            if(childNode(currentFieldNode, "globalName") != null) {
                typedefName = childNode(currentFieldNode, "globalName").getTextContent();
            } else {
                typedefName = String.format("%s_%s",
                                            Utility.underscoreToCamelCase(registerName, true),
                                            fieldName == "" ? oldFieldName : fieldName);
            }

            ConstantsEnum typedef = importEnumTypedef(currentFieldNode,
                                                  typedefName,
                                                  length);
            
            HashMap<String, String> implementation = importImplementation(currentFieldNode);

            if(fieldName != "") {
                RegisterField rf = new RegisterField(fieldName, position, length, fieldType, false, typedef, entries, baseEntry);
                rf.originalDesc = originalFieldName;
                rf.implementation = implementation;
                rf.fullDescription = description;
                RegisterField removed = fields.put(fieldName.toLowerCase(), rf);
                if(removed != null) {
                    System.err.println("ERROR: Multiple fields with the name " + removed.desc + " in register " + registerName);
                    failed = true;
                }

            }
            if(!excludeOldName && (oldFieldName != "")) {
                RegisterField rf = new RegisterField(oldFieldName, position, length, fieldType, false, typedef, entries, baseEntry);
                rf.implementation = implementation;
                rf.fullDescription = description;
                RegisterField removed = fields.put(oldFieldName.toLowerCase(), rf);
                if(removed != null) {
                    System.err.println("ERROR: Multiple fields with the name " + removed.desc + " in register " + registerName);
                    failed = true;
                }
            }
        }
        return defaults;
    }
    
    private BigInteger getDefault(Node node, int length, String registerName, String fieldName) {
        BigInteger value = new BigInteger(node.getTextContent().substring(2), 16);
        BigInteger mask = new BigInteger("0").setBit(length).add(new BigInteger("-1"));
        BigInteger result = value.and(mask);
        if(!value.equals(result)) {
            System.err.println(String.format("  ERROR: default of %s for %s.%s greater than %d bits",
                    node.getTextContent(), registerName, fieldName, length));
            return null;
        }
        return result;
    }

    private ConstantsEnum importEnumTypedef(Node currentFieldNode, String typedefName, int length) {
        ConstantsEnum typedef = null;
        Element valuesNode = (Element)childNode(currentFieldNode, "values");
        if(valuesNode != null) {
            String ref = valuesNode.getAttribute("typedef");
            if(ref != "") {
                //System.out.println(ref);
                for (ConstantsEnum t : regInfo.sharedEnumTypedefs) {
                    if(ref.equals(t.getName())) return t;
                }
                System.out.println("ERROR: referenced typedef "+ ref + " not found");
                return null;
            }
            int numberValues = numberChildNodes(valuesNode, "value");
            if(regInfo.verbose) System.out.println("found " + numberValues + " ConstantsEnumValue values in " + typedefName);
            typedef = new ConstantsEnum(typedefName, length);
            for(int m=0; m<numberValues; m++) {
                Element valueNode = (Element)childNode(valuesNode, "value", m);
                String valueName = valueNode.getAttribute("name");
                int valueEncoding = Integer.parseInt(valueNode.getAttribute("encoding"));
                ConstantsEnumValue typedefValue
                    = new ConstantsEnumValue(typedef, valueName,
                                             escape(valueName), valueEncoding);
                typedefValue.setDescription(valueNode.getTextContent());
                typedef.addValue(typedefValue);
            }
        }
        return typedef;
    }

    public boolean importXml(String filename) {
        Document document;
        DocumentBuilderFactory factory;
        InputStream is;

        factory = DocumentBuilderFactory.newInstance();
        factory.setValidating(false);
        factory.setNamespaceAware(true);
        factory.setXIncludeAware(true);
        try {
           DocumentBuilder builder = factory.newDocumentBuilder();
           //System.out.println("Builder: " + builder.getClass().getName());
           builder.setEntityResolver(new EntityResolver() {
               public InputSource resolveEntity(java.lang.String publicId, java.lang.String systemId)
                      throws SAXException, java.io.IOException
               {
                 if (publicId.equals("-//Fulcrum Microsystems//DTD registerInfo/EN")) {
                   // this deactivates the  DTD
                   return new InputSource(new ByteArrayInputStream("<?xml version='1.0' encoding='UTF-8'?>".getBytes()));
                 } else return null;
               }
           });
           if (filename.equals("-"))
               is = System.in;
           else
               is = new FileInputStream(new File(filename));
           document = builder.parse(is);
        } catch (SAXException sxe) {
           // Error generated during parsing)
           Exception  x = sxe;
           if (sxe.getException() != null)
               x = sxe.getException();
           x.printStackTrace();
           return true;
        } catch (ParserConfigurationException pce) {
            // Parser with specified options can't be built
            pce.printStackTrace();
            return true;
        } catch (IOException ioe) {
           // I/O error
           ioe.printStackTrace();
           return true;
        }

        // Get the last child because the first could be the DOCTYPE, etc
        Node registerInfo = document.getLastChild();

        NodeList registerSets = registerInfo.getChildNodes();
        for(int i = 0; i < registerSets.getLength(); i++) {
            importRegisterSet(registerSets.item(i), regInfo.rootBase);
        }

        System.out.println("RegisterInfo imported " + Integer.toString(regInfo.regMap.size()) +  " registers.");

        return failed;
    }


    private int numberChildNodes(Node parent, String childName) {
        int result = 0;
        Node current = parent.getFirstChild();
        while(current != null) {
            if(current.getNodeName() == childName) {
                result++;
            }
            current = current.getNextSibling();
        }

        return result;
    }


    private ArrayList<Node> childNodes(Node parent, String childName) {
        ArrayList<Node> result = new ArrayList<Node>();
        Node current = parent.getFirstChild();
        while(current != null) {
            if(current.getNodeName() == childName) {
                result.add(current);
            }
            current = current.getNextSibling();
        }
        return result;
    }

}
