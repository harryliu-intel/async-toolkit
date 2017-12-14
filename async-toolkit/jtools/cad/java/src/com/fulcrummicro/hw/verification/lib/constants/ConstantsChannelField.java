package com.fulcrummicro.hw.verification.lib.constants;

import org.nfunk.jep.JEP;


public class ConstantsChannelField extends ConstantsBase implements Cloneable {

    public enum ConstantsChannelFieldType {
        RESERVED("reserved",null), 
        CHANNEL("",null), 
        NODE("node",1),
        E1of2("e1of2",1), 
        E1of3("e1of3",2), 
        E1of4("e1of4",2);
        private final String typeString;
        private final Integer bits;
        private ConstantsChannelFieldType(String typeString, Integer bits) {
            this.typeString = typeString;
            this.bits = bits;
        }
        public static ConstantsChannelFieldType lookup(String typeString) {
            for (ConstantsChannelFieldType type : ConstantsChannelFieldType.values()) {
                if(typeString.equalsIgnoreCase(type.typeString)) {
                    return type;
                }
            }
            return CHANNEL;
        }
        public String getTypeString(int bitwidth) {
            if((bits==null) || (bitwidth <= bits)) {
                return typeString;
            }
            return String.format("%s[%d]", typeString, getNumChannels(bitwidth));
        }
        public int getNumChannels(int bitwidth) {
            if((bits==null) || (bitwidth <= bits)) {
                return 1;
            }
            return (int)Math.ceil(1.0*bitwidth/bits);
        }
    }
    
    private ConstantsChannelFieldType type = null;
    private String typeString = null;
    private ConstantsChannel typeConstantsChannel;
    private String enumString = null;
    private ConstantsEnum enumType = null;
    private Integer position = null;
    private String positionExpression = null;
    private Integer width = null;
    private String widthExpression;
    private Integer array = 1;
    private String arrayExpression;
    private String direction = "+";
    private ConstantsChannel constantsChannel;
    private boolean overlap = false;

    /** DIGESTER ONLY */
    @Deprecated
    public ConstantsChannelField() {}
    
    protected ConstantsChannelField(ConstantsChannelFieldType type) {
        setType(type);
    }
    
    public ConstantsChannelField clone() {
        try {
            return (ConstantsChannelField) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
            return null;
        }
    }

    @Override
    protected boolean isValid() {
        if(getType() == ConstantsChannelFieldType.CHANNEL) {
            return (position != null) && (typeConstantsChannel != null);
        } else {
            return (position != null) && (width != null);
        }
    }
    
    /**
     * @param parser
     * @return updated if changed
     */
    protected boolean process(JEP parser) {
        if(isValid()) {
            return false; // already set
        }
        if(widthExpression != null) {
            width = processInt(parser, widthExpression);
        }
        if(arrayExpression != null) {
            array = processInt(parser, arrayExpression);
        }
        if(positionExpression != null) {
            position = processInt(parser, positionExpression);
        } else {
            position = constantsChannel.nextPosition();
        }
        if(getType() == ConstantsChannelFieldType.CHANNEL) {
            typeConstantsChannel = constantsChannel.getConstantsCell().getChannel(typeString);
            if(typeConstantsChannel == null) {
                ConstantsDigester.error(String.format("Type %s not found for %s.%s", typeString, constantsChannel.getName(), getName()));
            } else {
                typeConstantsChannel.process(parser);
                if(width == null) {
                    width = typeConstantsChannel.getWidth();
                } else if (width != typeConstantsChannel.getWidth()) {
                    ConstantsDigester.error(String.format("Width %d for %s.%s does not match type %s %d.", 
                                                          width, constantsChannel.getName(), getName(),
                                                          typeString, typeConstantsChannel.getWidth()));
                }
            }
        }
        if(enumString != null) {
            enumType = constantsChannel.getConstantsCell().getEnum(enumString);
            if(enumType == null) {
                ConstantsDigester.error(String.format("Enum encoding %s not found for %s.%s", enumString, constantsChannel.getName(), getName()));
            } else {
                if(width == null) {
                    width = enumType.getSize();
                } else if(width != enumType.getSize()) {
                    ConstantsDigester.error(String.format("Specified width %d doesn't match enum encoding %s.%s.%s %d",
                            width, constantsChannel.getName(), getName(), enumString, enumType.getSize()));
                }
            }
        }
        return isValid();
    }

    public int getCastDefchanNumChannels() {
        if(getType() == ConstantsChannelFieldType.CHANNEL) {
            if(typeConstantsChannel.getType() == null) {
                return 1;
            } else {
                return typeConstantsChannel.getType().getNumChannels(typeConstantsChannel.getWidth());
            }
        } else {
            return getType().getNumChannels(getWidth());
        }
    }
    
    public String getCastDefchanTypeString() {
        String result;
        if(getType() == ConstantsChannelFieldType.CHANNEL) {
            if(typeConstantsChannel.getType() == null) {
                result = typeConstantsChannel.getNameCamelCase(true);
            } else {
                result = typeConstantsChannel.getType().getTypeString(typeConstantsChannel.getWidth());
            }
        } else {
            result = getType().getTypeString(getWidth());
        }
        return String.format("%-20s", result);
    }

    public String getCastFullTypeString() {
        String result;
        if(getType() == ConstantsChannelFieldType.CHANNEL) {
            result = typeConstantsChannel.getNameCamelCase(true);
        } else {
            result = String.format("int(%2d)", getWidth());
        }
        return String.format("%-20s", result);
    }

    public String getVerilogFullTypeString() {
        String arrayResult ="";
        String result;
        if(getArray()>1) {
            arrayResult = String.format(" [%d:0]", getArray()-1);
        }
        if(getType() == ConstantsChannelFieldType.CHANNEL) {
            result = typeConstantsChannel.getStructName() + arrayResult;
        } else if(enumType != null) {
            result = enumType.getStructName() + arrayResult;
        } else if(getWidth() > 1) {
            result = String.format("logic%s [%2d:0]", arrayResult, getWidth()-1);
        } else {
            result = String.format("logic%s", arrayResult);;
        }
        return String.format("%-20s", result);
    }

    @Override
    protected String toText() {
        return checkValid() ? String.format("      ConstantsChannelField: %s %s\n", getName(), getTypeString()) : "";
    }

    @Override
    protected String toCast() {
        String overlapString = allowOverlap() ? "[overlap]" : "";
        if(width == 1) {
            return checkValid() ? String.format("%-60s%s\n",
                                                String.format("  int %s_b_%s = %d;", 
                                                              constantsChannel.getName(), getName(),
                                                              position),
                                                String.format("// %s %s %s",
                                                              getTypeString(), getCastDescription(), overlapString)
                                                ) : "";
        } else {
            return checkValid() ? String.format("  %-28s %-28s %s",
                String.format("int %s_l_%s = %d;", 
                              constantsChannel.getName(), getName(),
                              position),
                String.format("int %s_h_%s = %d;", 
                              constantsChannel.getName(), getName(),
                              position + width - 1),
                String.format("// %s %s %s\n", getTypeString(), getCastDescription(), overlapString)) : "";
        }
    }
    
    protected String toCastStruct() {
        if(getType() == ConstantsChannelFieldType.CHANNEL) {
            return String.format("%-40s%s\n",
                                 String.format("      %-8s %s;", typeConstantsChannel.getNameCamelCase(true), getName()),
                                 String.format("// %s %s", getTypeString(), getCastDescription()));
        } else {
            return String.format("%-40s%s\n",
                                 String.format("      %-8s %s;", String.format("int(%d)", width), getName()),
                                 String.format("// %s %s", getTypeString(), getCastDescription()));
        }
    }

    @Override
    protected String toVerilog() {
        String overlapString = allowOverlap() ? "[overlap]" : "";
        if(width == 1) {
            return checkValid() ? String.format("%-60s%s\n",
                                                String.format("  `define %s_b_%s %d", 
                                                              constantsChannel.getName(), getName(),
                                                              position),
                                                String.format("// %s %s %s",
                                                              getTypeString(), getCastDescription(), overlapString)
                                                ) : "";
        } else {
            return checkValid() ? String.format("  %s\n  %s\n    %s\n",
                String.format("`define %s_l_%s %d", 
                              constantsChannel.getName(), getName(),
                              position),
                String.format("`define %s_h_%s %d", 
                              constantsChannel.getName(), getName(),
                              position + width - 1),
                String.format("// %s %s %s\n", getTypeString(), getCastDescription(), overlapString)) : "";
        }
    }
    
    /** DIGESTER ONLY
     * @param overlap
     */
    @Deprecated
    public void setOverlap(boolean overlap) {
        this.overlap = overlap;
    }

    /**
     * @return overlap
     */
    protected boolean allowOverlap() {
        return overlap;
    }

    /** DIGESTER ONLY
     * @param typeString the typeString to set
     */
    @Deprecated
    public void setTypeString(String typeString) {
        type = ConstantsChannelFieldType.lookup(typeString);
        this.typeString = typeString;
    }

    /**
     * @param type the type to set
     */
    protected void setType(ConstantsChannelFieldType type) {
        this.type = type; 
        this.typeString = type.toString();
    }
    
    /**
     * @return the typeString
     */
    public String getTypeString() {
        if(typeConstantsChannel != null) return typeConstantsChannel.getNameCamelCase(true);
        if(typeString == null) return constantsChannel.getTypeString();
        return typeString;
    }
    
    public ConstantsChannelFieldType getType() {
        if(type == null) return constantsChannel.getType();
        return type;
    }

    /** DIGESTER ONLY
     */
    @Deprecated
    public void setEnumString(String e) {
        this.enumString = e;
    }
    
    /** DIGESTER ONLY
     * @param position the position to set
     */
    @Deprecated
    public void setPositionExpression(String position) {
        this.positionExpression = position;
    }

    /**
     * @param position the position to set
     */
    protected void setPosition(int position) {
        this.position = position;
    }

    /**
     * @return the position
     */
    public int getPosition() {
        return position;
    }

    /** DIGESTER ONLY
     * @param width the width to set
     */
    @Deprecated
    public void setWidthExpression(String width) {
        this.widthExpression = width;
    }
    
    /** DIGESTER ONLY
     * @param width the width to set
     */
    @Deprecated
    public void setArrayExpression(String array) {
        this.arrayExpression = array;
    }

    /**
     * @param width the width to set
     */
    protected void setWidth(int width) {
        this.width = width;
    }

    /**
     * @return the width of a single entry in bits
     */
    public int getWidth() {
        if(width ==null) throw new RuntimeException(String.format(
                "Width not specified in %s.%s ", this.constantsChannel.getName(), name));
        return width;
    }

    /**
     * @return the width of all entries (arrayed)
     */
    public int getFullWidth() {
        return getWidth() * getArray();
    }
    
    public Integer getArray() {
        return array;
    }

    public void setArray(Integer array) {
        this.array = array;
    }

    public String getDirection() {
        return direction;
    }

    public void setDirection(String direction) {
        this.direction = direction;
    }

    /**
     * @param constantsChannel the constantsChannel to set
     */
    protected void setConstantsChannel(ConstantsChannel constantsChannel) {
        this.constantsChannel = constantsChannel;
    }
}
