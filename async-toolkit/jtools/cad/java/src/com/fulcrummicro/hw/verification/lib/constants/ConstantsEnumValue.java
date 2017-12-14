/**
 * 
 */
package com.fulcrummicro.hw.verification.lib.constants;

import org.nfunk.jep.JEP;

import com.fulcrummicro.util.misc.Utility;

public class ConstantsEnumValue extends ConstantsBase {
    private String originalName = "";
    private Integer encoding = null;
    private String encodingExpression;
    private ConstantsEnum constantsEnum;

    /** DIGESTER ONLY */
    @Deprecated
    public ConstantsEnumValue() {
    }
    
    public ConstantsEnumValue(ConstantsEnum e, String originalName, String name, Integer encoding) {
        this.constantsEnum = e;
        this.originalName = originalName;
        this.name = name;
        this.encoding = encoding;
        encodingExpression = encoding.toString();
    }

    @Override
    protected boolean isValid() {
        return encoding != null;
    }
    
    public String getFullName() {
        return String.format("%s_%S", constantsEnum.getName(), getName());
    }
    
    public String getVerilogName() {
        return String.format("%S__%S",
                constantsEnum.getVerilogName(), 
                Utility.possibleUnderscoreToLowerCase(getName()));
    }
    
    /**
     * @param parser
     * @return updated
     */
    protected boolean process(JEP parser) {
        if(encoding != null) {
            return false; // already set
        }
        encoding = processInt(parser, encodingExpression);
        return isValid();
    }
        
    /** DIGESTER ONLY
     * @param encoding the encoding to set
     */
    @Deprecated
    public void setEncodingExpression(String encoding) {
        this.encodingExpression = encoding;
    }

    /**
     * @return the encoding
     */
    public int getEncoding() {
        return encoding;
    }
    
    public String getVerilogEncoding() {
        return String.format("%d'd%s", constantsEnum.getSize(), getEncoding());
    }
    
    public String getOriginalName() { return originalName; }
    
    /**
     * @param constantsEnum the constantsEnum to set
     */
    protected void setConstantsEnum(ConstantsEnum constantsEnum) {
        this.constantsEnum = constantsEnum;
    }
    public ConstantsEnum getConstantsEnum() {return constantsEnum;}


    @Override
    protected String toText() {
        return checkValid() ? String.format("      ConstantsEnum.Value: %s = %d\n", getName(), getEncoding()) : "";
    }
    
    @Override
    protected String toCast() {
        return checkValid() ? String.format("  int %s_%s = %d; %s\n", 
                                            constantsEnum.getName(), getName(), 
                                            getEncoding(), getCastDescription())
                            : "";
    }
    
    protected String toVerilog(int width) {
        return checkValid() ? String.format("  %s = %d'd%d", 
                                            getFullName(), 
                                            width, getEncoding())
                            : "";
    }
    @Override
    protected String toVerilog() {
        return checkValid() ? String.format("  `define %s_%s %d %s\n", 
                                            constantsEnum.getName(), getName(), 
                                            getEncoding(), getCastDescription())
                            : "";
    }
}