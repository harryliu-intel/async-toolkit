package com.fulcrummicro.hw.verification.lib.constants;

import org.nfunk.jep.JEP;

public class ConstantsConstant extends ConstantsBase {
    private Integer value = null;
    private String valueExpression;
    Integer size = null;
    private String sizeExpression = null;
    private ConstantsCell constantsCell = null;

    /** DIGESTER ONLY */
    @Deprecated
    public ConstantsConstant() {
    }

    protected void setConstantsCell(ConstantsCell cell) {this.constantsCell = cell;}
    protected ConstantsCell getConstantsCell() {return constantsCell;}

    /** DIGESTER ONLY
     * @param value the value to set
     */
    @Deprecated
    public void setValueExpression(String value) {
        this.valueExpression = value;
    }

    /**
     * @return the value
     */
    public int getValue() {
        if(value ==null) throw new NullPointerException(constantsCell.getName()+"."+getName());
        return value;
    }
    
    public String getVerilogValue() {
        if(size == null) {
            return value.toString();
        } else {
            return String.format("%d'd%d", size, value);
        }
    }

    public Integer getSize() {
        return size;
    }
    
    /** DIGESTER ONLY
     * @param encoding the encoding to set
     */
    @Deprecated
    public void setSizeExpression(String size) {
        this.sizeExpression = size;
    }

    @Override
    protected boolean isValid() {
        return value != null;
    }
    
    /**
     * @param parser
     * @return updated if changed
     */
    protected boolean process(JEP parser) {
        if(value != null) {
            return false; // already set
        }
        if((size == null) && (sizeExpression != null)) {
            size = processInt(parser, sizeExpression);
        }
        value = processInt(parser, valueExpression, true);
        return isValid();
    }
    
    @Override
    protected boolean check() {
        if((size != null) && (bitwidth(value) > size)) {
            ConstantsDigester.error(String.format("constant %s extends beyond specified size %d to %d",
                    getName(), size, bitwidth(value)));
            return false;
        }
        return true;
    }
    
    @Override
    protected String toText() {
        return checkValid() ? String.format("    ConstantsConstant: %s = %d\n", getName(), getValue()) : "";
    }

    @Override
    protected String toCast() {
        return checkValid() ?  String.format("%-40s%s\n", 
                                             String.format("  int %s = %d;", getName(), getValue()),
                                             getCastDescription()
                                             )
                               : "";
    }

    @Override
    protected String toVerilog() {
        return checkValid() ?  String.format("%-40s%s\n", 
                                             String.format("  `define %s%s %d", 
                                                           getConstantsCell().getPrefix(), getName(), getValue()),
                                             getCastDescription()
                                             )
                               : "";
    }
}
