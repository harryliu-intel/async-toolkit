package com.fulcrummicro.hw.verification.lib.constants;

import org.nfunk.jep.JEP;

import com.fulcrummicro.util.misc.Utility;

public abstract class ConstantsBase {
    protected String name;
    protected String description;

    /** Returns true if changed */
    protected abstract boolean process(JEP parser);
    
    protected boolean check() {return true;}
    
    protected Integer processInt(JEP parser, String valueExpression) {
        return processInt(parser, valueExpression, false);
    }

    protected Integer processInt(JEP parser, String valueExpression, boolean update) {
        if(valueExpression == null) {
            throw new NullPointerException(String.format("No valueExpression for %s %s", 
                                                         getClass().getSimpleName(), getName()));
        }
        if(parser.parseExpression(valueExpression) != null) {
            if(Double.isNaN(parser.getValue())) {
                return null;
            }
            Integer value = (int)parser.getValue();
            if(value != parser.getValue()) {
                ConstantsDigester.warning(String.format("non-integer result for %s: %f from %s", 
                                                      getName(), parser.getValue(), valueExpression));
            }
            if(update) {
                parser.addVariable(getName(), value);
            }
            return value;
        }
        return null;
    }
    
    public static int bitwidth(int i) {
        if(i==0) return 0;
        return 32 - Integer.numberOfLeadingZeros(i);
    }
    
    protected abstract boolean isValid();
    protected boolean checkValid() {
        if(!isValid()) {
            ConstantsDigester.error(String.format("in %s %s", this.getClass().getSimpleName(), getName()));
        }
        return isValid();
    }

    /** DIGESTER ONLY
     * @param name the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    public String getVerilogName() {
        return Utility.possibleUnderscoreToLowerCase(getName());
    }
    
    public String getStructName() {
        return String.format("%s_t", getVerilogName());
    }

    /**
     * @return the name
     */
    public String getNameCamelCase(boolean firstLetterUppercase) {
        return Utility.underscoreToCamelCase(name, firstLetterUppercase);
    }

    /** DIGESTER ONLY
     * @param description the description to set
     */
    public void setDescription(String description) {
        if(!description.trim().equals("")) {
            this.description = description.trim();
        }
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    public String getCastDescription() {
        if(description == null) {return "";}
        description = description.replaceAll("\n", "\n    // ");
        return String.format(" // %s", description);
    }
    

    protected abstract String toText();
    
    protected abstract String toCast();
    protected abstract String toVerilog();
}
