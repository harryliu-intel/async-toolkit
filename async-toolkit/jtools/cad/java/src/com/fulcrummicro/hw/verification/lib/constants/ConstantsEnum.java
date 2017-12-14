package com.fulcrummicro.hw.verification.lib.constants;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import org.nfunk.jep.JEP;

import com.fulcrummicro.util.misc.Utility;

public class ConstantsEnum extends ConstantsBase implements Cloneable {
    
    public Vector<ConstantsEnumValue> values;
    Integer size = null; // width
    private String sizeExpression = null;

    /** DIGESTER ONLY */
    @Deprecated
    public ConstantsEnum() {
        values = new Vector<ConstantsEnumValue>();
    }
    
    public ConstantsEnum(String name, Integer width) {
        this();
        this.name = name;
        this.size= width;
    }

    public Integer getSize() {
        if(size == null) { // done here because of use by registerInfo which doesn't process()
            size=1;
            for (ConstantsEnumValue value : values) {
                size = Math.max(size, bitwidth(value.getEncoding()));
            }
        }
        return size;
    }
    
    /** DIGESTER ONLY
     * @param encoding the encoding to set
     */
    @Deprecated
    public void setSizeExpression(String size) {
        this.sizeExpression = size;
    }

    @SuppressWarnings("unchecked")
    @Override
    public ConstantsEnum clone() {
        ConstantsEnum clone = null;
        try {
            clone = (ConstantsEnum)super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
        clone.name = name;
        clone.size = size;
        clone.values = (Vector<ConstantsEnumValue>) values.clone();
        
        return clone;
    }

    public void addValue(ConstantsEnumValue v) {
        v.setConstantsEnum(this);
        values.add(v);
    }

    @Override
    protected boolean isValid() {
        boolean valid = true;
        for (ConstantsEnumValue value : values) {
            valid &= value.isValid();
        }
        return valid;
    }
    
    protected boolean process(JEP parser) {
        boolean valid = true;
        if((size == null) && (sizeExpression != null)) {
            size = processInt(parser, sizeExpression);
        }
        for (ConstantsEnumValue value : values) {
            valid = value.process(parser) & valid;
        }
        getSize();
        return valid;
    }
    
    public String getVerilogX() {
        String x = "";
        for(int i=0;i<size;i++) { x += "x"; }
        return String.format("%d'b%s", size, x);
    }
    
    @Override
    protected boolean check() {
        return !checkForDuplicateValues("enum");
    }
    
    /**
     * @param prefix
     * @return true if duplicates found
     */
    public boolean checkForDuplicateValues(String prefix) {
        boolean failed = false;
        HashMap<Integer, ConstantsEnumValue> map = new HashMap<Integer, ConstantsEnumValue>(); 
        for (ConstantsEnumValue value : values) {
            Integer encoding = value.getEncoding();
            if(map.containsKey(encoding)) {
                if(prefix != null) {
                    System.err.println(String.format("WARNING in %s: Duplicate in %s for encoding %d: %s and %s",
                                                     prefix, name, encoding, map.get(encoding).getName(), value.getName()));
                }
                failed = true;
            } else {
                map.put(encoding, value);
            }
        }
        return failed;
    }
    
    @Override
    protected String toText() {
        String result = String.format("    ConstantsEnum: %s\n", getName());
        for (ConstantsEnumValue value : values) {
            result += value.toText();
        }
        return result;
    }
    
    @Override
    public String toCast() {
        String result = String.format("  // Enum %s %s\n", getName(), getCastDescription());
        for (ConstantsEnumValue value : values) {
            result += value.toCast();
        }
        return result + "\n";
    }
    
    public String toVerilog() {
        String result = "";
        if(size == null) {
            result += "  // size == null in ConstantsEnum " + getName() + ". typedef not generated.\n";
        } else if(checkForDuplicateValues(null)) {
            result += "  // duplicate values in ConstantsEnum " + getName() + ". typedef not generated.\n";
        } else {
            result += (size == 1) ? "typedef enum logic {\n" :
                                           String.format("typedef enum logic[%d:0] {\n", size-1);
            for (Iterator<ConstantsEnumValue> i = values.iterator(); i.hasNext(); ) {
                ConstantsEnumValue value = i.next();
                result += value.toVerilog(size);
                if(i.hasNext()) result += ",";
                result += "\n";
            }
            result += String.format("} %s;\n", getStructName());
        }
        for (ConstantsEnumValue value : values) {
            result += value.toVerilog();
        }
        
        return result + "\n";
    }
}
