package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.util.HashMap;
import java.util.Map;
import java.util.Iterator;

import com.fulcrummicro.hw.verification.lib.constants.ConstantsEnum;
import com.fulcrummicro.util.misc.Utility;

//import static com.fulcrummicro.hw.verification.chip.mgmt.RegisterType.*;
/** Register field definition class **/

/* this is a wrapper class only, make everything public */
public class RegisterField {

    /** length in bits */
    public int len;

    /** position in bits starting from 0-indexed LSB */
    public int pos;

    /** field description, attempt to make this match property */
    public String desc;
    
    /** unmodified/unescaped desc */
    public String originalDesc;
    
    /** full description, not just name */
    public String fullDescription = "";
    
    public boolean oldVersion = false;

    public RegisterType type;

    public RegisterDataType dataType;
    
    public ConstantsEnum typedef;
    HashMap<String, String> implementation = null;
    
    public int entries = 1;
    public int baseEntry = 0;
    public int currentEntry = 0;

    protected RegisterField(String desc, int pos, int len, RegisterType type, RegisterDataType dataType, boolean old, ConstantsEnum typedef) {
        this.desc = desc;
        this.originalDesc = desc;
        this.pos = pos;
        this.len = len;
        this.type = type;
        this.dataType = dataType;
        this.oldVersion = old;
        this.typedef = typedef;
    }

    protected RegisterField(String desc, int pos, int len, RegisterType type, RegisterDataType dataType, boolean old, ConstantsEnum typedef, int entries, int baseEntry) {
        this.desc = desc;
        this.originalDesc = desc;
        this.pos = pos;
        this.len = len;
        this.type = type;
        this.dataType = dataType;
        this.oldVersion = old;
        this.typedef = typedef;
        this.entries = entries;
        this.baseEntry = baseEntry;
    }
    
    public String getVerilogStructName() {
        return Utility.possibleUnderscoreToLowerCase(desc);
    }
    
    public boolean isImplementationTrue(String attribute) {
        return (implementation != null) &&
               (implementation.get(attribute) != null) &&
               implementation.get(attribute).toLowerCase().equals("true");
    }
    
    public boolean isVolatile() {
        return isImplementationTrue("volatile");
    }
    
    /** defaults to base type if not set */
    public RegisterType getDebugType() {
        String attribute = "debugType";
        if(implementation.get(attribute) == null) return type; 
        return Enum.valueOf(RegisterType.class, implementation.get(attribute));
    }

}

