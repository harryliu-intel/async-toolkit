package com.fulcrummicro.hw.verification.chip.mgmt.registers;

/** Register Data Type definition class **/
public enum RegisterDataType {
    UNSIGNED,
    SIGNED,
    BOOL;

    public String toCspType(int width) {
        switch(this) {
            case UNSIGNED: return "int(" + width + ")";
            case SIGNED: return "sint(" + width + ")";
            case BOOL: return "bool";
        }
        return toString();
    }
}
