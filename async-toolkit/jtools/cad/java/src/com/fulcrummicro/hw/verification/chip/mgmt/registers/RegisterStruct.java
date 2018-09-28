package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

public class RegisterStruct {
    /** Functional spec register name **/
    String name;

    /** brief description, not just name */
    public String briefDescription;
    
    /** full description, not just name */
    public String description;
    
    /** Width of each entry (number of words) **/
    int width;
    
    /** List of fields, organized as a map for ease of access.
     *  In this copy, arrays have been unrolled for backwards compatibility. **/
    public final Map<String,RegisterField> fields;

    /** List of fields, organized as a map for ease of access.
     *  In this version, arrayed RegisterFields are only listed once. **/
    public Map<String,RegisterField> fieldsWithArrays;

    /** List of defaults, only 1 if non-array **/
    BigInteger defaults[];
    
    public RegisterStruct(String name, int width, Map<String,RegisterField> fields, BigInteger defaults[]) {
        this.name = name;
        this.width = width;
        this.fieldsWithArrays = fields;
        this.defaults = defaults;
        boolean foundArray = false;
        HashMap<String,RegisterField> expandedFields = new HashMap<String,RegisterField>();
        for (RegisterField field : fieldsWithArrays.values()) {
            if(field.entries > 1) {
                for(int i=field.baseEntry; i<field.entries; i++) {
                    foundArray = true;
                    int pos = field.pos + i * field.len;
                    RegisterField rf = new RegisterField(String.format("%s_%d", field.desc, i), 
                                                         pos, field.len, field.type, field.dataType,
                                                         field.oldVersion, field.typedef,
                                                         field.entries, field.baseEntry);
                    rf.currentEntry = i;
                    rf.implementation = field.implementation;
                    rf.fullDescription = field.fullDescription;
                    expandedFields.put(rf.desc.toLowerCase(), rf);
                }
            } else {
                expandedFields.put(field.desc.toLowerCase(), field);
            }
        }
        if(foundArray) {
            this.fields = expandedFields;
        } else {
            this.fields = fieldsWithArrays; // use same copy to save memory and keep same order
        }
    }

    /** What is it's name? **/
    public String getName() { return name; }

    /** Returns the width (in 32b words) of each entry in this register **/
    public int getEntryWidth() { return width; }

    
    public int getNumFields() { return fields.size(); }

    public int getNumberFieldBits() {
        if(getNumFields() == 0) {
            return 32 * width;
        }
        int numBits = 0; 
        for (RegisterField field : fields.values()) {
            numBits = Math.max(numBits, field.pos + field.len);
        }
        return numBits;
    }
    
    /** Grab a field **/
    public RegisterField getField(String fieldName) {
        return fields.get(fieldName.toLowerCase()); 
    }

    /** Returns an iterator to the fields */
    public Iterator<String> getFieldIter() {
        return fields.keySet().iterator();
    }

    /** Returns the fields in reverse position order or null if there is overlap.
     *  Also inserts reserved fields in gaps. */
    public ArrayList<RegisterField> getSortedFields() {
        // only print old names, not new (see bug 11384)
        return getSortedFields(true, false, true, true);
    }
    public ArrayList<RegisterField> getSortedFields(boolean withArrays) {
        return getSortedFields(true, withArrays, true, true);
    }
    public ArrayList<RegisterField> getSortedFields(boolean oldVersion, boolean withArrays, boolean reversed) {
        return getSortedFields(oldVersion, withArrays, reversed, true);
    }
    public ArrayList<RegisterField> getSortedFields(boolean oldVersion, boolean withArrays, boolean reversed, boolean includeReserved) {
        final TreeMap<Integer,RegisterField> fieldSorted = getFieldsSortedMap(oldVersion, withArrays);
    
        int currentPos = 0;
        int reservedCount = 0;
        ArrayList<RegisterField> result = new ArrayList<RegisterField>();
        for(Iterator<RegisterField> i = fieldSorted.values().iterator(); i.hasNext(); ) {
            RegisterField field = i.next();
            if((currentPos < field.pos) && includeReserved) {
                RegisterField reserved = new RegisterField(String.format("reserved%d", reservedCount++), 
                                                           currentPos, field.pos - currentPos,
                                                           RegisterType.RV, RegisterDataType.UNSIGNED,
                                                           false, null);
                if(reversed) result.add(0, reserved); else result.add(reserved);
            }
            if(reversed) result.add(0, field); else result.add(field);
            if(currentPos > field.pos) {
                // error
                return null;
            }
            // if arrays have already been expanded, use 1 for number of entries
            currentPos = field.pos + (withArrays ? field.entries : 1) * field.len;
        }
        return result;
    }

    /** Returns a map of the fields in position order.
     *  Does not insert reserved fields in gaps or check overlap. */
    public TreeMap<Integer,RegisterField> getFieldsSortedMap() {
        return getFieldsSortedMap(true, false);
    }
    public TreeMap<Integer,RegisterField> getFieldsSortedMap(boolean oldVersion, boolean withArrays) {
        final TreeMap<Integer,RegisterField> fieldSorted = new TreeMap<Integer,RegisterField>();
        Collection<RegisterField> values;
        if(withArrays) {
            values = fieldsWithArrays.values();
        } else {
            values = fields.values();
        }
        for (RegisterField field : values) {
            if(fieldSorted.containsKey(field.pos)) {
                if(oldVersion) {
                    if(fieldSorted.get(field.pos).oldVersion) continue; // keep old
                } else {
                    if(!fieldSorted.get(field.pos).oldVersion) continue; // keep new
                }
            }
            fieldSorted.put(field.pos, field);
        }
        return fieldSorted;
    }

    public boolean isVolatile() {
        for(RegisterField f : fieldsWithArrays.values()) {
            if(f.isVolatile()) return true;
        }
        return false;
    }
    
    /**
     * 
     * @return
     * width of this ReigsterStruct in bits
     */
    public int getBitWidth() {
        int bitWidth = 0;
        for (RegisterField f: this.fields.values()) {
            if (f.pos + f.len > bitWidth) bitWidth = f.pos + f.len;
        }
        return bitWidth;
    }

    /** returns true if there are errors */
    protected boolean checkForErrors() {
        boolean error = false;
        for (Map.Entry<String,RegisterField> entry : fieldsWithArrays.entrySet()) {
            final String desc = entry.getValue().desc;
            if(Util.systemverilogReservedWords.contains(desc)) {
                error = true;
                System.err.println(String.format("ERROR: %s: Field %s is a SystemVerilog reserved word.",
                        name, desc));
            }
        }
        return error;
    }
}
