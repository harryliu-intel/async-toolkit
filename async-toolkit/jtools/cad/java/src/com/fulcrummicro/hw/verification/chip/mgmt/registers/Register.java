package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import com.fulcrummicro.hw.verification.lib.bitarray.LittleEndianBitArray;
import com.fulcrummicro.hw.verification.lib.constants.ConstantsEnumValue;
import com.fulcrummicro.hw.verification.lib.testcase.ResetType;
import com.fulcrummicro.hw.verification.lib.testcase.ResetType.ResetDomain;
import com.fulcrummicro.hw.verification.util.common.ByteUtils;
import com.fulcrummicro.util.misc.Utility;

/** Register definition and properties class, with variants **/

public class Register extends RegisterStruct {
    private static final String validNameRegEx = "[a-zA-Z][a-zA-Z0-9_]*";
    /**
     *
     */
    private final RegisterInfo registerInfo;

    /** Number of entries **/
    int entries;
    int entries_1 = 1;
    int entries_2 = 1;

    /** Width of each entry (number of bits)
     *  If non-zero, defines the width in bits of the register for getNumberFieldBits()
     *  which is used to determine the defaults width.
     *  Used for specifying a larger number of bits for the case that there is a reserved
     *  field at the top that we don't want to put in the XML.
     */
    int bits;

    /** Has the register been implemented yet? **/
    boolean implemented;

    /** Address of entry#0 **/
    int address = -1;

    /** Extra Spacing to next entry, 0 for contiguous entries **/
    int extra_spacing = 0;
    int extra_spacing_1 = 0;
    int extra_spacing_2 = 0;

    /** List of domains that this register belongs to **/
    ResetDomain[] domains;

    /** Access domains - see bug 20959 */
    Vector<String> accessMethods = null;
    
    /** first valid entry **/
    int base_entry = 0;
    int base_entry_1 = 0;
    int base_entry_2 = 0;

    /** setAddress' addressing unit (32b words) **/
    int address_unit = 1;

    boolean atomic;
    /** atomic width (in 32b words */
    int atomic_width = 0;
    boolean oldName = false; // true if using old name for register in transitional period
    boolean suppressStructGeneration = false;
    String alias = null;
    Integer structId = null;

    HashMap<String, String> implementation = null;

    RegisterBase base;

    Register(RegisterInfo registerInfo, String n, int e, int w, boolean i,
        BigInteger def[], Map<String,RegisterField> f, int es, ResetDomain[] d) {
        super(n, w, f, def);
        this.registerInfo = registerInfo;
        entries = e;
        implemented = i;
        extra_spacing = es;
        atomic = false;
        base = new RegisterBase(this.registerInfo);
        if (d!=null)
            domains = d.clone();
        else {
            domains = new ResetDomain[1];
            domains[0] = ResetDomain.CHIP;
        }

    }

    /** Returns the default, includes shortcut wrapper for first one **/
    public BigInteger getDefault(int index) {
        assert (defaults != null) && (index < defaults.length) :
            "Error reading default[" + index + "] for register " + name;
        return defaults[index];
    }
    public int getDefault(int index, int word) {
        byte[] byteDefault = getDefault(index).toByteArray();
        byte[] correctedByteDefault = new byte[width*4];
        for(int i=0;i<correctedByteDefault.length; i++) correctedByteDefault[i] = 0;
        for(int i=0; i<correctedByteDefault.length && i<byteDefault.length; i++) {
            correctedByteDefault[i] = byteDefault[byteDefault.length-i-1];
        }
        int[] intDefault = ByteUtils.btoia(correctedByteDefault, false);
        return intDefault[word];
    }
    public int getNumDefaults() { return defaults.length; }

    public int getNumberFieldBits() {
        if(bits != 0) return bits;
        return super.getNumberFieldBits();
    }

    public LittleEndianBitArray getUnusedMask() {
        int fieldBits = getNumberFieldBits();
        int implBits = getImplementationWidth();
        if(implBits > fieldBits) {
            LittleEndianBitArray mask = new LittleEndianBitArray(implBits);
            mask.set(getTypeMask(null).invert());
            mask.setBit(fieldBits, implBits - fieldBits);
            return mask;
        } else {
            return getTypeMask(null).invert();
        }
    }

    public LittleEndianBitArray getUnusedOrReservedMask() {
        return getUnusedMask().or(getTypeMask(RegisterType.RV));
    }

    public LittleEndianBitArray getTypeMask(RegisterType type) {
        // LittleEndianBitArray mask = new LittleEndianBitArray(getNumberFieldBits());
        LittleEndianBitArray mask = new LittleEndianBitArray(getImplementationWidth());

        for (RegisterField field : fields.values()) {
            if((type == null) || field.type.equals(type)) {
                mask.setBit(field.pos, field.len);
            }
        }
        return mask;
    }

    /** bits set for RegisterType, expanded to 32*width bits */
    public LittleEndianBitArray getTypeMaskExpanded(RegisterType type) {
        LittleEndianBitArray mask = new LittleEndianBitArray(32*width);
        mask.set(getTypeMask(type));
        return mask;
    }

    public RegisterBase getRegisterBase() {
        return base;
    }

    public String getBaseMaskString(int nbits) {
        /*
         * BASE_ADDR_MASK === ~(2^{log_2{size(array)}}-1)
         *
         * Applies if:
         *   BASE_ADDR == BASE_ADDR & MASK
         *
         * and
         *
         *   BASE_ADDR == (BASE_ADDR+size(array)-1) & MASK
         *
         */

        int baseAddress = getAddress();  // absolute base
        String baseStr = Integer.toBinaryString(baseAddress);
        char[] baseChars = baseStr.toCharArray();
        int changedMask = getAddressChangedMask(true);
        LittleEndianBitArray ba = new LittleEndianBitArray(nbits);
        ba.set(changedMask);
        for (Integer set : ba) {
            baseChars[baseStr.length()-set-1] = 'x';
        }
        
        String baseMaskStr = new String(baseChars);
        
        // pad beginning with 0
        while(baseMaskStr.length() < nbits) {
            baseMaskStr = "0" + baseMaskStr;
        }
        
        return baseMaskStr;
    }

    public String getBaseName() { return base.getFullName(); }

    public String getAlias() { return alias; }

    /** Do we know what this register's address is? **/
    public boolean hasAddress() { return address != -1; }

    /** Returns whether this register is implemented or not **/
    public boolean isImplemented() { return implemented; }

    /** Returns whether this register is atomic or not **/
    public boolean isAtomic() { return atomic; }
    /** Returns the atomic width (in words) */
    public int getAtomicWidth() { return atomic_width; }

    public boolean isOldName() { return oldName; }

    /** Returns the number of entries in this register **/
    public int getNumEntries() { return entries; }
    public int getNumEntries1() { return entries_1; }
    public int getNumEntries2() { return entries_2; }

    /** Returns the first valid entry */
    public int getBaseEntry() { return base_entry; }
    public int getBaseEntry1() { return base_entry_1; }
    public int getBaseEntry2() { return base_entry_2; }

    /** returns the address offset from the XML */
    public int getAddressOffset() { return address; }

    /** Returns the address of this single-entry register **/
    @Deprecated
    public int getAddressSafe() {
        try {
            return getAddress();
        } catch(Exception e) {
            return -1;
        }
    }

    /** Returns the address of this single-entry register **/
    public int getAddress() {
        return doGetAddress(base_entry_2, base_entry_1, base_entry, 0);
    }

    /** Returns the address of entry number 'entry' **/
    public int getAddress(int entry) throws UnknownAddressException,
                                     IndexOutOfBoundsException {
        return getAddress(base_entry_2, base_entry_1, entry, 0);
    }

    /** Returns the address of word number 'word' of entry 'entry' **/
    public int getAddress(int entry, int word) throws UnknownAddressException,
                                               IndexOutOfBoundsException {
        return getAddress(base_entry_2, base_entry_1, entry, word);
    }

    /** Returns the address of word number 'word' of entries 'entry0' and 'entry1' **/
    public int getAddress(int entry1, int entry0, int word)
        throws UnknownAddressException, IndexOutOfBoundsException {
        return getAddress(base_entry_2, entry1, entry0, word);
    }

    /** Returns the address of word number 'word' of entries 'entry0', 'entry1', and 'entry2' **/
    public int getAddress(int entry2, int entry1, int entry0, int word)
        throws UnknownAddressException, IndexOutOfBoundsException {
        if (address == -1)
            throw new UnknownAddressException("Address of register "
                                              +name+" is unknown");
        if (entry0 >= entries || entry1 >= entries_1 || entry2 >= entries_2 ||
            word >= width ||
            entry0 < base_entry || entry1 < base_entry_1 || entry2 < base_entry_2)
            throw new IndexOutOfBoundsException("Entry2 "+entry2+", entry1 "+entry1+", entry0 "+entry0+
                ", word "+word+" is out of bounds for register "+
                name+".");
        /*System.out.println("base = 0x" + Integer.toHexString(getUnitBaseAddress()) +
            " address = 0x" + Integer.toHexString(address) +
            " * " + getAddressUnit() + " + i * 0x" + Integer.toHexString(getEntrySpace())
            );*/
        return doGetAddress(entry2, entry1, entry0, word);
    }

    private int doGetAddress(int entry2, int entry1, int entry0, int word) {
        return getUnitBaseAddress() + address * getAddressUnit() +
            getEntrySpace() * (entry0 - base_entry) +
            getEntrySpace1() * (entry1 - base_entry_1) +
            getEntrySpace2() * (entry2 - base_entry_2) +
            word;
    }

    public int getMaxAddress() {
        return doGetAddress(entries_2-1, entries_1-1, entries-1, width-1);
    }

    public int getTotalSize() {
        return doGetAddress(entries_2-1, entries_1-1, entries-1, width-1)
            - doGetAddress(base_entry_2, base_entry_1, base_entry, 0);
    }

    /** Base address for relative address offset registers **/
    public int getUnitBaseAddress() {
        return base.getAbsoluteAddress();
    }

    /** setAddress' addressing unit (words) **/
    public int getAddressUnit() { return address_unit; }

    public boolean contains(int addr) {
        return getOffsetInfo(addr) != null;
    }

    /** Returns name in the form FFU_SLICE_ACTION_ROUTE[0..27] [0..1023] */
    public String getSpecString() {
        String format = "[%d..%d]";
        String output = getName();
        if(getNumEntries2()>1) output += String.format(format, base_entry_2, entries_2 - 1);
        if(getNumEntries1()>1) output += String.format(format, base_entry_1, entries_1 - 1);
        if(getNumEntries()>1)  output += String.format(format, base_entry, entries - 1);
        return output;
    }

    /** Prints in the form (entry1, entry0)[word] */
    public String getOffsetString(int addr) {
        StringBuilder buffer = new StringBuilder();
        int[] info = this.getOffsetInfo(addr);

        if (info != null) {
            if (this.getNumEntries() > 1) {
                buffer.append("(");
                if (this.getNumEntries2() > 1)
                    buffer.append(String.format("0x%x,", info[OffsetInfo.ENTRY2]));
                if (this.getNumEntries1() > 1)
                    buffer.append(String.format("0x%x,", info[OffsetInfo.ENTRY1]));
                buffer.append(String.format("0x%x)", info[OffsetInfo.ENTRY0]));
            }
            if (this.getEntryWidth() > 1)
                buffer.append(String.format("[0x%x]", info[OffsetInfo.WORD]));
        }
        return buffer.toString();
    }

    public static class OffsetInfo {public static int ENTRY2=0, ENTRY1=1, ENTRY0=2, WORD=3; }

    /** returns int[] {entry2, entry1, entry0, word} */
    public int[] getOffsetInfo(int addr) {
        try {
            int addrSave = addr;
            int k = 0, j = 0, i = 0, w = 0;
            addr -= getUnitBaseAddress();
            addr -= address*getAddressUnit();
            w = addr%getEntrySpace();
            // addr is now relative to the the base address
            if (getNumEntries2() != 1) {
                k = addr/getEntrySpace2() + getBaseEntry2();
                addr%=getEntrySpace2();
                j = addr/getEntrySpace1() + getBaseEntry1();
                addr%=getEntrySpace1();
                i = addr/getEntrySpace() + getBaseEntry();
            } else if (getNumEntries1()!= 1) {
                j = addr/getEntrySpace1() + getBaseEntry1();
                addr%=getEntrySpace1();
                i = addr/getEntrySpace() + getBaseEntry();
            } else if (getNumEntries() != 1) {
                i = addr/getEntrySpace() + getBaseEntry();
            }
            if(getAddress(k,j,i,w) == addrSave)
                return new int[]{k,j,i,w};
        } catch(Exception e) {}

        return null;
    }

    public class IndexInfo {
        public final int wordIdx;
        public final int entry0Idx;
        public final int entry0UpperIdx;
        public final int entry0Bits;
        public final int entry1Idx;
        public final int entry1UpperIdx;
        public final int entry1Bits;
        public final int entry2Idx;
        public final int entry2UpperIdx;
        public final int entry2Bits;
        public final int totalEntryBits;
        public IndexInfo(Register reg) {
            wordIdx = Util.log2(reg.getEntryWidth()) - 1;
            entry0Idx = Util.log2(reg.getEntrySpace());
            entry0UpperIdx = Util.log2(reg.getEntrySpace() * reg.getNumEntries()) - 1;
            entry0Bits = entry0UpperIdx - entry0Idx + 1;
            entry1Idx = Util.log2(reg.getEntrySpace1());
            entry1UpperIdx = Util.log2(reg.getEntrySpace1() * reg.getNumEntries1()) - 1;
            entry1Bits = entry1UpperIdx - entry1Idx + 1;
            entry2Idx = Util.log2(reg.getEntrySpace2());
            entry2UpperIdx = Util.log2(reg.getEntrySpace2() * reg.getNumEntries2()) - 1;
            entry2Bits = entry2UpperIdx - entry2Idx + 1;
            totalEntryBits = entry2Bits + entry1Bits + entry0Bits;
        }
    }
    public IndexInfo getIndexInfo() {
        return new IndexInfo(this);
    }

    /** Originally in CastCommon.jamon. Now generates reg_BASE_MASK constant */
    public int getAddressMatchMask() {
        return getAddressMatchMask(true);
    }
    public int getAddressMatchMaskExcludingWordBits() {
        return getAddressMatchMask(false);
    }
    private int getAddressMatchMask(boolean includeWordBits) {
        return ~getAddressChangedMask(includeWordBits);
    }
    private int getAddressChangedMask(boolean includeWordBits) {
        int addrMask = 0; //ignored address bits
        Register.IndexInfo indexInfo = getIndexInfo();
        if(includeWordBits) {
            for (int i=0; i<=indexInfo.wordIdx; i++) {
                addrMask |= (1<<i);
            }
        }
        for (int i=indexInfo.entry0Idx; i<=indexInfo.entry0UpperIdx; i++) {
            addrMask |= (1<<i);
        }
        for (int i=indexInfo.entry1Idx; i<=indexInfo.entry1UpperIdx; i++) {
            addrMask |= (1<<i);
        }
        for (int i=indexInfo.entry2Idx; i<=indexInfo.entry2UpperIdx; i++) {
            addrMask |= (1<<i);
        }
        return addrMask;
    }

    public Map<String, String> getImplementation() {
        return this.implementation;
    }

    public int getImplementationWidth() {
        int w = getNumberFieldBits(); //Math.min(getAtomicWidth()*32, getNumberFieldBits());

        String sramWidth = implementation.get("width");
        if(sramWidth != null) try {
            int sw = Integer.parseInt(sramWidth);
            w = Math.max(w, sw);
        } catch(NumberFormatException e) {
            System.err.println("ERROR: invalid 'width' implementation field on " + name + ": " + sramWidth);
        }
        if(w < getNumberFieldBits()) {
            System.err.println("ERROR: implementation width " + w + " < number field bits " + getNumberFieldBits() + " on " + name);
        }
        return w;
    }

    public int getSramWidth() {
        int w = getImplementationWidth();

        String errorDetection = implementation.get("error-detection");
        if((errorDetection != null) && !errorDetection.equals("none")) {
            String[] ed = errorDetection.split(":");
            if(ed.length == 2) {
                try {
                    int edw = Integer.parseInt(ed[1]);
                    w += edw;
                } catch (NumberFormatException e) {
                    System.err.println("ERROR: invalid 'error-detection' implementation field on " + name + ": " + errorDetection);
                }
            } else {
                System.err.println("ERROR: invalid 'error-detection' implementation field on " + name + ": " + errorDetection);
            }
        }
        return w;
    }

    public Integer getStructId() {
        return this.structId;
    }

    public boolean isImplementationTrue(String attribute) {
        return (implementation.get(attribute) != null) &&  implementation.get(attribute).toLowerCase().equals("true");
    }

    public boolean isStructure() {
        return (getNumFields() > 1) || isImplementationTrue("structure");
    }

    public boolean isShadow() {
        return isImplementationTrue("shadow");
    }

    /** Spacing between entries (words) **/
    /** Spacing is normally contiguous, which means each register is
     * 'width' words apart.  For non-contiguous registers, put the
     * number of 'extra' words needed into the 'extra_spacing' field.
     **/
    public int getEntrySpace() { return width + extra_spacing; }
    public int getEntrySpace1() { return width + extra_spacing_1; }
    public int getEntrySpace2() { return width + extra_spacing_2; }

    /**
     *
     * @return
     * number of dimensions with > 1 entries
     */
    public int getNumDimensions() {
        if (this.entries_2 > 1) return 3;
        else if (this.entries_1 > 1) return 2;
        else return 1;
    }

    /**
     * @return
     * the reset type that specifies the reset domain of this register
     */
    public ResetType getResetType(int entry) {
        return new ResetType(domains, entry);
    }
    
    public Vector<String> getAccessMethods() {
        if(accessMethods == null) return base.accessMethods;
        return accessMethods;
    }

    public void setImplemented(boolean implemented) {
        this.implemented = implemented;
    }

    /** Print info out nicely **/
    public String toString() {
        String s = name;
        for (int i=0; i<40-name.length(); i++) s+=" ";
        s += entries + " x " + width;
        int l = s.length();
        for (int i=0; i<50-l; i++) s+=" ";
        if (address != -1) {
            try {s += " @ " + getAddress();}
            catch(Exception e) {s += " @ (unknown address)";}
        }
        else s += " @ (unknown address)";
        return s;
    }

    /** returns true if there are errors */
    private boolean checkDimForErrors(int dim, int entries, int extra_spacing) {
        boolean failed = false;
        if(entries > 1) {
            if(extra_spacing < 0) {
                System.err.println("    ERROR: Stride (" + (extra_spacing + width) +
                        ") < width (" + width + ") on dim " + dim + " for reg " + name);
                failed = true;
            } else if(extra_spacing != 0) {
                if(getAddressSafe() % (extra_spacing + width) != 0) {
                    if(this.registerInfo.verbose) {
                        System.err.println("    WARNING: address base 0x" + Integer.toHexString(getAddressSafe()) +
                            " not a multiple of stride 0x" + Integer.toHexString(extra_spacing + width) +
                            " for register " + name + " in dim " + dim);
                    }
                }
                int shifted_stride = extra_spacing + width;
                while(shifted_stride != 1) {
                    if((shifted_stride & 1) == 0) {
                        shifted_stride = shifted_stride>>1;
                    } else {
                        System.err.println("    WARNING: stride of " + (extra_spacing + width) +
                            " not a power of 2 for register " + name + " in dim " + dim);
                        break;
                    }
                }
            }
        }
        return failed;
    }

    protected boolean checkFieldsForErrors() {
        boolean failed = false;
        if(fields.isEmpty()) {
            System.err.println("    ERROR: no fields for register " + name);
            failed = true;
        }
        for (RegisterField field : fields.values()) {
            if(!field.desc.matches(validNameRegEx)) {
                System.err.println("    WARNING: invalid field name in register " +
                    name + ": \"" + field.desc + "\"");
                failed = true;
            }

            if(field.pos + field.len > 32 * width) {
                System.err.println("    ERROR*: " + name + "." + field.desc +
                    "extends beyond end of register width");
                failed = true;
            }

            int maxFieldValues = (int) Math.pow(2,field.len);
            if(field.typedef != null) {
                if(maxFieldValues < field.typedef.values.size()) {
                    System.err.println(String.format("ERROR: Field length of %s too small in register %s [2^%d (%d) < %d]",
                                                     field.desc, name, field.len, maxFieldValues, field.typedef.values.size()));
                    failed = true;
                } else { // should fit, but may be misnumbered
                    for (ConstantsEnumValue value : field.typedef.values) {
                        if(value.getEncoding() >= maxFieldValues) {
                            System.err.println(String.format("ERROR: Invalid encoding of %s in register %s, field %s [%d should be < %d]",
                                                             value.getName(), name, field.desc, value.getEncoding(), maxFieldValues));
                        }
                    }
                }
            } // field.typedef == null
        }
        return failed;
    }

    /** returns true if there are errors */
    @Override
    public boolean checkForErrors() {
        boolean failed = false;
        if(!name.matches(validNameRegEx)) {
            System.err.println("    ERROR: invalid name for register: \"" + name + "\"");
            failed = true;
        }
        failed |= checkDimForErrors(0, entries, extra_spacing);
        failed |= checkDimForErrors(1, entries_1, extra_spacing_1);
        failed |= checkDimForErrors(2, entries_2, extra_spacing_2);
        try {
            int maxAddr = getAddress(entries_2-1, entries_1-1, entries-1, width-1);
            int maxBase = base.getAbsoluteAddress() + base.getSize() - 1;
            if(getAddress() > maxBase) {
                System.err.println("    ERROR: register " + name +
                    " does not start within parent registerSet " + base.getFullName() +
                    " (0x" + Integer.toHexString(getAddress()) + " > 0x" +
                    Integer.toHexString(maxBase) + ")");
                failed = true;
            } else if(maxAddr > maxBase) {
                System.err.println("    ERROR: register " + name +
                    " extends beyond end of registerSet " + base.getFullName() +
                    " (0x" + Integer.toHexString(maxAddr) + " > 0x" +
                    Integer.toHexString(maxBase) + ")");
                failed = true;
            }
        } catch (Exception e) {
            System.err.println("    ERROR: " + Utility.exceptionString(e));
            failed = true;
        }
        
        if(base_entry!=0 || base_entry_1!=0 || base_entry_2!=0) {
            System.err.println("Non-zero base entry currently not supported. Register: "+name);
            failed=true;
        }

        //check that none of the ignored bits are set, indicating non-aligned address (moved from jamon)
        int regBaseAddr = getUnitBaseAddress() + getAddressOffset();
        int addrMask = ~ getAddressMatchMask();
        if (((regBaseAddr & addrMask) != 0) & !implementation.containsKey("allow-index-offset") ) {
            System.out.printf("ERROR %s indexes do not start at 0, addr = 0x%x, index mask = 0x%x\n",getName(), regBaseAddr, addrMask);
            failed = true;
        }

        failed |= checkFieldsForErrors();
        failed |= super.checkForErrors();

        return failed;
    }

    public boolean checkForWarnings() {
        boolean failed = false;
        for (RegisterField field : fields.values()) {
            if(field.typedef != null) {
                failed |= field.typedef.checkForDuplicateValues(name +"."+ field.desc);
            }
        }
        return failed;
    }
}
