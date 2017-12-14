/**
 *
 */
package com.fulcrummicro.hw.verification.chip.mgmt.registers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import com.fulcrummicro.hw.verification.lib.constants.ConstantsEnum;


public class RegisterBase implements Comparable<RegisterBase> {
    /**
     *
     */
    private final RegisterInfo registerInfo;
    String name;
    Integer incrementalAddress;
    Integer size;
    RegisterBase parent;
    String busStopId = null;
    /** Access domains - see bug 20959 */
    Vector<String> accessMethods = null;
    /** atomic width (in 32b words */
    int atomic_width = 1;

    /** Map of base address to Base **/
    final TreeMap<Integer, ArrayList<RegisterBase>> children;
    final ArrayList<RegisterStruct> structs = new ArrayList<RegisterStruct>();
    final ArrayList<ConstantsEnum> enums = new ArrayList<ConstantsEnum>();
    final ArrayList<Register> registers = new ArrayList<Register>();

    protected RegisterBase(RegisterInfo registerInfo) { this(registerInfo, "top level", 0, 0, null); }

    private RegisterBase(RegisterInfo registerInfo, String name, Integer incrementalAddress, Integer size, RegisterBase parent) {
        this.registerInfo = registerInfo;
        this.name = name;
        this.incrementalAddress = incrementalAddress;
        this.size = size;
        this.parent = parent;
        children = new TreeMap<Integer,ArrayList<RegisterBase>>();
    }

    public Integer getAbsoluteAddress() {
        Integer absolute = incrementalAddress;
        if(parent != null) {
            absolute += parent.getAbsoluteAddress();
        }
        return absolute;
    }

    public String getBusStopId() {
        return busStopId;
    }

    public String getFullName() {
        if(parent != null)
            return name + "_BASE";
        else
            return name;
    }

    public ConstantsEnum getEnum(String name) {
        for (ConstantsEnum ce : enums) {
            if (ce.getName().equals(name)) {
                return ce;
            }
        }
        return null;
    }

    public List<ConstantsEnum> getEnums() {
        return this.enums;
    }

    public Integer getIncrementalAddress() {
        return this.incrementalAddress;
    }

    public String getName() {
        return this.name;
    }

    public RegisterBase getParent() {
        return this.parent;
    }

    public Integer getSize() {
        if(size == 0)
            return 1;
        else
            return size;
    }

    public List<RegisterStruct> getStructs() {
        return this.structs;
    }

    /** Returns the atomic width (in words) */
    public int getAtomicWidth() { return atomic_width; }
    
    protected RegisterBase add(String name, Integer incrementalAddress, Integer size) {
        RegisterBase child = new RegisterBase(this.registerInfo, name, incrementalAddress, size, this);
        if(children.get(child.getAbsoluteAddress()) == null) {
            ArrayList<RegisterBase> baseList = new ArrayList<RegisterBase>();
            children.put(child.getAbsoluteAddress(),baseList);
        }

        children.get(child.getAbsoluteAddress()).add(child);
        return child;
    }

    protected void add(Register r) {
        r.base = this;
        registers.add(r);
    }

    /** Call getBases(null) on the top to get all of them */
    public ArrayList<RegisterBase> getBases(ArrayList<RegisterBase> list) {
        if(list == null) {
            list = new ArrayList<RegisterBase>();
        }

        if(parent != null) {
            list.add(this);
        }

        Iterator<ArrayList<RegisterBase>> i = children.values().iterator();
        while(i.hasNext()) {
            for(Iterator<RegisterBase> j = i.next().iterator(); j.hasNext();) {
                j.next().getBases(list);
            }
        }

        return list;
    }

    public ArrayList<Register> getRegisters() {
        return RegisterInfo.getSortedRegisterList(registers);
    }

    public ArrayList<Register> getRegisters(String key, String value) {
        ArrayList<Register> result = new ArrayList<Register>();
        for (Register register : registers) {
            String regValue = register.implementation.get(key);
            if(regValue == null) {
                if(value == null) result.add(register);
            } else if(regValue.equals(value)) {
                result.add(register);
            }
        }
        return RegisterInfo.getSortedRegisterList(result);
    }

    public Collection<String> getGroups() {
        HashSet<String> groups = new HashSet<String>();
        for (Register r : registers) {
            groups.add(r.implementation.get("group"));
        }
        groups.remove(null);
        ArrayList<String> glist = new ArrayList<String>(groups);
        // always include top, even if empty, as last element
        glist.add(null);
        return glist;
    }

    public void checkForErrors(RegisterInfo regInfo) {
        if(parent != null ) {
            if(size == 0) {
                System.err.println("WARNING: size is 0 for base " + getFullName());
            } else {
                if(!this.registerInfo.verbose && parent.name.equals("PORT_STATS")) return;

                if(getAbsoluteAddress() % size != 0) {
                    System.err.println("WARNING: Address 0x" + Integer.toHexString(getAbsoluteAddress()) + " of " +
                        getFullName() + " not a multiple of size 0x" + Integer.toHexString(size));
                }

                Iterator<ArrayList<RegisterBase>> i = parent.children.values().iterator();
                while(i.hasNext()) {
                    for(Iterator<RegisterBase> j = i.next().iterator(); j.hasNext(); ) {
                        RegisterBase sibling = j.next();
                        if(sibling == this) break;
                        // if start of sibling is enclosed
                        if((sibling.getAbsoluteAddress() >= getAbsoluteAddress()) &&
                            (sibling.getAbsoluteAddress() < getAbsoluteAddress() + size)) {
                            // if end of sibling is also enclosed
                            if((sibling.getAbsoluteAddress() + sibling.size >= getAbsoluteAddress()) &&
                                (sibling.getAbsoluteAddress() + sibling.size <= getAbsoluteAddress() + size)) {
                                System.err.print("WARNING: " + getFullName() + " encloses " + sibling.getFullName());
                            } else {
                                System.err.print("WARNING: " + getFullName() + " extends into " + sibling.getFullName());
                            }
                            System.err.println("    (0x" + Integer.toHexString(getAbsoluteAddress()) + "-0x" +
                                Integer.toHexString(getAbsoluteAddress() + size - 1) + "), (0x" +
                                Integer.toHexString(sibling.getAbsoluteAddress()) + "-0x" +
                                Integer.toHexString(sibling.getAbsoluteAddress() + sibling.size - 1) + ")");
                        }
                    }
                }
            }
        }

        Iterator<ArrayList<RegisterBase>> i = children.values().iterator();
        while(i.hasNext()) {
            for(Iterator<RegisterBase> j = i.next().iterator(); j.hasNext(); ) {
                j.next().checkForErrors(regInfo);
            }
        }
    }

    public void printAddressMap() {printAddressMap(false);}
    public void printAddressMap(boolean showRegs) {printAddressMap("", showRegs);}

    private void printAddressMap(String prefix, boolean showRegs) {
        if(parent != null) {
            prefix += "* ";
            System.out.println(prefix + " 0x" + Integer.toHexString(getAbsoluteAddress()) + "    " + getFullName());
        }

        if(showRegs) {
            TreeSet<Register> regs = new TreeSet<Register>(new Comparator<Register>() {
                public int compare(Register r1, Register r2) {
                    return r1.getAddressSafe() - r2.getAddressSafe();
                }
                @SuppressWarnings({"all"}) // equals is required by Comparator but never used
                public boolean equals(Register r1, Register r2) {return compare(r1,r2) == 0;}
            });
            regs.addAll(this.registerInfo.regMap.values());
            for (Register register : regs) {
                if(register.base == this) {
                    System.out.println(prefix + "    # 0x" + Integer.toHexString(register.getAddressSafe()) + "   " + register.name);
                    if(register.getTotalSize() != 0) {
                        System.out.println(prefix + "      0x" +
                            Integer.toHexString(register.getAddressSafe() + register.getTotalSize()));
                    }
                }
            }
        }

        Iterator<ArrayList<RegisterBase>> i = children.values().iterator();
        while(i.hasNext()) {
            for(Iterator<RegisterBase> j = i.next().iterator(); j.hasNext(); ) {
                RegisterBase child = j.next();
                child.printAddressMap(prefix, showRegs);
            }
        }
        if(parent != null) {
            System.out.print(prefix + " 0x" + Integer.toHexString(getAbsoluteAddress() + getSize()-1));
            if(children.values().size()>0) {
                System.out.println("    [" + getFullName() + "]");
            } else {
                System.out.println();
            }
        }
    }

    /* (non-Javadoc)
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    public int compareTo(RegisterBase o) {
        return this.getAbsoluteAddress().compareTo(o.getAbsoluteAddress());
    }

    /**
     *
     * @param structName name of struct to be looking for, case insensitive
     * @return
     * null if not found
     */
    public RegisterStruct getStruct(String structName) {
        RegisterStruct s = null;
        for (RegisterStruct c: this.structs) {
            if (c.name.toUpperCase().compareTo(structName.toUpperCase()) == 0) {
                s = c;
            }
        }
        return s;
    }

}
