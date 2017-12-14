package com.fulcrummicro.hw.verification.lib.constants;

import java.util.Collection;
import java.util.Vector;

import org.nfunk.jep.JEP;

import com.fulcrummicro.util.misc.JepMathExpressionParser;

public class ConstantsCell extends ConstantsBase {
    protected Vector<ConstantsConstant> constants;
    protected Vector<ConstantsEnum> enums;
    protected Vector<ConstantsChannel> channels;
    protected String prefix = null; // prefix for all constants in cell in Verilog
    protected String uses = null;
    protected Vector<ConstantsCell> usedCells;
    protected Constants top;
    protected JEP parser = null;
    
    /** DIGESTER ONLY */
    @Deprecated
    public ConstantsCell() {
        constants = new Vector<ConstantsConstant>();
        enums = new Vector<ConstantsEnum>();
        channels =  new Vector<ConstantsChannel>();
        usedCells = new Vector<ConstantsCell>();
    }
    

    /** DIGESTER ONLY */
    @Deprecated
    public void setPrefix(String prefix) {
        this.prefix = prefix;
    }

    /**
     * @return the verilog constants prefix
     */
    public String getPrefix() {
        return prefix == null ? "" : prefix;
    }
    
    public boolean hasPrefix() {
        return prefix != null;
    }

    /** DIGESTER ONLY */
    @Deprecated
    public void setUses(String uses) {
        this.uses = uses;
    }
    
    public Collection<ConstantsCell> getUsedCells() {
        return usedCells;
    }
    
    protected void setConstantsTop(Constants c) {
        top = c;
    }

    /** DIGESTER ONLY */
    @Deprecated
    public void addConstant(ConstantsConstant c) {
        c.setConstantsCell(this);
        constants.add(c);
    }
    /** DIGESTER ONLY */
    @Deprecated
    public void addEnum(ConstantsEnum e) {enums.add(e);}
    /** DIGESTER ONLY */
    @Deprecated
    public void addChannel(ConstantsChannel c) {
        c.setConstantsCell(this);
        channels.add(c);
    }

    public Integer getValue(String valueName) {
        for (ConstantsConstant c : constants) {
            if(c.getName().equals(valueName)) {
                return c.getValue();
            }
        }
        for (ConstantsCell cell : usedCells) {
            Integer i = cell.getValue(valueName);
            if(i != null) {
                return i;
            }
        }
        return null;
    }

    public ConstantsChannelField getChannelField(String channelName, String fieldName) {
        ConstantsChannelField field = getChannelFieldUnchecked(channelName, fieldName);
        if(field == null) {
            throw new NullPointerException(String.format("No field %s found in channel %s.", fieldName, channelName));
        }
        return field;
    }
    
    public ConstantsChannelField getChannelFieldUnchecked(String channelName, String fieldName) {
        for (ConstantsChannel c : channels) {
            if(c.getName().equals(channelName)) {
                ConstantsChannelField f = c.getField(fieldName);
                if(f != null) {
                    return f;
                }
            }
        }
        for (ConstantsCell cell : usedCells) {
            ConstantsChannelField f = cell.getChannelField(channelName, fieldName);
            if(f != null) {
                return f;
            }
        }
        return null;
    }

    protected ConstantsChannel getChannel(String channelName) {
        for (ConstantsChannel c : channels) {
            if(c.getName().equals(channelName)) {
                return c;
            }
        }
        for (ConstantsCell cell : usedCells) {
            ConstantsChannel c = cell.getChannel(channelName);
            if(c != null) {
                return c;
            }
        }
        return null;
    }
    
    public ConstantsEnum getEnum(String enumName) {
        for (ConstantsEnum c : enums) {
            //System.out.println(String.format("%s: %s.%s", enumName, getName(), c.getName()));
            if(c.getName().equals(enumName)) {
                return c;
            }
        }
        for (ConstantsCell cell : usedCells) {
            //System.out.println("Used: " + cell.getName());
            ConstantsEnum c = cell.getEnum(enumName);
            if(c != null) {
                return c;
            }
        }
        return null;
    }
    
    public Integer getEnumValue(String enumName, String valueName) {
        ConstantsEnum c = getEnum(enumName);
        if(c != null) {
            for (ConstantsEnumValue e : c.values) {
                if(e.getName().equals(valueName)) {
                    return e.getEncoding();
                }
            }
        }
        return null;
    }
    
    
    
    @Override
    protected boolean isValid() {
        boolean valid = true;
        for (ConstantsConstant c : constants) {
            valid &= c.isValid();
        }
        for (ConstantsBase c : enums) {
            valid &= c.isValid();
        }
        for (ConstantsBase c : channels) {
            valid &= c.isValid();
        }
        return valid;
    }
    
    private void processUses() {       
        if(uses != null) {
            String cellNames[] = uses.split(",");
            for (String cellName : cellNames) {
                ConstantsCell cell = top.getCell(cellName);
                if(cell != null) {
                    usedCells.add(cell);
                    cell.process();

                    for (Object varName : cell.parser.getSymbolTable().keySet()) {
                        parser.addVariable((String)varName, cell.parser.getSymbolTable().getValue(varName));
                    }
                } else {
                    ConstantsDigester.error(String.format("Unknown uses cell %s in cell %s", cellName, getName()));
                }
            }
        }
    }

    private boolean loopProcess(Collection<? extends ConstantsBase> group, String desc) {
        int maxLoops = 1000;
        boolean changed;
        int loopCount = 0;
        do {
            loopCount++;
            changed = false;
            for (ConstantsBase c : group) {
                changed = c.process(parser) | changed;
            }
        } while (changed && loopCount < maxLoops);

        boolean success = (loopCount < maxLoops);
        if(!success) ConstantsDigester.error(String.format("Cell %s did not complete processing %s successfully!", getName(), desc));
        return success;
    }
    
    /** Returns true on success */
    protected boolean process() {
        if(parser != null) {
            return true;
        }
        parser = new JepMathExpressionParser();
        processUses();
        
        boolean success = true;
        
        success = loopProcess(constants, "constants") && success;
        success = loopProcess(enums, "enums") && success;
        success = loopProcess(channels, "channels") && success;
        
        return success;
    }
    
    @Override
    protected boolean check() {
        boolean success = true;

        for(ConstantsBase c: constants) { success = c.check() && success; } 
        for(ConstantsBase c: enums) { success = c.check() && success; } 
        for(ConstantsBase c: channels) { success = c.check() && success; }
        
        return success;
    }
    
    @Override
    protected boolean process(JEP jep) {
        assert false;
        return false;
    }
    
    @Override
    public String toText() {
        String result = String.format("  ConstantsCell: %s\n", getName());
        for (ConstantsBase c : constants) {result += c.toText();}
        for (ConstantsBase c : enums) {result += c.toText();}
        for (ConstantsBase c : channels) {result += c.toText();}
        return result;
    }
    
    @Override
    public String toCast() {
        String inherits = "";
        for (ConstantsCell cell : usedCells) {
            inherits += " <+ " + cell.getName(); 
        }
        String result = String.format("define %s()() attributes%s {\n", getName(), inherits);
        for (ConstantsBase c : constants) {result += c.toCast();}
        result += "\n";
        for (ConstantsBase c : enums) {result += c.toCast();}
        for (ConstantsBase c : channels) {result += c.toCast();}
        if(!channels.isEmpty()) {
            result += "\n  csp {\n";
            for (ConstantsChannel c : channels) {result += c.toCastStruct();}
            result += "  }\n";
        }
        return result + "}\n\n";
    }

    @Override
    protected String toVerilog() {
        String result = String.format("// begin %s\n", getName());
        for (ConstantsBase c : constants) {result += c.toVerilog();}
        result += "\n";
        for (ConstantsBase c : enums) {result += c.toVerilog();}
        for (ConstantsBase c : channels) {result += c.toVerilog();}
        return result + "// end " + getName() + "\n\n";
    }
}
