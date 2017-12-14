package com.fulcrummicro.hw.verification.lib.constants;

import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import org.nfunk.jep.JEP;

import com.fulcrummicro.hw.verification.lib.constants.ConstantsChannelField.ConstantsChannelFieldType;
import com.fulcrummicro.util.misc.Utility;

public class ConstantsChannel extends ConstantsBase {

    final TreeSet<ConstantsChannelField> fields;
    private final Set<ConstantsChannelField> unsortedFields;
    private Integer width = null;
    private String widthExpression = null;
    private ConstantsChannelFieldType type = null;
    private String typeString = null;
    private ConstantsChannel extendsChannel = null;
    private String extendsChannelString = null;
    private ConstantsCell constantsCell = null;
    private boolean processed = false;
    
    /** DIGESTER ONLY */
    @Deprecated
    public ConstantsChannel() {
        fields = new TreeSet<ConstantsChannelField>(new Comparator<ConstantsChannelField>() {
            public int compare(ConstantsChannelField o1,
                               ConstantsChannelField o2) {
                /* We want to sort first by position, then width. 
                 * We must not return 0 unless they are the same or we will lose fields. */
                if(o1.getPosition() != o2.getPosition()) {
                    return o1.getPosition() - o2.getPosition();
                } else if(o1.getWidth() != o2.getWidth()){
                    return o1.getWidth() - o2.getWidth();
                } else {
                    return o1.getName().compareTo(o2.getName());
                }
            }
        });
        unsortedFields = new LinkedHashSet<ConstantsChannelField>();
    }

    /** DIGESTER ONLY */
    @Deprecated
    public void setWidthExpression(String width) {
        this.widthExpression = width;
    }

    /** DIGESTER ONLY */
    @Deprecated
    public void addField(ConstantsChannelField field) {
        field.setConstantsChannel(this);
        unsortedFields.add(field);
    }

    /** DIGESTER ONLY
     * @param typeString the typeString to set
     */
    @Deprecated
    public void setTypeString(String typeString) {
        type = ConstantsChannelFieldType.lookup(typeString);
        this.typeString = typeString;
    }

    /** DIGESTER ONLY
     * @param extendsChannelString the extendsChannelString to set
     */
    @Deprecated
    public void setExtendsChannelString(String extendsChannelString) {
        this.extendsChannelString = extendsChannelString;
    }
    
    protected void setConstantsCell(ConstantsCell cell) {this.constantsCell = cell;}
    protected ConstantsCell getConstantsCell() {return constantsCell;}

    /**
     * @return the typeString
     */
    public String getTypeString() {
        return typeString;
    }
    
    public String getExtendsChannelString() { return extendsChannelString; }
    
    public ConstantsChannelFieldType getType() {
        return type;
    }
    
    public int getWidth() {
        return width;
    }
    
    public boolean isSingleDirection() {
        boolean s = true;
        for (ConstantsChannelField f : fields) {
            if(!f.getDirection().equals("+")) {
                s = false;
            }
        }
        return s;
    }
    
    public ConstantsChannelField getField(String fieldName) {
        for (ConstantsChannelField f : fields) {
            if(f.getName().equals(fieldName)) {
                return f;
            }
        }
        return null;
    }
    
    protected int nextPosition() {
        if(fields.isEmpty()) return 0;
        ConstantsChannelField field = fields.last();
        return field.getPosition() + field.getFullWidth();
    }
    
    private ConstantsChannelField createReservedField(int reservedCount, int pos, int fieldWidth) {
        ConstantsChannelField reserved = new ConstantsChannelField(ConstantsChannelFieldType.RESERVED);
        reserved.setConstantsChannel(this);
        reserved.setName(String.format("reserved%d", reservedCount));
        reserved.setPosition(pos);
        reserved.setWidth(fieldWidth);
        return reserved;
    }
    
    private void addReservedFields() {
        int currentPos = 0;
        int reservedCount = 0;
        Set<ConstantsChannelField> fieldsToAdd = new HashSet<ConstantsChannelField>();
        for (ConstantsChannelField field : fields) {
            if(currentPos < field.getPosition()) {
                fieldsToAdd.add(createReservedField(reservedCount++, 
                                                    currentPos, 
                                                    field.getPosition() - currentPos));
            }
            if(currentPos > field.getPosition()) {
                Vector<String> names = new Vector<String>();
                for (ConstantsChannelField overlapField : fields) {
                    if((field == overlapField) || (field.allowOverlap() && overlapField.allowOverlap())){
                        continue;
                    }
                    int oa = overlapField.getPosition();
                    int ob = oa + overlapField.getFullWidth();
                    int fa = field.getPosition();
                    int fb = fa + field.getFullWidth();
                    if(((fa >= oa) && (fa < ob)) || ((oa >= fa) && (oa < fb))) {
                        names.add(overlapField.getName());
                    }
                }
                if(!names.isEmpty()) {
                    ConstantsDigester.warning(String.format("overlapping field %s in channel %s: previous %d > field %d: %s", 
                                                          field.getName(), getName(), currentPos, field.getPosition(), names));
                }
            }
            currentPos = Math.max(field.getPosition() + field.getFullWidth(), currentPos);
        }
        if(width == null) {
            width = currentPos;
        } else if(width > currentPos) {
            fields.add(createReservedField(reservedCount++, 
                                           currentPos,
                                           width - currentPos));
        } else if(width < currentPos) {
            ConstantsDigester.error(String.format("channel %s extends beyond specified width %d to %d",
                                                  getName(), width, currentPos));
        }
        fields.addAll(fieldsToAdd);
    }

    @Override
    protected boolean isValid() {
        boolean valid = unsortedFields.isEmpty();
        for (ConstantsChannelField field : fields) {
            valid &= field.isValid();
        }
        return valid;
    }
    
    protected boolean process(JEP parser) {
        if(processed) return false;
        processed = true;
        if(extendsChannelString != null) {
            extendsChannel = constantsCell.getChannel(extendsChannelString);
            extendsChannel.process(parser);
            for (ConstantsChannelField field : extendsChannel.fields) {
                if(field.getType() != ConstantsChannelField.ConstantsChannelFieldType.RESERVED) {
                    ConstantsChannelField fieldClone = field.clone();
                    fieldClone.setConstantsChannel(this);
                    fields.add(fieldClone);
                }
            }
        }
        
        if(widthExpression != null) {
            width = processInt(parser, widthExpression);
        }
                           
        for (ConstantsChannelField field : unsortedFields) {
            field.process(parser);
            fields.add(field);
        }
        for (ConstantsChannelField field : fields) {
            unsortedFields.remove(field);
        }
        addReservedFields();
        return false; // no changes
    }
    
    protected boolean hasOverlapingFields() {
        boolean overlap = false;
        for (ConstantsChannelField field : fields) {
            overlap |= field.allowOverlap();
        }
        return overlap;
    }

    @Override
    protected String toText() {
        String result = String.format("    ConstantsChannel: %s\n", getName());
        for (ConstantsChannelField field : fields) {
            result += field.toText();
        }
        return result;
    }
    
    @Override
    protected String toCast() {
        String result = String.format("  // Channel %s %s\n", getNameCamelCase(true), getCastDescription());
        //result += String.format("  int W_%s = %d;\n", getName(), width);
        for (ConstantsChannelField field : fields) {
            result += field.toCast();
        }
        return result + "\n";
    }
    
    protected String toCastStruct() {
        String result = "";
        if(hasOverlapingFields()) {
            result += String.format("    // No structure created for %s due to field overlap.\n", name);
        } else {
            String structResult = "";
            for (ConstantsChannelField field : fields) {
                structResult = field.toCastStruct() + structResult; // cast has bit 0 at bottom
            }
            result += String.format("    structure %s = (\n%s    );\n\n", getNameCamelCase(true), structResult);
        }
        
        return result;
    }
    
    @Override
    protected String toVerilog() {
        String result = String.format("  // Channel %s %s\n", getNameCamelCase(true), getCastDescription());
        //result += String.format("  int W_%s = %d;\n", getName(), width);
        for (ConstantsChannelField field : fields) {
            result += field.toVerilog();
        }
        return result + "\n";
    }
    
}
