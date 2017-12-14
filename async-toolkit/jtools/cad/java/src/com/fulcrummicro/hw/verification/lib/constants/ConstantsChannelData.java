package com.fulcrummicro.hw.verification.lib.constants;

import com.fulcrummicro.hw.verification.lib.sim.Data;

public class ConstantsChannelData extends Data {
    private final ConstantsCell cell;
    private final String channel;
    
    public ConstantsChannelData(ConstantsCell cell, String channel) {
        super(cell.getChannel(channel).getWidth());
        this.cell = cell;
        this.channel = channel;
    }

    public ConstantsChannelData(ConstantsCell cell, String channel, Data channelData) {
        super(channelData.width());
        this.cell = cell;
        this.channel = channel;
        this.setTo(channelData);
    }
    
    public ConstantsChannelData(ConstantsCell cell, String channel, int width) {
        super(width);
        this.cell = cell;
        this.channel = channel;
    }
    
    public void setField(String fieldName, long value) {
        ConstantsChannelField field = cell.getChannelField(channel, fieldName);
        this.aval.set(value, field.getPosition(), field.getWidth());
        this.bval.set(0, field.getPosition(), field.getWidth());
    }

    public void setField(String fieldName, Data value) {
        ConstantsChannelField field = cell.getChannelField(channel, fieldName);
        this.aval.copy(value.getAvalBA(), 0, field.getPosition(), field.getWidth());
        this.bval.copy(value.getBvalBA(), 0, field.getPosition(), field.getWidth());
    }
    
    public long getUnsignedLongField(String fieldName) {
        ConstantsChannelField field = cell.getChannelField(channel, fieldName);
        return this.aval.getUnsignedLong(field.getPosition(), field.getWidth());
    }
    
    public ConstantsChannelField getConstantsChannelField(String fieldName) {
        return cell.getChannelField(channel, fieldName);
    }
    
    public Data getDataField(String fieldName) {
        ConstantsChannelField field = cell.getChannelField(channel, fieldName);
        return this.getSlice(field.getPosition(), field.getWidth());
    }
    
    @Override
    public ConstantsChannelData clone() {
        ConstantsChannelData newdata = new ConstantsChannelData(cell, channel, width());
        newdata.set(aval, bval);
        return newdata;
    }
    
    /** Create a copy that is the same size or wider */
    public ConstantsChannelData clone(int widerWidth) {
        ConstantsChannelData newdata = new ConstantsChannelData(cell, channel, widerWidth);
        newdata.set(this);
        return newdata;
    }
}
