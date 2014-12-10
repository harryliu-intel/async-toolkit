package com.avlsi.layout;
import com.avlsi.file.common.HierName;
import java.util.Collection;
import java.util.List;

/**
 * Factory to create Collections of APPortDef's from CellInterface info
 **/
public abstract class APPortDefFactory {
    public abstract APPortDef makePowerGridDef( final HierName netName, 
                                                final HierName portName, 
                                                final float width,
                                                final float spacing );
    public abstract APPortDef makeNodeDef( final HierName netName,
                                           final HierName portName,
                                           final int direction, 
                                           final float width,
                                           final float spacing );   
    public abstract APPortDef makeChannelDef( final HierName portName,
                                              final String portType,
                                              final List subPorts,
                                              final int direction,
                                              final boolean bunched );
    public abstract APPortDef makeHorizontalStrutDef( final HierName netName, 
                                                      final HierName portName, 
                                                      final float width,
                                                      final float spacing );
}
