/*
 * Copyright 2003 Fulcrum Microsystems.  All rights reserved.
 * $Id$
 * $DateTime$
 * $Author$
 */

package com.avlsi.layout;

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.text.MessageFormat;

import com.avlsi.util.container.CollectionUtils;
import com.avlsi.util.container.Pair;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.directive.impl.DirectiveEmitter;
import com.avlsi.file.common.HierName;
import com.avlsi.file.cdl.util.rename.CadenceNameInterface;
import com.avlsi.file.cdl.util.rename.CDLNameInterface;
import com.avlsi.file.cdl.util.rename.CDLRenameException;

public class SkillDirectiveEmitter implements DirectiveEmitter {

    public String emit(String block,String type,Object val) {
        if(val == null) return "nil";
        final boolean isArray = type.endsWith("[]");
        final String baseType = isArray ? type.replaceFirst("\\[\\]","") : type;
        if(isArray) {
            final Object[] array;
            if(val instanceof Object[]) {
                array = (Object[]) val;
            } else if(val instanceof List) {
                array = ((List)val).toArray();
            } else {
                throw new RuntimeException("directive value + " + val.toString() + " of type " + type + " is not a list or array" );
            }
            final StringBuffer ret = new StringBuffer();
            ret.append( "( list " );
            
            for ( int i = 0; i < array.length ; ++i ) {
                final String elementString = emit(block,baseType,array[i]);
                ret.append( elementString + " " );
            }
            ret.append( ")" );
            
            return ret.toString();
        }
        else {
            SkillStringInterface SSI =
                (SkillStringInterface) ssiMap.get(baseType);
            if(SSI == null) {
                SSI = stringSkillStringInterface;
            }
            return SSI.getString(val);
        }
    }
    
    private static MessageFormat pairForm = new MessageFormat("( list {0} {1} )" );
    private static MessageFormat stringForm = new MessageFormat("\"{0}\"" );
    private static CDLNameInterface renamer = new CadenceNameInterface(true);


    public static SkillStringInterface defaultSkillStringInterface = new SkillStringInterface() {
            public String getString(Object o) {
                return o.toString();
            } 
        };

    public static SkillStringInterface booleanSkillStringInterface = new SkillStringInterface() {
            public String getString(Object o) {
                Boolean b = (Boolean)o;
                if( b.booleanValue() )
                    return "t";
                else
                    return "nil";
            }
        };

    public static SkillStringInterface stringSkillStringInterface = new SkillStringInterface() {
            public String getString(Object o) {
                final String s = o.toString();
                final StringBuffer buf = new StringBuffer();
                for (int i = 0; i < s.length(); ++i) {
                    final char c = s.charAt(i);
                    switch(c) {
                      case '\n': buf.append("\\n"); break;
                      case '\t': buf.append("\\t"); break;
                      case '\b': buf.append("\\b"); break;
                      case '\r': buf.append("\\r"); break;
                      case '\f': buf.append("\\f"); break;
                      case '\\': buf.append("\\\\"); break;
                      case '"' : buf.append("\\\""); break;
                      default:
                        if (c > '\177') {
                            throw new RuntimeException("Cannot translate 0x" +
                                    Integer.toString(c, 16) + " to SKILL");
                        } else if (c >= ' ' && c <= '~') {
                            buf.append(c);
                        } else {
                            buf.append('\\');
                            if (c < '\010') buf.append('0');
                            if (c < '\100') buf.append('0');
                            buf.append(Integer.toOctalString(c));
                        }
                        break;
                    }
                }
                return stringForm.format( new Object[] { buf.toString() } );
            }
        };

    public static SkillStringInterface nodeSkillStringInterface = new SkillStringInterface() {
            public String getString(Object o) {
                final String name = ((HierName)o).getCadenceString();
                String cadenceName;
                try { 
                    cadenceName = renamer.renameNode(name);
                } catch (CDLRenameException e) {
                    throw new RuntimeException(e);
                }

                return stringForm.format( new Object[] { cadenceName } );
            }
        };

    public static SkillStringInterface wideChannelSkillStringInterface = new SkillStringInterface() {
            public String getString(Object o) {
                final String name = (String) o;
                String cadenceName;
                try { 
                    cadenceName = renamer.renameNode(name);
                } catch (CDLRenameException e) {
                    throw new RuntimeException(e);
                }

                return stringForm.format( new Object[] { cadenceName } );
            }
        };

    public static SkillStringInterface instanceSkillStringInterface = new SkillStringInterface() {
            public String getString(Object o) {
                final String name = ((HierName)o).getCadenceString();
                String cadenceName;
                try { 
                    cadenceName = renamer.renameSubCellInstance(name);
                } catch (CDLRenameException e) {
                    throw new RuntimeException(e);
                }
                return stringForm.format( new Object[] { cadenceName } );
            }
        };

    public static SkillStringInterface halfOpSkillStringInterface = new SkillStringInterface() {
            public String getString(Object o) {
                Pair pair = (Pair)o;
                final String direction;
                final HierName name = (HierName)pair.getFirst();
                
                if( pair.getSecond() == null )
                    direction = "+-";
                else if( ((Boolean)pair.getSecond()).booleanValue() )
                    direction = "+";
                else
                    direction = "-";
                    
                return pairForm.format( new Object[] { stringForm.format( new Object[] { name.getCadenceString() } ),
                                                       stringForm.format( new Object[] { direction } ) } );
            }
        };

    public static SkillStringInterface layerSkillStringInterface = new SkillStringInterface() {
            public String getString(Object o) {
                Pair pair = (Pair)o;
                return pairForm.format( new Object[] { stringForm.format( new Object[] { pair.getFirst().toString() } ), 
                                                       stringForm.format( new Object[] { pair.getSecond().toString() } ) } );
            }
        };


    public static Map ssiMap = 
        new HashMap( CollectionUtils.mapify(new Object[] {
            DirectiveConstants.INT_TYPE, defaultSkillStringInterface,
            DirectiveConstants.FLOAT_TYPE, defaultSkillStringInterface,
            DirectiveConstants.DOUBLE_TYPE, defaultSkillStringInterface,
            DirectiveConstants.BOOLEAN_TYPE, booleanSkillStringInterface,
            DirectiveConstants.STRING_TYPE, stringSkillStringInterface,
            DirectiveConstants.NODE_TYPE, nodeSkillStringInterface,
            DirectiveConstants.UNCHECKED_NODE_TYPE, nodeSkillStringInterface,
            DirectiveConstants.DEEP_NODE_TYPE, nodeSkillStringInterface,
            DirectiveConstants.HALFOP_TYPE, halfOpSkillStringInterface,
            DirectiveConstants.UNCHECKED_HALFOP_TYPE, halfOpSkillStringInterface,
            DirectiveConstants.DEEP_HALFOP_TYPE, halfOpSkillStringInterface,
            DirectiveConstants.CHANNEL_TYPE, nodeSkillStringInterface,
            DirectiveConstants.WIDE_CHANNEL_TYPE, wideChannelSkillStringInterface,
            DirectiveConstants.INSTANCE_TYPE, instanceSkillStringInterface,
            DirectiveConstants.LAYER_TYPE, layerSkillStringInterface } ) );

}
