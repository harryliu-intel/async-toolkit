package com.avlsi.layout;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.text.MessageFormat;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import com.avlsi.cast2.util.DirectiveFilter;
import com.avlsi.cast2.util.DirectiveWalker;
import com.avlsi.cast2.util.DirectiveActionInterface;
import com.avlsi.cast2.directive.impl.DirectiveEmitter;
import com.avlsi.cast2.directive.UnknownDirectiveException;
import com.avlsi.file.cdl.util.rename.CDLRenameException;
import com.avlsi.file.common.HierName;
import com.avlsi.util.cmdlineargs.CommandLineArg;
import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.container.Pair;
import com.avlsi.fast.BlockInterface;
import com.avlsi.fast.DirectiveBlock;
import com.avlsi.cell.CellInterface;
import com.avlsi.cast2.directive.DirectiveConstants;
import com.avlsi.cast2.util.DirectiveUtils;


public class CellDirs2Skill implements CellProcessorInterface {
     
    private static MessageFormat bindForm = new MessageFormat("( setarray {0} {1} {2} )\n" );
    private static MessageFormat createForm = new MessageFormat("( setq {0} ( makeTable `{0} nil ) )\n" );
    private static MessageFormat stringForm = new MessageFormat("\"{0}\"" );

    private static MessageFormat directiveForm = new MessageFormat("\"{0}({1})\"" );

    private static final Set STANDARD_IGNORED_DIRECTIVES =
        new HashSet( Arrays.asList(
                new String[] { DirectiveConstants.CAP,
                               DirectiveConstants.ESTIMATED_DELAY,
                               DirectiveConstants.EXCLCC,
                               DirectiveConstants.NOCC_NODES } ) );

    private final CellInterface cell;

    private final boolean reduceDirectives;
 
    public CellDirs2Skill( CellInterface cell ) 
    {
        this( cell, false ); 
    }
    
    public CellDirs2Skill( CellInterface cell, boolean reduceDirectives ) 
    {
        this.cell = cell;
        this.reduceDirectives = reduceDirectives;
    }
    
    public void process(final Writer writer)
        throws IOException, UnknownDirectiveException {    
        final String topTable = "DirectiveTable";
        writer.write( createForm.format( new Object[] { topTable } ) );
        
        final DirectiveEmitter de = new SkillDirectiveEmitter();

        DirectiveActionInterface dai = new DirectiveActionInterface() {
                
                public String getBlockTableString(BlockInterface block) {
                    if( block.getType().equals(BlockInterface.CELL) )
                        return topTable;
                    else
                        return block.getType() + "Table";
                }

                public String getParameterizedDirectiveTableString(BlockInterface block, String directive) {
                    return getBlockTableString(block) + "_" + directive;
                }

           
                public void doBlockInterface(BlockInterface block) 
                    throws IOException {
                    String blockTable = getBlockTableString(block);
                    if( !block.getType().equals(BlockInterface.CELL) ) {
                        writer.write( createForm.format( new Object[] { blockTable } ) );
                        writer.write( bindForm.format( new Object[] 
                            { topTable, 
                              stringForm.format( new Object[] { block.getType() } ),
                              blockTable } ) );
                    }
                }
           
                public void doParameterizedDirectiveType(BlockInterface block,
                                                         DirectiveBlock db,
                                                         String directive,
                                                         String parameterType,
                                                         String valueType) 
                    throws IOException {
                    String blockTable = getBlockTableString(block);
                    String subTable = 
                        getParameterizedDirectiveTableString(block, directive);
                    writer.write( createForm.format( new Object[] { subTable } ) );
                    writer.write( bindForm.format( new Object[] 
                        { blockTable, 
                          directiveForm.format( new Object[] { directive,
                                                               parameterType} ),
                          subTable
                        } ) );
                }

                public void doParameterizedDirectiveValue(BlockInterface block,
                                                          DirectiveBlock db,
                                                          String directive,
                                                          Object parameter,
                                                          Object value,
                                                          String parameterType,
                                                          String valueType) 
                    throws IOException {
                  
                    String subTable = 
                        getParameterizedDirectiveTableString(block, directive);

                    Object[] args =  new Object[] 
                        { subTable, 
                          de.emit(block.getType(),
                                  parameterType,
                                  parameter),
                          de.emit(block.getType(),
                                  valueType,
                                  value)
                        };
                    writer.write( bindForm.format( args ) );
                    
                }
                
                public void doUnParameterizedDirective(BlockInterface block,
                                                       DirectiveBlock db,
                                                       String directive,
                                                       Object value,
                                                       String valueType) 
                    throws IOException {
                    
                    String blockTable = getBlockTableString(block);
                    Object[] args = new Object[]
                        { blockTable,
                          stringForm.format( new Object[] { directive } ),
                          de.emit(block.getType(),
                                  valueType,
                                  value)
                        };
                    writer.write( bindForm.format( args ) );
                }
            };

        final DirectiveWalker dw = new DirectiveWalker(
            new DirectiveFilter.ByDirective(false, STANDARD_IGNORED_DIRECTIVES)
            .filter(dai));
        
        final BlockInterface cellBlock = cell.getBlockInterface();
        dw.walk(cellBlock);
        if (!reduceDirectives) {
            dw.walk(cellBlock, BlockInterface.PRS);
            dw.walk(cellBlock, BlockInterface.SUBCELL);
        }
    }
}
