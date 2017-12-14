package com.fulcrummicro.hw.verification.lib.constants;

import java.io.File;
import java.io.IOException;

import org.apache.commons.digester.Digester;

import com.avlsi.util.cmdlineargs.CommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CachingCommandLineArgs;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsDefImpl;
import com.avlsi.util.cmdlineargs.defimpl.CommandLineArgsWithConfigFiles;
import com.fulcrummicro.hw.verification.chip.mgmt.registers.RegisterInfo;
import com.fulcrummicro.util.misc.Utility;


public class ConstantsDigester {
    
    public static class ConstantsDigesterConfig {
        private final File file;
        private Constants constants;
        public ConstantsDigesterConfig(File file) {this.file = file;}
        public void setConstants(Constants c) {constants = c;}
        public Constants process() {
            constants.process(file);
            return constants;
        }
    }
    
    protected static boolean parseError = false;

    /**
     * @param args
     */
    public static void main(String[] args) {
        final CommandLineArgs theArgs = 
            new CachingCommandLineArgs(new CommandLineArgsWithConfigFiles(new CommandLineArgsDefImpl(args)));
        final String xml = theArgs.getArgValue("xml", "");
        final String cast = theArgs.getArgValue("cast", "");     
        final String verilog = theArgs.getArgValue("verilog", "");
        
        Constants c = parse(xml);
        /*
        if(cast.length() > 0) {
            try {
                FileWriter castWriter = new FileWriter(cast);
                castWriter.write(c.toCast(module));
                castWriter.flush();
                castWriter.close();
            } catch (IOException e) {
                System.err.println(Utility.exceptionString(e));
            }
        }
        if(verilog.length() > 0) {
            try {
                FileWriter verilogWriter = new FileWriter(verilog);
                verilogWriter.write(c.toVerilog());
                verilogWriter.flush();
                verilogWriter.close();
            } catch (IOException e) {
                System.err.println(Utility.exceptionString(e));
            }
        }*/
        if(cast.length() > 0) {
            try {
                new TemplateCast().render(RegisterInfo.generatePrintWriter(cast), c);
            } catch (IOException e) {
                System.err.println(Utility.exceptionString(e));
            }
        }
        if(verilog.length() > 0) {
            try {
                new TemplateVerilog().render(RegisterInfo.generatePrintWriter(verilog), c.cells.values());
            } catch (IOException e) {
                System.err.println(Utility.exceptionString(e));
            }
        }
        
        if(parseError) {
            System.err.println("FAIL");
        } else {
            System.err.println("PASS");
        }
    }
    
    public static Constants parse(String filename) {
        try {
            File file = new File(filename);
            return parse(file);
        } catch( Exception exc ) {
            exc.printStackTrace();
        }
        return null;
    }
    
    public static Constants parse(File file) {
        try {
            ConstantsDigesterConfig c = new ConstantsDigesterConfig(file);
            Digester digester = new Digester();
            digester.setValidating( false );
            
            digester.push(c);
            digester.addObjectCreate("constants", Constants.class);
            digester.addSetProperties("constants");
            
            digester.addCallMethod("constants/import", "addImport",0);
            
            digester.addObjectCreate("constants/cell", ConstantsCell.class);
            digester.addSetProperties("constants/cell");
            digester.addBeanPropertySetter("constants/cell/description");
            
            digester.addObjectCreate("constants/cell/constant", ConstantsConstant.class);
            digester.addSetProperties("constants/cell/constant", 
                                      new String[] {"name", "value", "size"}, 
                                      new String[] {"name", "valueExpression", "sizeExpression"});
            digester.addBeanPropertySetter("constants/cell/constant", "description");
            digester.addSetNext("constants/cell/constant", "addConstant");

            digester.addObjectCreate("constants/cell/enum", ConstantsEnum.class);
            digester.addSetProperties("constants/cell/enum");
            digester.addBeanPropertySetter("constants/cell/enum/description");
            digester.addObjectCreate("constants/cell/enum/values/value", ConstantsEnumValue.class);
            digester.addSetProperties("constants/cell/enum/values/value", 
                                      new String[] {"name", "encoding"}, 
                                      new String[] {"name", "encodingExpression"});
            digester.addBeanPropertySetter("constants/cell/enum/values/value", "description");
            digester.addSetNext("constants/cell/enum/values/value", "addValue");
            digester.addSetNext("constants/cell/enum", "addEnum");

            digester.addObjectCreate("constants/cell/channel", ConstantsChannel.class);
            digester.addSetProperties("constants/cell/channel", new String[] {"extends"}, new String[] {"extendsChannelString"});
            digester.addBeanPropertySetter("constants/cell/channel/description");
            digester.addBeanPropertySetter("constants/cell/channel/width", "widthExpression");
            digester.addBeanPropertySetter("constants/cell/channel/type", "typeString");
            digester.addObjectCreate("constants/cell/channel/fields/field", ConstantsChannelField.class);
            digester.addSetProperties("constants/cell/channel/fields/field", 
                    new String[] {"name", "dir"}, 
                    new String[] {"name", "direction"});
            digester.addBeanPropertySetter("constants/cell/channel/fields/field/description");
            digester.addBeanPropertySetter("constants/cell/channel/fields/field/array", "arrayExpression");
            digester.addBeanPropertySetter("constants/cell/channel/fields/field/type", "typeString");
            digester.addBeanPropertySetter("constants/cell/channel/fields/field/position", "positionExpression");
            digester.addBeanPropertySetter("constants/cell/channel/fields/field/width", "widthExpression");
            digester.addBeanPropertySetter("constants/cell/channel/fields/field/encoding", "enumString");
            digester.addSetNext("constants/cell/channel/fields/field", "addField");
            digester.addSetNext("constants/cell/channel", "addChannel");
            
            digester.addSetNext("constants/cell", "addCell");
            
            digester.addSetNext("constants", "setConstants");
            
            c = (ConstantsDigesterConfig)digester.parse(file);
            return c.process();
        } catch( Exception exc ) {
            exc.printStackTrace();
        }

        return null;
    }
    
    public static void warning(String error) {
        System.err.println(error);
    }
    
    public static void error(String error) {
        System.err.println(error);
        parseError = true;
    }

}
