#
# White Model register collateral generator
#
# Usage:
# % rdlgen.rb perl_reg_api5 top.rdl registers.pm
#

class RegistersPmView < RDL::ViewGen
  def initialize(top_rdl_file, view_file, template_file)
    super(top_rdl_file)           # init @top_map
    source(view_file, template_file)  # init @view_code from wm_reg_int.template
    update(perlRegParamsApi5,         # insert new code between delimiters
           "Auto generated reg functions begin",
           "Auto generated reg functions end")
    commit(view_file)           # save @view_code to view_file
  end

  private
    
  def printAmParam(code, amName, amBase, amSize)
    code.push "################ #{amName}_BASE ################"
    code.push "our $FPPS_#{amName}_BASE".ljust(64)+"= 0x%07X;" % amBase
    code.push "our $FPPS_#{amName}_SIZE".ljust(64)+"= 0x%07X;" % amSize
    code.push ""
    return code
  end
  
  def printRegParam(code, amName, regName, nDims, regWidth, entries0, entries1, entries2, stride0, stride1, stride2, regOffset, default)
    #regWidth is in quad words here
    numQuadWords = regWidth/2
    code.push "our $FPPS_#{regName}_WIDTH".ljust(74)+"= %d;" % numQuadWords
    if(nDims == 3) 
        code.push "our $FPPS_#{regName}_ENTRIES_0".ljust(74)+"= %d;" % entries0  
        code.push "our $FPPS_#{regName}_ENTRIES_1".ljust(74)+"= %d;" % entries1 
        code.push "our $FPPS_#{regName}_ENTRIES_2".ljust(74)+"= %d;" % entries2 
        if(regWidth > 1)
            code.push "sub FPPS_#{regName}\n{\n\t_validatePrototype(@_, 5, -1);\n\tmy ($self, $index2, $index1, $index0, $qword) = @_;\n\treturn ((0x%07X * $index2)" % stride2 + " + (0x%07X * $index1)" % stride1 + " + (0x%07X * $index0) + ($qword * 8)" % stride0 + " + 0x%07X + $FPPS_#{amName}_BASE);\n}" % regOffset
        else
            code.push "sub FPPS_#{regName}\n{\n\t_validatePrototype(@_, 4, -1);\n\tmy ($self, $index2, $index1, $index0) = @_;\n\treturn ((0x%07X * $index2)" % stride2 + " + (0x%07X * $index1)" % stride1 + " + (0x%07X * $index0)" % stride0 + " + 0x%07X + $FPPS_#{amName}_BASE);\n}" % regOffset
        end        
    end
    if(nDims == 2) 
        code.push "our $FPPS_#{regName}_ENTRIES_0".ljust(74)+"= %d;" % entries0  
        code.push "our $FPPS_#{regName}_ENTRIES_1".ljust(74)+"= %d;" % entries1 
        if(regWidth > 1)
            code.push "sub FPPS_#{regName}\n{\n\t_validatePrototype(@_, 4, -1);\n\tmy ($self, $index1, $index0, $qword) = @_;\n\treturn ((0x%07X * $index1)" % stride1 + " + (0x%07X * $index0) + ($qword * 8)" % stride0 + " +  0x%07X + $FPPS_#{amName}_BASE);\n}" % regOffset
        else
            code.push "sub FPPS_#{regName}\n{\n\t_validatePrototype(@_, 3, -1);\n\tmy ($self, $index1, $index0) = @_;\n\treturn ((0x%07X * $index1)" % stride1 + " + (0x%07X * $index0)" % stride0 + " + 0x%07X + $FPPS_#{amName}_BASE);\n}" % regOffset
        end        
    end
    if(nDims == 1)
        code.push "our $FPPS_#{regName}_ENTRIES".ljust(74)+"= %d;" % entries0  
        if(regWidth > 1)
            code.push "sub FPPS_#{regName}\n{\n\t_validatePrototype(@_, 3, -1);\n\tmy ($self, $index, $qword) = @_;\n\treturn ((0x%07X * $index) + ($qword * 8)" % stride0 + " + 0x%07X + $FPPS_#{amName}_BASE);\n}" % regOffset
        else
            code.push "sub FPPS_#{regName}\n{\n\t_validatePrototype(@_, 2, -1);\n\tmy ($self, $index) = @_;\n\treturn ((0x%07X * $index)" % stride0 + " + 0x%07X + $FPPS_#{amName}_BASE);\n}" % regOffset
        end 
    end
    if(nDims == 0)
        if(regWidth > 1)
            code.push "sub FPPS_#{regName}\n{\n\t_validatePrototype(@_, 2, -1);\n\tmy ($self, $qword) = @_;\n\treturn (($qword * 8) + 0x%07X + $FPPS_#{amName}_BASE);\n}" % regOffset
        else
            code.push "sub FPPS_#{regName}\n{\n\t_validatePrototype(@_, 1, -1);\n\tmy ($self) = @_;\n\treturn (0x%07X + $FPPS_#{amName}_BASE);\n}" % regOffset
        end
    end
    code.push "sub FPPS_#{regName}_GET_DEFAULT\n{\n\t_validatePrototype(@_, 1, -1);\n\tmy ($self) = @_;\n\treturn \"0x%X\";\n}" % default
    code.push ""
    return code
  end
  
  def printRegField(code, regName, fld)
    fldname = fld.inst_name
    if fld.lsb == fld.msb
      code.push "our $FPPS_#{regName}_b_#{fldname}".ljust(74)+"= %d;" % fld.lsb
    else
      code.push "our $FPPS_#{regName}_l_#{fldname}".ljust(74)+"= %d;" % fld.lsb
      code.push "our $FPPS_#{regName}_h_#{fldname}".ljust(74)+"= %d;" % fld.msb
    end
    return code
  end
  
  def printRegVersion(code)
    reg_ver, reg_tag = get_reg_version()
    if reg_ver != nil and reg_tag != nil
      code.push "############### REGISTER VERSION ################"
      code.push "# Used only by SW and not part of the register specs"
      code.push "our $FPPS_REG_VERSION".ljust(69)+"= %s;" % reg_ver
      code.push "our $FPPS_REG_TAG".ljust(68)+"= %s;" % reg_tag
      code.push ""
    end
  end

  def perlRegParamsApi5()
    code = Array.new
    nDims = 0;
    stride0 = 0;
    stride1 = 0;
    stride2 = 0;
    antries = 1;
    printRegVersion(code);
    @top_map.instances.each_with_index.map do |am_inst,ix|
      antries = am_inst.inst_size;
      stride2 = am_inst.addr_incr;
      amBase = am_inst.addr_base;
      if(antries > 1)
        amSize = antries * stride2;
      else
        amSize = 1 << (am_inst.type_defn.addressbits || 0)
      end
      amName = am_inst.inst_name.upcase;
      code = printAmParam(code, amName, amBase, amSize)
      rf_defn = am_inst.type_defn
      rf_defn.instances.each do |rg_inst|
        entries = rg_inst.inst_size # outer number of entries
        intries = 0                 # inner number of entries 
        stride1 = rg_inst.addr_incr;
        rg_defn = rg_inst.type_defn
        rf_addr = 0;
        if rg_defn.class == RDL::Regfile
          rf_addr = rg_inst.addr_base
          rg_inst = rg_inst.type_defn.instances[0]
          rg_defn = rg_inst.type_defn
          intries = rg_inst.inst_size
        end
        #puts "REG #{rg_inst.inst_name} rfaddr = 0x%08X" % rf_addr
        
        regName = rg_inst.inst_name
        base = (am_inst.addr_base + rf_addr + rg_inst.addr_base);
        default = rg_defn.fields.map{|x|x.reset << x.lsb}.inject{|sum,y|sum + y}
        reg_Offset = (rf_addr + rg_inst.addr_base);
        width = rg_defn.regwidth / 32
        if rg_inst.addr_incr > 0
          stride0 = rg_inst.addr_incr
        else
          stride0 = rg_defn.regwidth / 8 
        end

        #puts "REG #{rg_inst.inst_name} stride0 = 0x%08X" % stride0
        #puts "REG #{rg_inst.inst_name} stride1 = 0x%08X" % stride1
        #puts "REG #{rg_inst.inst_name} stride2 = 0x%08X" % stride2

        entries0 = 0;
        entries1 = 0;
        entries2 = 0;
        if(antries > 1)
            if entries > 1
              if intries > 1
                nDims = 3;
                entries0 = intries;
                entries1 = entries;
                entries2 = antries;
              else
                nDims = 2;
                entries0 = entries;
                entries1 = antries;
                stride1 = stride2;
              end
            else
              if intries > 1
                nDims = 2;
                entries0 = intries;
                entries1 = antries;
                stride1 = stride2;
              else
                nDims = 1;
                entries0 = antries;
                stride0 = stride2;
              end
            end
        else
            if entries > 1
              if intries > 1
                nDims = 2;
                entries0 = intries;
                entries1 = entries;
              else
                nDims = 1;
                entries0 = entries;
              end
            else
              if intries > 1
                nDims = 1;
                entries0 = intires;
              else
                nDims = 0;
              end
            end
        end    
        code = printRegParam(code, amName, regName, nDims, width, entries0, entries1,  entries2, stride0, stride1, stride2, reg_Offset, default)

        rg_defn.fields.each do |fld|
          code = printRegField(code, regName, fld) 
        end
        code.push ""
      end
    end
    return code
  end

end
