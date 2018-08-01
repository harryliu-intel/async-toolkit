#
# White Model register collateral generator
#
# Usage:
# % rdlgen.rb wm_reg_int top.rdl hlp_api_regs_int.h
#

class HlpApiRegsIntHView < RDL::ViewGen
  def initialize(top_rdl_file, view_file, template_file)
    super(top_rdl_file)           # init @top_map
    source(view_file, template_file) # init @view_code from template
    update(wmRegParams,         # insert new code between delimiters
           "// Auto generated reg params begin\n",
           "// Auto generated reg params end\n")
    update(wmRegEnumsAndStructs,         # insert new code between delimiters
           "// Auto generated reg enums begin\n",
           "// Auto generated reg enums end\n")
           
    commit(view_file)           # save @view_code to view_file
  end

  private
    
  def printAmParam(code, amName, amBase, amSize)
    code.push ""
    code.push "/******** #{amName}_BASE *******/"
    code.push "#define HLP_#{amName}_BASE".ljust(64)+"(0x%07X)" % amBase
    code.push "#define HLP_#{amName}_SIZE".ljust(64)+"(0x%07X)" % amSize
    code.push ""
    return code
  end
  
  def printRegParam(code, amName, regName, nDims, regWidth, entries0, entries1, entries2, stride0, stride1, stride2, regOffset)
    code.push "#define HLP_#{regName}_WIDTH".ljust(64)+"%d" % regWidth
    if(nDims == 3) 
        code.push "#define HLP_#{regName}_ENTRIES_0".ljust(64)+"%d" % entries0  
        code.push "#define HLP_#{regName}_ENTRIES_1".ljust(64)+"%d" % entries1 
        code.push "#define HLP_#{regName}_ENTRIES_2".ljust(64)+"%d" % entries2 
        if(regWidth > 1)
            code.push "#define HLP_#{regName}(index2, index1, index0, word)".ljust(64)+"((0x%07X) * ((index2) - 0)" % stride2 + "+(0x%07X) * ((index1) - 0)" % stride1 + " + (0x%07X) * ((index0) - 0) + ((word)*4)" % stride0 + "+ (0x%07X) + (HLP_#{amName}_BASE))" % regOffset
        else
            code.push "#define HLP_#{regName}(index2, index1, index0)".ljust(64)+"((0x%07X) * ((index2) - 0)" % stride2 + "+(0x%07X) * ((index1) - 0)" % stride1 + " + (0x%07X) * ((index0) - 0)" % stride0 + " + (0x%07X) + (HLP_#{amName}_BASE))" % regOffset
        end        
    end
    if(nDims == 2) 
        code.push "#define HLP_#{regName}_ENTRIES_0".ljust(64)+"%d" % entries0  
        code.push "#define HLP_#{regName}_ENTRIES_1".ljust(64)+"%d" % entries1 
        if(regWidth > 1)
            code.push "#define HLP_#{regName}(index1, index0, word)".ljust(64)+"((0x%07X) * ((index1) - 0)" % stride1 + " + (0x%07X) * ((index0) - 0) + ((word)*4)" % stride0 + "+ (0x%07X) + (HLP_#{amName}_BASE))" % regOffset
        else
            code.push "#define HLP_#{regName}(index1, index0)".ljust(64)+"((0x%07X) * ((index1) - 0)" % stride1 + " + (0x%07X) * ((index0) - 0)" % stride0 + " + (0x%07X) + (HLP_#{amName}_BASE))" % regOffset
        end        
    end
    if(nDims == 1)
        code.push "#define HLP_#{regName}_ENTRIES".ljust(64)+"%d" % entries0  
        if(regWidth > 1)
            code.push "#define HLP_#{regName}(index, word)".ljust(64)+"((0x%07X) * ((index) - 0) + ((word)*4)" % stride0 + "+ (0x%07X) + (HLP_#{amName}_BASE))" % regOffset
        else
            code.push "#define HLP_#{regName}(index)".ljust(64)+"((0x%07X) * ((index) - 0)" % stride0 + " + (0x%07X) + (HLP_#{amName}_BASE))" % regOffset
        end 
    end
    if(nDims == 0)
        if(regWidth > 1)
            code.push "#define HLP_#{regName}(word)".ljust(64)+"(((word)*4) + (0x%07X) + (HLP_#{amName}_BASE))" % regOffset
        else
            code.push "#define HLP_#{regName}()".ljust(64)+"((0x%07X) + (HLP_#{amName}_BASE))" % regOffset
        end
    end
    code.push ""
    return code
  end
  
  def printRegField(code, regName, fld)
    fldname = fld.inst_name
    if fld.lsb == fld.msb
      code.push "#define HLP_#{regName}_b_#{fldname}".ljust(64)+"%d" % fld.lsb
    else
      code.push "#define HLP_#{regName}_l_#{fldname}".ljust(64)+"%d" % fld.lsb
      code.push "#define HLP_#{regName}_h_#{fldname}".ljust(64)+"%d" % fld.msb
    end
    return code
  end
  
  def printEnum(encode)
    code = Array.new
    enum = {encode.type_name => code}
    eName = encode.type_name
    eName.sub!(/_enum$/,'')
    width = encode.encodings[0].width
    code.push "typedef enum {"
    length = encode.encodings.map{ |e| e.name.size }.max +
      encode.type_name.size + 3
    length = 29 if length < 29
    encode.encodings.each do |e|
        code.push "    HLP_#{eName.upcase}_#{e.name.upcase}".ljust(32)+" = #{e.value},"
    end
    code.last.sub!(/\,\z/,"")
    eNameCC = eName.split('_').collect(&:capitalize).join
    code.push "} hlp#{eNameCC};"
    code.push ""
    return enum
  end

  def printRegVersion(code)
    reg_ver, reg_tag = get_reg_version()
    if reg_ver != nil and reg_tag != nil
      code.push "/************* REGISTER VERSION **************/"
	  code.push "/* Used only by SW, not part of the register specs */"
      code.push "#define HLP_REG_VERSION".ljust(69)+"%s" % reg_ver
      code.push "#define HLP_REG_TAG".ljust(68)+"%s" % reg_tag
      code.push ""
    end
  end

  def wmRegParams()
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
        #order is index2 index1 index0 = entries2 entries1 entries0, they can select from antries entries intries
        #by default index2<-antries, index1<-entries, index0<-intries, but if they select other tries
        #you need to change strides accordingly
        #on initial stride2<-map addr incr
        #           stride1<-rf addr incr
        #           stride0<-reg addr incr
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
                # shouldn't here a stride0=stride1?
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
                entries0 = intries;
              else
                nDims = 0;
              end
            end
        end    
        code = printRegParam(code, amName, regName, nDims, width, entries0, entries1,  entries2, stride0, stride1, stride2, reg_Offset)

        rg_defn.fields.each do |fld|
          code = printRegField(code, regName, fld) 
        end
        code.push ""
      end
    end
    return code
  end

  def wmRegEnumsAndStructs()
    code = Array.new
    rf_uniq =Hash.new
    @top_map.instances.each do |am_inst|
      next if am_inst.type_name =~ /shell_ctl/i
      code.push "/****** #{am_inst.inst_name.upcase}_BASE Register Structs ******/"
      #rf_uniq = Hash.new
      rf_defn = am_inst.type_defn
      rf_defn.instances.each do |rg_inst|
        rg_defn = rg_inst.type_defn
        if rg_defn.class == RDL::Regfile
          rg_inst = rg_inst.type_defn.instances[0]
          rg_defn = rg_inst.type_defn
        end
        code.push(rg_enums = Array.new)
        next if rf_uniq.has_key?(rg_defn.type_name) #||
                #rg_defn.type_name =~ /^shell_ctl/
        #skip printing of switch_ip and switch_im since switch field is already used by for other purposes.        
        next if rg_defn.type_name =~ /^switch_ip/ ||
                rg_defn.type_name =~ /^switch_im/
        rf_uniq[rg_defn.type_name] = true
        rName = rg_defn.type_name
        rNameCC = rg_defn.type_name.split('_').collect(&:capitalize).join
        rNameCC.sub!(/R$/,'') 
        code.push "typedef struct _hlp#{rNameCC}"
        code.push "{"
        uniq_enums = Hash.new
        rg_defn.fields.each do |fld|
          if fld.encode.nil?
            if(fld.width == 1)
            type = "fm_bool"
            else
                if(fld.width <= 8)
                    type = "fm_byte"
                else
                    if(fld.width <= 16)
                        type = "fm_uint16"
                    else
                        if(fld.width <= 32)
                            type = "fm_uint32"
                        else
                            #if(fld.width <= 64)
                                type = "fm_uint64"
                            #end    
                        end
                    end
                end
            end
          else
            eName = fld.encode.type_name;
            eName.sub!(/_enum$/,'')
            eNameCC = eName.split('_').collect(&:capitalize).join
            type = "hlp#{eNameCC}"

            enum = printEnum(fld.encode)
            if !rf_uniq.has_key?(enum.keys[0])
              rf_uniq[enum.keys[0]] = true
              uniq_enums.merge!(enum)
            end
          end
         #name = fld.inst_name.gsub(/([A-Z][a-z])/,'_\1').downcase
         #name = name.sub(/^_/,'').gsub(/__/,"_")
         #MAP_TYPE symbol name is already used by sys/mman.h
         if(fld.inst_name == "MAP_TYPE")
            name = "MAP_ETYPE"
         else   
            name = fld.inst_name;
         end   
          code.push "  #{type} ".ljust(32) + "#{name};"
        end
        rg_enums.push uniq_enums.values
        code.push "} hlp#{rNameCC};"
        code.push ""
      end
      code.push ""
    end
    return code
  end
  
end # class WmRegistersView
