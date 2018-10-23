#
# White Model register collateral generator
#
# Usage:
# % rdlgen.rb wm_reg_fields top.rdl hlp_debug_reg_fields.c
#

class HlpDebugRegFieldsCView < RDL::ViewGen
  def initialize(top_rdl_file, view_file, template_file)
    super(top_rdl_file)           # init @top_map
    source(view_file, template_file) # init @view_code from template
    update(wmRegFields,         # insert new code between delimiters
           "    // Auto generated reg fields begin\n",
           "    // Auto generated reg fields end\n")
    commit(view_file)           # save @view_code to view_file
  end

  def wmRegFields()
    code = Array.new
    @top_map.instances.each do |am_inst|
      rf_defn = am_inst.type_defn
      rf_defn.instances.each do |rg_inst|
        rg_defn = rg_inst.type_defn
        if rg_defn.class == RDL::Regfile
          rg_inst = rg_inst.type_defn.instances[0]
          rg_defn = rg_inst.type_defn
        end
        regName = rg_inst.inst_name
        
        #MAP_TYPE is reserved. So add prefix '_' to regName
        #/usr/include/bits/mman.h:# define MAP_TYPE      0x0f            /* Mask for type of mapping.  */
        if regName == "MAP_TYPE"
            code.push "    if (strcmp(registerName, \"#{regName}\") == 0)"
            code.push "    {"
            code.push "        static const fmRegisterField _#{regName}[] ="
            code.push "        {"
            
            rg_defn.fields.each do |fld|
              code.push "            { \"#{fld.inst_name}\", #{fld.lsb}, #{fld.width} },"
            end
            code.push "            { NULL, 0, 0} "
            code.push "        };"
            code.push "        return _#{regName};"
            code.push "    }"
        else
            code.push "    if (strcmp(registerName, \"#{regName}\") == 0)"
            code.push "    {"
            code.push "        static const fmRegisterField #{regName}[] ="
            code.push "        {"
            
            rg_defn.fields.each do |fld|
              code.push "            { \"#{fld.inst_name}\", #{fld.lsb}, #{fld.width} },"
            end
            code.push "            { NULL, 0, 0} "
            code.push "        };"
            code.push "        return #{regName};"
            code.push "    }"
        end
      end
    end
    return code
  end

end # class WmRegistersView
