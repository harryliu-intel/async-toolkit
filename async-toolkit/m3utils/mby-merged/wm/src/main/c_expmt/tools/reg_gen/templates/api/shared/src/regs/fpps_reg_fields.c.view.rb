#
# White Model register collateral generator
#
class FppsRegFieldsCView < RDL::ViewGen
  def initialize(top_rdl_file, view_file, template_file)
    super(top_rdl_file)           # init @top_map
    source(view_file, template_file) # init @view_code from template
    update(wmRegFieldsApi5,         # insert new code between delimiters
           "Auto generated reg fields begin",
           "Auto generated reg fields end")
    commit(view_file)           # save @view_code to view_file
  end

  def wmRegFieldsApi5()
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

        ifdef_debug_regs(rg_inst.type_defn.name, code)

        code.push "\tif (!strcmp(regname, \"#{regName}\")) {"
        #MAP_TYPE is reserved. So add prefix '_' to regName
        #/usr/include/bits/mman.h:# define MAP_TYPE      0x0f
        if regName == "MAP_TYPE"
            regName = regName + "_X"
        end
        code.push "\t\tstatic const struct ies_register_field"
        code.push "\t\t\t#{regName}[] = {"
        
        rg_defn.fields.each do |fld|
	  if (fld.inst_name != fld.inst_name.underscore.upcase)
             raise "Error: #{fld.inst_name} is not in correct format"
          end
          fldname = fld.inst_name
          code.push "\t\t\t{ \"#{fldname}\", #{fld.lsb}, #{fld.width} },"
        end
        code.push "\t\t\t{ NULL, 0, 0}"
        code.push "\t\t};"
        code.push "\t\treturn #{regName};"
        code.push "\t}"
      end

      # In case last register is debug register
      close_debug_reg_ifdef(code)

    end
    return code
  end

end # class WmRegistersView
