#
# White Model register collateral generator
#
class FppsRegsIntHView < RDL::ViewGen
  def initialize(top_rdl_file, view_file, template_file)
    super(top_rdl_file)           # init @top_map
    source(view_file, template_file) # init @view_code from template
    update(wmRegParamsApi5,         # insert new code between delimiters
           "Auto generated reg defines begin",
           "Auto generated reg defines end")
    commit(view_file)           # save @view_code to view_file
  end

  private

  def printAmParam(code, amName, amBase, amSize)
    code.push "/******** #{amName}_BASE *******/"
    code.push "#define FPPS_#{amName}_BASE".ljust(64)+" (0x%07X)" % amBase
    code.push "#define FPPS_#{amName}_SIZE".ljust(64)+" (0x%07X)" % amSize
    code.push ""
    return code
  end

  def printRegParam(code, amName, regName, nDims, regWidth, entries0, entries1, entries2, stride0, stride1, stride2, regOffset)
    code.push "#define FPPS_#{regName}_WIDTH".ljust(74) + " %d" % regWidth
    if(nDims == 3)
      code.push "#define FPPS_#{regName}_ENTRIES_0".ljust(74) +
        " %d" % entries0
      code.push "#define FPPS_#{regName}_ENTRIES_1".ljust(74) +
        " %d" % entries1
      code.push "#define FPPS_#{regName}_ENTRIES_2".ljust(74) +
        " %d" % entries2
      if(regWidth >= 1)
        code.push "" +
          "#define FPPS_#{regName}(index0, index1, index2, qword)".ljust(74)+
          " \\\n\t((0x%07X) * ((index0) - 0)" % stride0 + " +".ljust(37) +
          " \\\n\t (0x%07X) * ((index1) - 0)" % stride1 + " +".ljust(37) +
          " \\\n\t (0x%07X) * ((index2) - 0) + ((qword) * 8)" % stride2 +
          " +".ljust(21) +
          " \\\n\t (0x%07X) + (FPPS_#{amName}_BASE))" % regOffset
      else
        code.push "" +
          "#define FPPS_#{regName}(index0, index1, index2)".ljust(74) +
            " \\\n\t((0x%07X) * ((index0) - 0)" % stride0 + " +".ljust(37)+
            " \\\n\t (0x%07X) * ((index1) - 0)" % stride1 + " +".ljust(37)+
            " \\\n\t (0x%07X) * ((index2) - 0)" % stride2 + " +".ljust(22)+
            " \\\n\t (0x%07X) + (FPPS_#{amName}_BASE))" % regOffset
      end
    end
    if(nDims == 2)
      code.push "#define FPPS_#{regName}_ENTRIES_0".ljust(74) +
        " %d" % entries0
      code.push "#define FPPS_#{regName}_ENTRIES_1".ljust(74) +
        " %d" % entries1
      if(regWidth >= 1)
        code.push "" +
          "#define FPPS_#{regName}(index0, index1, qword)".ljust(74) +
          " \\\n\t((0x%07X) * ((index0) - 0)" % stride0 + " +".ljust(37) +
          " \\\n\t (0x%07X) * ((index1) - 0) + ((qword) * 8)" % stride1 +
          " +".ljust(21) +
          " \\\n\t (0x%07X) + (FPPS_#{amName}_BASE))" % regOffset
      else
        code.push "#define FPPS_#{regName}(index0, index1)".ljust(74) +
          " \\\n\t((0x%07X) * ((index0) - 0)" % stride0 + " +".ljust(37) +
          " \\\n\t (0x%07X) * ((index1) - 0)" % stride1 + " +".ljust(22) +
          " \\\n\t (0x%07X) + (FPPS_#{amName}_BASE))" % regOffset
      end
    end
    if(nDims == 1)
      code.push "#define FPPS_#{regName}_ENTRIES".ljust(74)+" %d" % entries0
      if(regWidth >= 1)
        code.push "#define FPPS_#{regName}(index, qword)".ljust(74) +
          " \\\n\t((0x%07X) * ((index) - 0) + ((qword) * 8)" % stride0 +
          " +".ljust(22) +
          " \\\n\t (0x%07X) + (FPPS_#{amName}_BASE))" % regOffset
      else
        code.push "#define FPPS_#{regName}(index)".ljust(74) +
          " \\\n\t((0x%07X) * ((index) - 0)" % stride0 + " +".ljust(23) +
          " \\\n\t (0x%07X) + (FPPS_#{amName}_BASE))" % regOffset
      end
    end
    if(nDims == 0)
      if(regWidth >= 1)
        code.push "#define FPPS_#{regName}(qword)".ljust(74) +
          " \\\n\t(((qword) * 8) +".ljust(69) +
          " \\\n\t (0x%07X) + (FPPS_#{amName}_BASE))" % regOffset
      else
        code.push "#define FPPS_#{regName}()".ljust(74) +
          " \\\n\t((0x%07X) + (FPPS_#{amName}_BASE))" % regOffset
      end
    end
    code.push ""
    return code
  end

  def printRegField(code, regName, fld)
    fldname = fld.inst_name
    if fld.lsb == fld.msb
      code.push "#define FPPS_#{regName}_B_#{fldname}".ljust(74) +
        " %d" % fld.lsb
    else
      code.push "#define FPPS_#{regName}_L_#{fldname}".ljust(74) +
        " %d" % fld.lsb
      code.push "#define FPPS_#{regName}_H_#{fldname}".ljust(74) +
        " %d" % fld.msb
    end
    return code
  end

  def printRegVersion(code)
    reg_ver, reg_tag = get_reg_version()
    if reg_ver != nil and reg_tag != nil
      code.push "/************* REGISTER VERSION **************/"
      code.push "/* Used only by SW, not part of the register specs */"
      code.push "#define FPPS_REG_VERSION".ljust(69)+"%s" % reg_ver
      code.push "#define FPPS_REG_TAG".ljust(68)+"%s" % reg_tag
      code.push ""
    end
  end

  def wmRegParamsApi5()
    code = Array.new

    printRegVersion(code);

    # Addrmap -> register block -> register (reg or regfile)
    #
    # regname[idx0][idx1][idx2]
    #
    # am_inst = Addrmap instance
    # rf_defn = Regfile definition
    # antries = Number of register block instances
    # entries = Number of register instances in the block
    # intries = Number of indexes in a register instance

    # iterate each Addrmap instance (register block)
    @top_map.instances.each_with_index.map do |am_inst,ix|

      amName        = am_inst.inst_name.upcase;
      amBase        = am_inst.addr_base;
      amSize        = 0;
      antries       = am_inst.inst_size;

      if(antries > 1)
        amSize = antries * am_inst.addr_incr;
      else
        amSize = 1 << (am_inst.type_defn.addressbits || 0)
      end

      code = printAmParam(code, amName, amBase, amSize)

      # iterate each register instance in the register block
      rf_defn = am_inst.type_defn
      rf_defn.instances.each do |rg_inst|
        nDims       = 0;
        stride0     = 0;
        stride1     = 0;
        stride2     = 0;
        a_stride    = am_inst.addr_incr;
        e_stride    = rg_inst.addr_incr;
        entries     = rg_inst.inst_size; # Register instances
        intries     = 0;                 # Register indexes
        rg_defn     = rg_inst.type_defn;
        rf_addr     = 0;

        # Some registers are defined as regfile opposed to reg.  Adjust.
        if rg_defn.class == RDL::Regfile
          rf_addr = rg_inst.addr_base
          rg_inst = rg_inst.type_defn.instances[0]
          rg_defn = rg_inst.type_defn
          intries = rg_inst.inst_size
        end

        # NOTE: In a Regfile, rg_inst has changed to expose more indexes.
        i_stride = rg_inst.addr_incr

        # Register offset
        reg_Offset = (rf_addr + rg_inst.addr_base);

        # Register name
        regName = rg_inst.inst_name

        # base = block_addr + regfile offset + register offset from block
        base = (am_inst.addr_base + rf_addr + rg_inst.addr_base);

        # Number of qwords (all are 64-bit registers)
        width = rg_defn.regwidth / 64

        # Flags bits:
        # bit 0             1=Volatile, 0=Non-Volatile
        flags       = 0;

        entries0    = 1;
        entries1    = 1;
        entries2    = 1;
        if (antries > 1)      # Register block has multiple instances
          if (entries > 1)    # Register in the block has multiple instances
            if (intries > 1)  # Register instance has multiple indexes
              nDims = 3;      # reg[A][E][I]
              entries0 = antries;
              entries1 = entries;
              entries2 = intries;
              stride0 = a_stride;
              stride1 = e_stride;
              stride2 = i_stride;
            else              # Register instance has 1 index
              nDims = 2;      # reg[A][E]
              entries0 = antries;
              entries1 = entries;
              stride0 = a_stride;
              stride1 = e_stride;
              stride2 = 0;    # Not used
            end
          else                # Register in the block has 1 instance
            if (intries > 1)  # Register instance has multiple indexes
              nDims = 2;      # reg[A][I]
              entries0 = antries;
              entries1 = intries;
              stride0 = a_stride;
              stride1 = i_stride;
              stride2 = 0;    # Not used
            else              # Register instance has 1 index
              nDims = 1;      # reg[A]
              entries0 = antries;
              stride0 = a_stride;
              stride1 = 0;    # Not used
              stride2 = 0;    # Not used
            end
          end
        else                    # Register block has 1 instance
          if (entries > 1)    # Register in the block has multiple instances
            if (intries > 1)  # Register instance has multiple indexes
              nDims = 2;      # reg[E][I]
              entries0 = entries;
              entries1 = intries;
              stride0 = e_stride;
              stride1 = i_stride;
              stride2 = 0;
            else              # Register instance has 1 index
              nDims = 1;      # reg[E]
              entries0 = entries;
              stride0 = e_stride;
              stride1 = 0;
              stride2 = 0;
            end
          else                # Register in the block has 1 instance
            if (intries > 1)  # Register instance has multiple indexes
              nDims = 1;      # reg[I]
              entries0 = intries;
              stride0 = i_stride;
              stride1 = 0;
              stride2 = 0;
            else              # Register instance has multiple indexes
              nDims = 0;      # reg
              stride0 = 0;
              stride1 = 0;
              stride2 = 0;
            end
          end
        end

        ifdef_debug_regs(rg_inst.type_defn.name, code)

        code = printRegParam(code, amName, regName, nDims, width,
                             entries0, entries1, entries2,
                             stride0, stride1, stride2, reg_Offset)


        rg_defn.fields.each do |fld|
          code = printRegField(code, regName, fld)
        end
        code.push ""
      end

      # In case last register is debug register
      close_debug_reg_ifdef(code)
    end
    return code
  end

end # class WmRegistersView
