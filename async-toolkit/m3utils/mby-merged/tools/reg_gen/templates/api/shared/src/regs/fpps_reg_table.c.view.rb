#
# White Model register collateral generator
#
class FppsRegTableCView < RDL::ViewGen
  def initialize(top_rdl_file, view_file, template_file)
    super(top_rdl_file)           # init @top_map
    source(view_file, template_file) # init @view_code from template

    # Get a data hash indexed by address so that we can sort the generated
    # table by address.
    #
    # data = {
    #   <address>   => {
    #       name        => "",
    #       flags       => 0,
    #       width       => 0,
    #       max0        => 0,
    #       max1        => 0,
    #       max2        => 0,
    #       stride0     => 0,
    #       stride1     => 0,
    #       stride2     => 0,
    #       debug_str   => "String to determine if debug register",
    #   },
    #   ...
    # }
    data = wmRegTableApi5
    code = Array.new
    data.sort.each {
      |addr, hash|

      ifdef_debug_regs(hash["debug_str"], code)

      code.push "\t{ \"#{hash["name"]}\",".ljust(47) +
                "0x%07X,".ljust(8) % addr +
                "0x%02X,".ljust(8) % hash["flags"] +
                "#{hash["width"]},".ljust(7) +
                "#{hash["max0"]},".ljust(7) +
                "#{hash["max1"]},".ljust(7) +
                "#{hash["max2"]},".ljust(7) +
                "0x%07X,".ljust(8) % hash["stride0"] +
                "0x%07X,".ljust(8) % hash["stride1"] +
                "0x%07X }," % hash["stride2"]
    }
    close_debug_reg_ifdef(code)

    update(code,               # insert new code between delimiters
           "Auto generated reg table begin",
           "Auto generated reg table end")
    commit(view_file)           # save @view_code to view_file
  end

  private

  def wmRegTableApi5()
    data = Hash.new

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
      # iterate each register instance in the register block
      rf_defn = am_inst.type_defn
      rf_defn.instances.each do |rg_inst|
        nDims       = 0;
        stride0     = 0;
        stride1     = 0;
        stride2     = 0;
        a_stride    = am_inst.addr_incr;
        e_stride    = rg_inst.addr_incr;
        antries     = am_inst.inst_size;
        entries     = rg_inst.inst_size # Register instances 
        intries     = 0                 # Register indexes
        rg_defn     = rg_inst.type_defn
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
        if (antries > 1)        # Register block has multiple instances
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

        max0 = entries0 - 1;
        max1 = entries1 - 1;
        max2 = entries2 - 1;
        rg_defn.fields.each do |fld|
          for _ in 0..fld.width-1
            if (fld.accesstype != "RW" && fld.accesstype != "RSV")
              # Reg is not cacheable (volatile)
              flags = flags | 1;
            end
          end
        end

        data[base] = {
          "name"          => regName,
          "flags"         => flags,
          "width"         => width,
          "max0"          => max0,
          "max1"          => max1,
          "max2"          => max2,
          "stride0"       => stride0,
          "stride1"       => stride1,
          "stride2"       => stride2,
          "debug_str"     => rg_inst.type_defn.name,
        }
      end
    end

    return data
  end

end # class WmRegTableView
