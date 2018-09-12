#
# White Model register collateral generator
#
# Usage:
# % rdlgen.rb wm_reg_table top.rdl hlp_debug_reg_table.c
#

class HlpDebugRegTableCView < RDL::ViewGen
  def initialize(top_rdl_file, view_file, template_file)
    super(top_rdl_file)           # init @top_map
    source(view_file, template_file) # init @view_code from template
    update(wmRegTable,         # insert new code between delimiters
           "    // Auto generated reg table begin\n",
           "    // Auto generated reg table end\n")
    commit(view_file)           # save @view_code to view_file
  end

  private
  
  def wmRegTable()
    code = Array.new
    nDims = 0;
    stride0 = 0;
    stride1 = 0;
    stride2 = 0;
    min0 = 0;
    min1 = 0;
    min2 = 0;
    max0 = 0;
    max1 = 0;
    max2 = 0;
    flags = 0;
    group = 0;
    numPorts = 24;
    numEpls = 9;
    numLanes = 4;
    antries = 1;
    @top_map.instances.each_with_index.map do |am_inst,ix|
      antries = am_inst.inst_size;
      stride2 = am_inst.addr_incr;
      
      rf_defn = am_inst.type_defn
      rf_defn.instances.each do |rg_inst|
        antries = am_inst.inst_size;
        stride2 = am_inst.addr_incr;

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
        regName = rg_inst.inst_name
        base = (am_inst.addr_base + rf_addr + rg_inst.addr_base);
        width = rg_defn.regwidth / 32
        if(rg_inst.addr_incr > 0)
            stride0 = rg_inst.addr_incr
        else    
            stride0 = rg_defn.regwidth / 8
        end
        
        #puts "REGT #{rg_inst.inst_name} stride0 = 0x%08X" % stride0
        #puts "REGT #{rg_inst.inst_name} stride1 = 0x%08X" % stride1
        #puts "REGT #{rg_inst.inst_name} stride2 = 0x%08X" % stride2
        #here the initial order is entries0 entries1 entries2, stride2(map) stride1(rf) stride0(reg) 
        min0 = 0;
        min1 = 0;
        min2 = 0;
        flags = 0;
        group = 0;
        numPorts = 32;
        numEpls = 8;
        numLanes = 4;
        entries0 = 1;
        entries1 = 1;
        entries2 = 1;
        if(antries > 1)
            if entries > 1
              if intries > 1
                nDims = 3;
                entries0 = antries;
                entries1 = entries;
                entries2 = intries;
		            tmp = stride0;
		            stride0 = stride2;
		            stride2 = tmp;
              else
                nDims = 2;
                entries0 = antries;
                entries1 = entries;
		            stride1 = stride0;
		            stride0 = stride2;
                stride2 = 0;
              end
            else
              if intries > 1
                nDims = 2;
                entries0 = antries;
                entries1 = intries;
                stride1 = stride0;
                stride0 = stride2;
                stride2 = 0;
              else
                nDims = 1;
                entries0 = antries;
                stride0 = stride2;
                stride1 = 0;
                stride2 = 0;
              end
            end
        else
            if entries > 1
              if intries > 1
                nDims = 2;
                entries0 = entries;
                entries1 = intries;
                stride2 = stride0;
		            stride0 = stride1;
		            stride1 = stride2;
                stride2 = 0;
              else
                nDims = 1;
                entries0 = entries;
                stride2 = 0;
                stride1 = 0;
              end
            else
              if intries > 1
                nDims = 1;
                entries0 = intries;
                stride1 = 0;
                stride2 = 0;
              else
                nDims = 0;
                stride2 = 0;
                stride1 = 0;
              end
            end
        end    
        
        max0 = entries0 - 1;
        max1 = entries1 - 1;
        max2 = entries2 - 1;
        if(nDims == 3)
           if(width > 1)
               mName = "MWTPLIDX"
           else
               mName = "TPLINDEX"
           end
           if(entries1 == numPorts)
               flags = 2;
           end 
           if(entries0 == numPorts)
                flags = 1;
           end
        end    
        if(nDims == 2)
           if(width > 1)
               mName = "MWDBLIDX"
           else
               mName = "DBLINDEX"
           end
           if(entries1 == numPorts)
               flags = 2;
           end 
           if (entries1 == numEpls && entries0 == numLanes)
               flags = 8
           end
        end    
        if(nDims == 1)
            if(width > 1)
                mName = "MWINDEX"
            else
                mName = "INDEXED"
            end
            if(entries0 == numPorts)
                flags = 1;
            end
            if (entries0 == numEpls)
                flags = 4 
            end
        end
        if(nDims == 0)
          if(width > 1)
            mName = "MULTIWRD"
          else
            mName = "SCALAR"
          end  
        end
        
        if(/STATS/ =~ regName)
            flags = flags | 16
        end    
        code.push "    { \"HLP_#{regName}\",".ljust(52)+"#{mName},".ljust(10)+"0x%07X,".ljust(8) % base +"0x%02X,".ljust(8) % flags +"#{group},".ljust(6)+"#{width},".ljust(6)+"#{min0},".ljust(7)+"#{min1},".ljust(7)+"#{min2},".ljust(7)+"#{max0},".ljust(7)+"#{max1},".ljust(7)+"#{max2},".ljust(7)+"0x%07X,".ljust(10) % stride0 +"0x%07X,".ljust(10) % stride1 + "0x%07X }," % stride2 
      end
    end

    return code
  end

end # class WmRegTableView
