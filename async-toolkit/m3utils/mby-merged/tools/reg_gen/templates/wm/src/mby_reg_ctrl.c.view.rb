#
# White Model register collateral generator
#
# Usage:
# % rdlgen.rb wm_reg_ctrl top.rdl hlp_model_reg_ctrl.c
#

class MbyRegCtrlCView < RDL::ViewGen
  def initialize(top_rdl_file, view_file, template_file)
    super(top_rdl_file)           # init @top_map
    source(view_file, template_file) # init @view_code from template
    update(wmRegAttributes,         # insert new code between delimiters
           "// Auto generated reg attributes begin\n",
           "// Auto generated reg attributes end\n")
    update(wmRegDefaults,         # insert new code between delimiters
           "// Auto generated reg defaults begin\n",
           "// Auto generated reg defaults end\n")
    update(wmRegResetDomains,         # insert new code between delimiters
           "// Auto generated reg reset domains begin\n",
           "// Auto generated reg reset domains end\n")

    commit(view_file)           # save @view_code to view_file
  end

  private

  def getAddrMask(wordMaxIdx, entry0MaxIdx, entry1MaxIdx, entry2MaxIdx)
    addrMask = 0xFFFF_FFFF;

    if(entry0MaxIdx > 0)
        addrMask = (addrMask << ((Math.log2(entry0MaxIdx)).floor + 1)) & 0xFFFF_FFFF
    end
    if(entry1MaxIdx > 0)
        addrMask = (addrMask << ((Math.log2(entry1MaxIdx)).floor + 1)) & 0xFFFF_FFFF
    end
    if(entry2MaxIdx > 0)
        addrMask = (addrMask << ((Math.log2(entry2MaxIdx)).floor + 1)) & 0xFFFF_FFFF
    end
    if(wordMaxIdx > 0)
        addrMask = (addrMask << ((Math.log2(wordMaxIdx)).floor + 1)) & 0xFFFF_FFFF
        for i in 0..(Math.log2(wordMaxIdx)).floor
            addrMask |= (1<<i);
        end
    end

    addrMask <<= 2; #byte offset

    return addrMask & 0xFFFF_FFFF;
  end

  def getAddrMaskNew(max0, max1, max2, stride0, stride1, stride2)
    addrMask = 0xFFFF_FFFF;

    if(max0 > 0)
	strideBits = ((Math.log2(stride0)).floor)
	strideMask = ((1 << ((Math.log2(max0)).floor + 1)) -1) << strideBits
	addrMask = addrMask & ~strideMask;
    end
    if(max1 > 0)
	strideBits = ((Math.log2(stride1)).floor)
	strideMask = ((1 << ((Math.log2(max1)).floor + 1)) -1) << strideBits
	addrMask = addrMask & ~strideMask;
    end
    if(max2 > 0)
	strideBits = ((Math.log2(stride2)).floor)
	strideMask = ((1 << ((Math.log2(max2)).floor + 1)) -1) << strideBits
	addrMask = addrMask & ~strideMask;
    end

    return addrMask & 0xFFFF_FFFF;
  end

  def wmRegAttributes()
    code = Array.new
    nDims = 0;
    stride0 = 0;
    stride1 = 0;
    stride2 = 0;
    antries = 1;
    skip = 0;
    skipZero = 0;
    @top_map.instances.each do |am_inst|
      antries = am_inst.inst_size;
      stride2 = am_inst.addr_incr;
      amBase = am_inst.addr_base;

      rf_defn = am_inst.type_defn
      rf_defn.instances.each do |rg_inst|
        entries = rg_inst.inst_size # outer number of entries
        intries = 0                 # inner number of entries
	stride1 = rg_inst.addr_incr;
        rg_defn = rg_inst.type_defn
        rf_addr = 0
        if rg_defn.class == RDL::Regfile
          rf_addr = rg_inst.addr_base
          rg_inst = rg_inst.type_defn.instances[0]
          rg_defn = rg_inst.type_defn
          intries = rg_inst.inst_size
        end
        regName = rg_inst.inst_name
        base = ((am_inst.addr_base + rf_addr + rg_inst.addr_base));
        width = rg_defn.regwidth / 32
        #num of words
        if rg_inst.addr_incr > 0
          stride0 = rg_inst.addr_incr
        else
          stride0 = rg_defn.regwidth / 8
        end

        #if rg_inst.addr_incr > 0 && ((rg_inst.addr_incr) > (rg_defn.regwidth / 8))
        #  width = rg_inst.addr_incr / 4
        #else
        #  width = rg_defn.regwidth / 32
        #end
        #nwords = rg_defn.regwidth / 32

        #puts "REG #{regName} regWidth=0x%x" % rg_defn.regwidth
        #puts "REG #{regName} addr_incr=0x%x" % rg_inst.addr_incr
        #puts "REG #{regName} width=0x%x" % width

        max0 = 0;
        max1 = 0;
        max2 = 0;

        if(antries > 1)
            if entries > 1
              if intries > 1
                nDims = 3;
                max0 = intries-1;
                max1 = entries-1;
                max2 = antries-1;
              else
                nDims = 2;
                max0 = entries-1;
                max1 = antries-1;
		            stride1 = stride2;
              end
            else
              if intries > 1
                nDims = 2;
                max0 = intries-1;
                max1 = antries-1;
                stride1 = stride2;
              else
                nDims = 1;
                max0 = antries-1;
                stride0 = stride2;
              end
            end
        else
            if entries > 1
              if intries > 1
                nDims = 22;
                max0 = intries-1;
                max1 = entries-1;
              else
                nDims = 12;
                max0 = entries-1;
              end
            else
              if intries > 1
                nDims = 1;
                max0 = intries-1;
              else
                nDims = 0;
              end
            end
        end

        addrMask = getAddrMaskNew(max0, max1, max2, stride0, stride1, stride2);
	addrResult = 0;
        rwMask = 0;
        roMask = 0;
        cwMask = 0;
        cw1Mask = 0;
        rvMask = 0;

        rg_defn.fields.each do |fld|
            #puts "REG #{regName} fld.width = %d" % fld.width
            #puts "REG #{regName} fld.accesstype=%s" % fld.accesstype


            for i in 0..fld.width-1
                if(fld.accesstype == "RW" || fld.accesstype == "RW/V" || fld.accesstype == "WO")
                    rwMask = rwMask | (1 << (fld.lsb + i));
                end
                if(fld.accesstype == "RO" || fld.accesstype == "RO/V")
                    roMask = roMask | (1 << (fld.lsb + i));
                end
                if(fld.accesstype == "RW/C" || fld.accesstype == "RW/C/V")
                    cwMask = cwMask | (1 << (fld.lsb + i));
                end
                if(fld.accesstype == "RW/1C" || fld.accesstype == "RW/1C/V")
                    cw1Mask = cw1Mask | (1 << (fld.lsb + i));
                end
                if(fld.accesstype == "RV")
                    rvMask = rvMask | (1 << (fld.lsb + i));
                end
            end
            #puts "REG #{regName} rwMask=0x%x" % rwMask
        end
        for word in 0..width-1
            addrResult = base + word*4;
            #puts "REG #{regName} width = %d" % width
            #puts "REG #{regName} base = %d" % base

            rwMaskW = (rwMask >> 32*word) & 0xFFFF_FFFF;
            roMaskW = (roMask >> 32*word) & 0xFFFF_FFFF;
            cwMaskW = (cwMask >> 32*word) & 0xFFFF_FFFF;
            cw1MaskW = (cw1Mask >> 32*word) & 0xFFFF_FFFF;
            rvMaskW = (rvMask >> 32*word) & 0xFFFF_FFFF;
            #puts "REG #{regName} rwMaskW=0x%x" % rwMaskW
            if(rwMaskW == 0 &&
               roMaskW == 0 &&
               cwMaskW == 0 &&
               cw1MaskW == 0 &&
               rvMaskW == 0)
		#Might be better to skip with all zero
		#There are about twice as many entries matched
                skipZero = skipZero + 1;
            end
            if(rwMaskW != 0xFFFF_FFFF ||
               roMaskW != 0 ||
               cwMaskW != 0 ||
               cw1MaskW != 0 ||
               rvMaskW != 0)
                code.push sprintf("    {0x%08X, 0x%06X, 0x%08X, 0x%08X, 0x%08X, 0x%08X, 0x%08X}, /* %s [%0d]*/", addrMask, addrResult, rwMaskW, roMaskW, cwMaskW, cw1MaskW, rvMaskW, regName, word);
            end
        end

      end
    end
    return code
  end #wmRegAttributes

def wmRegDefaults()
    code = Array.new
    nDims = 0;
    stride0 = 0;
    stride1 = 0;
    stride2 = 0;
    antries = 1;
    @top_map.instances.each_with_index.map do |am_inst,ix|
      antries = am_inst.inst_size;
      stride2 = am_inst.addr_incr;
      amBase = am_inst.addr_base;

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
        width = rg_defn.regwidth / 32
        if rg_inst.addr_incr > 0
          stride0 = rg_inst.addr_incr
        else
          stride0 = rg_defn.regwidth / 8
        end
        #if rg_inst.addr_incr > 0 && ((rg_inst.addr_incr/4) > (rg_defn.regwidth / 32))
        #  width = rg_inst.addr_incr / 4
        #else
        #  width = rg_defn.regwidth / 32
        #end

        max0 = 0;
        max1 = 0;
        max2 = 0;
        if(antries > 1)
            if entries > 1
              if intries > 1
                nDims = 3;
                max0 = intries-1;
                max1 = entries-1;
                max2 = antries-1;
              else
                nDims = 2;
                max0 = entries-1;
                max1 = antries-1;
		stride1 = stride2;
              end
            else
              nDims = 1;
              max0 = antries-1;
              stride0 = stride2;
            end
        else
            if entries > 1
              if intries > 1
                nDims = 22;
                max0 = intries-1;
                max1 = entries-1;
              else
                nDims = 12;
                max0 = entries-1;
              end
            else
              nDims = 0;
            end
        end

        if(default != 0)
#if (max0 > 0)
#code.push sprintf("nDims %d stride0 %x max0 %d\n", nDims, stride0, max0)
#end
#if (max1 > 0)
#code.push sprintf("stride1 %x max1 %d\n", stride1, max1)
#end
#if (max2 > 0)
#code.push sprintf("stride2 %x max2 %d\n", stride2, max2)
#end
            addrMask = getAddrMaskNew(max0, max1, max2, stride0, stride1, stride2);
            for word in 0..width-1
                addrResult = base + word*4;
                defaultW = (default >> 32*word) & 0xFFFF_FFFF;
                if(defaultW != 0)
                    code.push sprintf("    {0x%08X, 0x%08X, 0x%08X}, /* %s [%d] */", addrMask, addrResult, defaultW, regName, word)
                end
            end
        end
      end
    end
    return code
  end

  def wmRegResetDomains()
    code = Array.new
    nDims = 0;
    stride0 = 0;
    stride1 = 0;
    stride2 = 0;
    antries = 1;
    @top_map.instances.each_with_index.map do |am_inst,ix|
      antries = am_inst.inst_size;
      stride2 = am_inst.addr_incr;
      amBase = am_inst.addr_base;

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
        width = rg_defn.regwidth / 32
        if rg_inst.addr_incr > 0
          stride0 = rg_inst.addr_incr
        else
          stride0 = rg_defn.regwidth / 8
        end
        #if rg_inst.addr_incr > 0 && ((rg_inst.addr_incr/4) > (rg_defn.regwidth / 32))
        #  width = rg_inst.addr_incr / 4
        #else
        #  width = rg_defn.regwidth / 32
        #end

        max0 = 0;
        max1 = 0;
        max2 = 0;
        if(antries > 1)
            if entries > 1
              if intries > 1
                nDims = 3;
                max0 = intries-1;
                max1 = entries-1;
                max2 = antries-1;
              else
                nDims = 2;
                max0 = entries-1;
                max1 = antries-1;
		stride1 = stride2;
              end
            else
              nDims = 1;
              max0 = antries-1;
              stride0 = stride2;
            end
        else
            if entries > 1
              if intries > 1
                nDims = 2;
                max0 = intries-1;
                max1 = entries-1;
              else
                nDims = 1;
                max0 = entries-1;
              end
            else
              nDims = 0;
            end
        end

        addrMask = getAddrMaskNew(max0, max1, max2, stride0, stride1, stride2);

        addrResult = base;
        domainMask = 1;
        # All register fields will reside in a single reset doamin
        # so it is sufficient to look at reset domain of first register field
        #rg_defn.fields[0].reset.each do |rstDomain|
        if(/DEVICE/ =~ rf_defn.resetdomains)
            domainMask = domainMask | (1 << 1);
        end
        if(/MASTER/ =~ rf_defn.resetdomains)
            domainMask = domainMask | (1 << 2);
        end
        if(/COLD_MEM/ =~ rf_defn.resetdomains)
            domainMask = domainMask | (1 << 12);
        else
            if(/COLD/ =~ rf_defn.resetdomains)
                domainMask = domainMask | (1 << 3);
            end
        end
        if(/PCIE_WARM/ =~ rf_defn.resetdomains)
            domainMask = domainMask | (1 << 4);
        end
        if(/PCIE_HOT_MEM/ =~ rf_defn.resetdomains)
            domainMask = domainMask | (1 << 14);
        else
            if(/PCIE_HOT/ =~ rf_defn.resetdomains)
                domainMask = domainMask | (1 << 5);
            end
        end
        if(/PCIE_DATAPATH_MEM/ =~ rf_defn.resetdomains)
            domainMask = domainMask | (1 << 15);
        else
            if(/PCIE_DATAPATH/ =~ rf_defn.resetdomains)
                domainMask = domainMask | (1 << 7);
            end
        end
        if(/EPL/ =~ rf_defn.resetdomains)
            domainMask = domainMask | (1 << 8);
        end
        if(/SWITCH_MEM/ =~ rf_defn.resetdomains)
            domainMask = domainMask | (1 << 17);
        else
            if(/SWITCH/ =~ rf_defn.resetdomains)
                domainMask = domainMask | (1 << 9);
            end
        end

        if(domainMask != 0)
            code.push sprintf("    {0x%08X, 0x%06X, 0x%08X}, /* %s */", addrMask, addrResult, domainMask, regName)
        end
        #rg_defn.fields.each do |fld|
        #  code = printRegField(code, regName, fld)
        #end
      end
    end
    return code
  end

end # class MbyRegCtrlCView
