#
# SystemRDL generator for MGM shell controller address maps
#
# Usage:
# % rdl.rb rdl_mgm_map <map-out-file>.rdl
#

class RdlMgmMap < RDL::ViewGen

  # These will not have '<blk>_' prefix
  NONBLKS = %w()

  # These create multiple ARP instances
  STRIPES = {"MAC4" => 8, "MSEC" => 8}

  # For TCAMs, use different cfg_r register type
  TCAMLST = %w()

  class Logical < Hash
    require "yaml"
    def initialize(logical_file)
      logical_str = IO.read(logical_file).split(/\n/).map do |ln|
        ln.strip.empty? ? nil : ln.sub(/=/,":").split
      end.compact
      logical_str.map!{|ln|ln[0..-3].join('-')+' '+ln[-2..-1].join(' ')}
      logical_yml = YAML.load(logical_str.join("\n"))
      logical_yml["Name"] = File.basename(logical_file,".*")
      logical_yml["Memory-Instances"] ||= 1
      self.merge!(logical_yml)
    end
  end

  def initialize(view_file, template="rdl_mgm_map")
    source(view_file, template) # init @view_code from template
    update(shell_ctl_addrmaps,  # insert new code inside delimiters
           "Auto-generated addrmaps begin",
           "Auto-generated addrmaps end")
    commit(view_file)           # save @view_code to view_file
  end

  private

  def shell_ctl_register(insts, type, name, addr)
    addr_bits = Math.log2(insts*8).ceil
    type_name = type.upcase
    type_hash = {"cfg"=>"CONFIG","tcam_cfg"=>"CONFIG","cerr_cnt"=>"ECC_COR_ERR","uerr_cnt"=>"ECC_UNCOR_ERR"}
    type_name = type_hash[type] if type_hash[type]
    line  = "  shell_ctl_#{type}_r".ljust(25)+"#{name}_#{type_name}"
    line += "[#{insts}]" if insts > 1
    # Just use implicit addressing for simplicity (keep track of the offset here, for future code, if necessary)
    line  = (line.ljust(60) + ";")
    return line, addr + (type =~ /_cnt/ ? 8 : 2**addr_bits)
  end

  def shell_ctl_addrmaps()
    code = []
    aprs = {}
    arys = STRIPES
    Dir.chdir("../../tools/mgm/logicals") do
      find_logical_files = "\\find . -regex '.*\\.\\(custom_\\)?logical'"
      `#{find_logical_files}`.split(/\n/).sort.each do |fn|
        aprs[File.dirname(fn)] ||= []
        aprs[File.dirname(fn)].push Logical.new(fn)
      end
    end
    aprs.each do |apr_dir,apr_logicals|
      apr = apr_dir.split(/\W/)[-1].upcase
      am_name = apr+"_SHELL_CTL"
      if ! NONBLKS.include?(apr)
        am_name = apr+"_SHELL_CTL"
      end
      code << "};"
      code << "addrmap #{am_name.downcase}_map {"
      code << "  name = \"#{am_name}\";"
      code << "  desc = \"#{apr} Shell Controller Registers\";"
      code << '  addressing = regalign;'
      code << '  Space = "MSG";'
      code << '  Opcode = "MEM-SB";'
      code << '  No_IOSF_Primary = true;'
      if arys.keys.include? apr
        code << "  ArrayedMaps = #{arys[apr]};"
      end
      code << '  AddressBits = n/a;' # to be updated later...
      size = code.size
      code << '  ResetDomains = "MGMT";'
      addr = 0
      # CERR register
      line, addr = shell_ctl_register(1,"cerr_cnt",apr,addr)
      code << line
      # UERR register
      line, addr = shell_ctl_register(1,"uerr_cnt",apr,addr)
      code << line
      apr_logicals.each do |logical|
        mem_insts = logical["Memory-Instances"]
        reg_name  = logical["Name"].upcase
        addr_bits = Math.log2(mem_insts*8).ceil
        addr = ((addr >> addr_bits) + 1) << addr_bits if mem_insts > 2
        # Status registers
        line, addr = shell_ctl_register(mem_insts,"status",reg_name,addr)
        code << line
        # Config registers
        cfg_type   = TCAMLST.include?(reg_name) ? "tcam_cfg" : "cfg"
        line, addr = shell_ctl_register(mem_insts,cfg_type,reg_name,addr)
        code << line
        if logical["Debug-Read"]
          # Debug read ctl registers
          line, addr = shell_ctl_register(mem_insts,"dbg_ctl",reg_name,addr)
          code << line
          # Debug read data registers
          line, addr = shell_ctl_register(mem_insts,"dbg_data",reg_name,addr)
          code << line
        end
      end
      code[size-1] = "  AddressBits = %0d;" % Math.log2(addr).ceil
    end
    code.push("};").shift
    return code.flatten
  end
end # class RdlMgmMap
