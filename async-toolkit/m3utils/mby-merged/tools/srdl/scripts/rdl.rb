#!/usr/intel/pkgs/ruby/2.2.1/bin/ruby
#
# SystemRDL parser and view generating utility
#
# Usage: rdl.rb <out-type> <inp-file> <out-file>
#
# Generator script can be found in <out-type>.rb.
#
# % rdl.rb htm_table top_map.rdl registers.html
# % rdl.rb sv_lparam top_map.rdl register_pkg.vh
# % rdl.rb sv_struct top_map.rdl register_struct.vh
#

module RDL
  require 'ostruct'             # for OpenStruct

  # ASCII encoding needed on netbatched machines
  Encoding.default_external = Encoding::UTF_8
  Encoding.default_internal = Encoding::UTF_8

  module StringExtensions
    # Convert CamelCase name to lower case underscore
    def underscore
      self.
        gsub(/([A-Z]+)([A-Z][a-z])/,'\1_\2').
        gsub(/([a-z\d])([A-Z])/,'\1_\2').
        downcase
    end
    # Convert SV numeric format to integer (Fixnum)
    def sv_i
      case self.strip.gsub(/_/,"")
      when /^0x(\w+)/i , /'h(\w+)/i
        $1.to_i(16)             # hex
      when /'b([01]+)/
        $1.to_i(2)              # binary
      when /'d(\d+)/            # decimal
        $1.to_i
      else
        self.to_i               # decimal
      end
    end
  end # module StringExtensions

  # Mix-in the above methods to the built-in String class
  String.include StringExtensions

  class Enum
    attr_accessor :type_name    # String
    attr_accessor :encodings    # Array
    def initialize(type_name, prop_token)
      # enum <type_name> { <encoding|prop_token>*; };
      @type_name = type_name
      @encodings = Array.new
      rexp = /^\s*(\w+)\s*=\s*(\d+)'d(\d+)\s*\{\s*desc\s*=\s*\"(.*?)\"/m
      prop_token.scan(rexp) do |nnm,wid,val,dsc|
        # <name> = <value> { <property|desc>*; };
        # where <value> is <width>'d<value|decimal>
        enum = OpenStruct.new
        enum.name  = nnm
        enum.value = val.sv_i
        enum.width = wid.sv_i
        enum.desc  = dsc
        @encodings.push enum
      end
    end
  end # class Enum

  class Field
    attr_accessor :accesstype   # String
    attr_accessor :desc         # String
    attr_accessor :valrandomize # Boolean
    attr_accessor :msb          # Fixnum
    attr_accessor :lsb          # Fixnum
    attr_accessor :reset        # Fixnum
    attr_accessor :inst_name    # String
    attr_accessor :width        # Fixnum
    attr_accessor :encode       # Enum
    def initialize(fld_token, enums, lsb)
      # field { <prop> } <inst_name>[<msb>:<lsb>] = <reset>;
      fld_keywords = %w(AccessType desc encode ValRandomize)
      fld_token.sub(/^\s+\}/,"").scan(/^\s*(\S+)\s*=\s*(.*?);/m) do |lhs,rhs|
        if fld_keywords.include? lhs
          instance_variable_set("@#{lhs.downcase}",rhs)
        else
          @inst_name, @msb, @lsb = lhs.scan(/\w+/)
          if @lsb.nil?
            @lsb = lsb
            if @msb.nil?
              @msb   = lsb
              @width = 1
            else
              @width = @msb.to_i
              @msb   = @lsb + @width - 1
            end
          else
            @msb   = @msb.sv_i
            @lsb   = @lsb.sv_i
            @width = @msb - @lsb + 1
          end
          rhs.strip.scan(/('?\w+)\z/) { |val,rest| @reset = val.sv_i }
        end
      end
      @accesstype = @accesstype[1..-2] # remove "s...
      @desc       = @desc[1..-2]
      if !@encode.nil?          # elaborating field encode...
        @encode = enums.find { |x| x.type_name == @encode }
        raise "Error: enum not found for #{@inst_name}" if @encode.nil?
      end
      @valrandomize = eval(@valrandomize) if !@valrandomize.nil?
    end
  end # class Field

  class Reg
    attr_accessor :type_name    # String
    attr_accessor :name         # String
    attr_accessor :desc         # String
    attr_accessor :regwidth     # Fixnum
    attr_accessor :accesswidth  # Fixnum
    attr_accessor :fields       # Array
    attr_accessor :Security_PolicyGroup     # String
    attr_accessor :Security_ReadAccess_Str  # String
    attr_accessor :Security_WriteAccess_Str # String
    attr_accessor :Security_PolicyAccess    # String
    attr_accessor :Security_PolicyRole      # String
    def initialize(type_name, prop_token, enums)
      # reg <type_name> { <prop_token> };
      @type_name = type_name
      sec_keywords  = %w(Security_PolicyGroup Security_ReadAccess_Str)
      sec_keywords += %w(Security_WriteAccess_Str)
      sec_keywords += %w(Security_PolicyAccess Security_PolicyRole)
      reg_keywords  = %w(name desc regwidth accesswidth) + sec_keywords
      prop_token.scan(/\s*(\w+)\s*=\s*(.*?);/m) do |k,v|
        # <prop_name> = <prop_value>;
        if reg_keywords.include? k
          if sec_keywords.include? k
            v =~ /['"](\w+)['"]/
            v = $1
            raise "Error: unrecognized security tag in #{type_name}" if v.nil?
          end
          instance_variable_set("@#{k}",v)
          reg_keywords.delete k
        end
      end
      @name        = @name[1..-2] # remove "s...
      @desc        = @desc[1..-2]
      @regwidth    = @regwidth.sv_i
      @accesswidth = @accesswidth.sv_i
      @fields      = Array.new
      lsb = 0
      prop_token.scan(/^\s*field\s*(\{.*?^\s+\}.*?;)/m) do |prop,rest|
        @fields.push Field.new(prop, enums, lsb) # field <prop>
        @fields.sort! { |a,b| b.lsb <=> a.lsb }
        lsb = @fields.first.msb + 1
      end
    end
  end # class Reg

  class Addrmap
    attr_accessor :type_name    # String
    attr_accessor :name         # String
    attr_accessor :desc         # String
    attr_accessor :addressing   # String
    attr_accessor :space        # String
    attr_accessor :arrayedmaps  # Fixnum
    attr_accessor :addressbits  # Fixnum
    attr_accessor :resetdomains # String
    attr_accessor :instances    # Array

    def initialize(type_name, prop_token, regs_rfs)
      @type_name = type_name
      map_keywords  = %w(name desc addressing Space AddressBits ResetDomains)
      map_keywords += %w(ArrayedMaps)
      prop_token.scan(/\s*(\w+)\s*=\s*(.*?);/m) do |k,v|
        instance_variable_set("@#{k.downcase}",v) if map_keywords.include? k
      end
      @name         = @name[1..-2]  if !@name.nil? # remove "s...
      @desc         = @desc[1..-2]  if !@desc.nil?
      @desc       ||= ""
      @space        = @space[1..-2] if !@space.nil?
      @arrayedmaps  = @arrayedmaps.nil? ? 1 : @arrayedmaps.sv_i
      @addressbits  = @addressbits.sv_i if !@addressbits.nil?
      @resetdomains = @resetdomains[1..-2] if !@resetdomains.nil?
      @instances = Array.new
      # <type_name> <inst_name>[<inst_size>] @0x<addr_base> += <addr_incr>
      prop_token.scan(/^\s*(\w+)\s+(\w+)(.*?);/) do |tt,ii,addr_alloc|
        inst = OpenStruct.new(:inst_size => 1,
                              :addr_base => 0,
                              :addr_incr => 8) # 64b assumed
        inst.type_name = tt
        inst.inst_name = ii
        inst.inst_size = $1.sv_i if addr_alloc =~ /\[('?\w+)\]/
        inst.addr_base = $1.sv_i if addr_alloc =~ /@('?\w+)/
        inst.addr_incr = $1.sv_i if addr_alloc =~ /\+=\s*('?\w+)/
        inst.type_defn = regs_rfs.find { |x| x.type_name == tt }
        @instances.push inst
      end # scan
    end # initialize()
  end # class Addrmap

  class Regfile < Addrmap
    def initialize(type_name, prop_token, registers)
      # Note: use of regfile limits to an array of a register
      super(type_name, prop_token, registers)
    end
  end # class Regfile

  class Parser
    attr_accessor :addrmaps     # Array
    def initialize(rdl_file)
      enums     = Array.new
      registers = Array.new
      regfiles  = Array.new
      addrmaps  = Array.new
      rdl = rdl_file.class == String && rdl_file || IO.read(rdl_file)
      rdl.gsub!(/\/\/.*/,"")
      rdl.gsub!(/^\s*lsm_IMN_map\s+\w+.*/,"") # FIXME
      rdl.scan(/^enum\s+(\w+)\s*(\{.*?^\};)/m    ) do |tn,bdy|
        enums.push Enum.new(tn, bdy)
      end
      rdl.scan(/^reg\s+(\w+)\s*(\{.*?^\};)/m     ) do |tn,bdy|
        registers.push Reg.new(tn, bdy, enums)
      end
      rdl.scan(/^regfile\s+(\w+)\s*(\{.*?^\};)/m ) do |tn,bdy|
        regfiles.push Regfile.new(tn, bdy, registers)
      end
      rdl.scan(/^addrmap\s+(\w+)\s*(\{.*?^\};)/m ) do |tn,bdy|
        addrmaps.push Addrmap.new(tn, bdy, registers+regfiles)
      end
      @addrmaps = addrmaps
    end
  end # class Parser

  class Reader
    attr_reader :top_map
    def initialize(rdl_file)
      rdl_file, addrmap = rdl_file.split(/:/)
      parser = RDL::Parser.new( read_include_all(rdl_file,{}) )
      @top_map = parser.addrmaps[-1]
      @top_map = parser.addrmaps.find{|x|x.type_name==addrmap} if !addrmap.nil?
      @top_map.instances.each do |am_inst|
        am_defn = parser.addrmaps.find do |x|
          x.type_name == am_inst.type_name
        end
        next if am_defn.nil?
        am_flat = am_defn.instances.map do |morr|
          if morr.type_name =~ /_map\z/
            morr.type_defn = parser.addrmaps.find do |am|
              am.type_name == morr.type_name
            end
            morr.type_defn.instances.map do |i|
              i.addr_base += morr.addr_base; i
            end
          else
            morr
          end
        end.flatten
        am_defn.instances = am_flat
        am_inst.type_defn = am_defn
      end
    end
    def read_include_all(rdl_file, readit)
      return [] if rdl_file =~ /lsm_cfg.rdl/ # FIXME
      return [] if !File.exist?(rdl_file)
      IO.readlines(rdl_file).map do |ln|
        if ln =~ /^\s*`include\s+\"(.*?)\"/
          readit[$1] ? [] : read_include_all($1, readit[$1] = true && readit)
        else
          ln
        end
      end.join
    end
  end # class Reader

  class ViewGen
    MAP_CONFIG = {"epl_pb_map_regs" => 0}
    def initialize(rdl_file, view_file="", addrmap="")
      rdl_file += ":#{addrmap}" if !addrmap.empty?
      @top_map  = RDL::Reader.new(rdl_file).top_map
      elaborate() if @top_map.instances[0].type_defn.class == RDL::Addrmap
    end

    # Create new output view file from a template
    def create(output_file, template)
      source("#{File.dirname(__FILE__)}/#{template}.template")
      commit(output_file)
    end

    # Read output_file into @view_code Array
    def source(output_file, template="")
      if File.exist? output_file
        @view_code = IO.readlines(output_file)
      else
        if File.exist?("#{File.dirname(__FILE__)}/#{template}.template")
          create(output_file, template)
        else
          raise "Error: Can't find template #{template}"
        end
      end
    end

    # Insert new_code into in-memory @view_code between delimiters
    def update(new_code, start, stop)
      return if new_code.nil?
      re1 = Regexp.new(start)
      re2 = Regexp.new(stop)
      cant_find_marker = false
      index1 = @view_code.index{|e|e =~ re1} || (cant_find_marker=true)
      index2 = @view_code.index{|e|e =~ re2} || (cant_find_marker=true)
      return if cant_find_marker
      prolog = @view_code[0..index1]
      epilog = @view_code[index2..-1]
      @view_code = prolog + new_code + epilog
    end

    # Write updated in-memory @view_code to output_file
    def commit(output_file)
      File.open(output_file,"w") do |fd|
        fd.puts @view_code
      end
    end

    # Search/replace template markers
    def replace(regex, repl, nofurther=100)
      nofurther = @view_code.size if nofurther < 100
      if (ndx = @view_code[0,nofurther].index{|x|x=~/#{regex}/})
        @view_code[ndx].sub!(/#{regex}/, repl)
      end
    end

    # Justify code array on token
    def justify(code, token='=')
      regexp = Regexp.new(token)
      spaces = code.map { |e| e.index regexp }
      mspace = spaces.max
      justified_code = Array.new
      spaces.each_with_index do |spc,i|
        justified_code.push code[i].sub(regexp, " "*(mspace-spc) + token)
      end
      return justified_code
    end

    private

    def elaborate()
      rg_prev = OpenStruct.new(:inst_size => 1,
                               :addr_incr => 0,
                               :addr_base => 0,
                               :entries0  => 1,
                               :entries1  => 1)
      each_reg_info() do |ri|
        rg_inst = ri.rg_inst
        rg_inst.addr_incr = 1 << Math.log2(rg_inst.type_defn.regwidth/8).ceil
        prev_incr  = (1 << Math.log2(rg_prev.inst_size).ceil) * rg_prev.addr_incr
        prev_incr *= (1 << Math.log2(rg_prev.entries1).ceil) if rg_prev.entries1 > 1
        this_incr  = (1 << Math.log2(rg_inst.inst_size).ceil) * rg_inst.addr_incr
        this_incr *= (1 << Math.log2(ri.entries1).ceil) if ri.entries1 > 1
        if ri.first
          rg_inst.addr_base = 0
        elsif ri.entries1 > 1
          incr = [prev_incr,this_incr].max
          mask = ~(incr - 1)
          rg_inst.addr_base = (rg_prev.addr_base & mask) + incr
        else
          rg_inst.addr_base =  rg_prev.addr_base + [prev_incr,this_incr].max
        end
        rg_prev = rg_inst.dup
        rg_prev.entries0 = ri.entries0
        rg_prev.entries1 = ri.entries1
      end
    end # elaborate()

    def each_reg_info()
      @top_map.instances.each do |am_inst|
        am_inst.type_defn.instances.each_with_index do |rg_inst,id|
          rg_defn  = rg_inst.type_defn
          entries0 = rg_inst.inst_size
          entries1 = 0
          if rg_defn.class == RDL::Regfile
            rg_inst  = rg_inst.type_defn.instances[0]
            rg_defn  = rg_inst.type_defn
            entries1, entries0 = entries0, rg_inst.inst_size
          end
          reg_info = OpenStruct.new
          reg_info.rg_inst  = rg_inst
          reg_info.entries1 = entries1
          reg_info.entries0 = entries0
          reg_info.first    = (id == 0)
          yield(reg_info) if block_given?
        end
      end
    end # each_reg_info
  end # class ViewGen

  # rdl.rb <out-type> <inp-file> <out-file>
  class Generator
    def initialize(argv)
      $LOAD_PATH.push File.dirname(__FILE__)
      require (out_type = argv.shift)+".rb"
      out_class = out_type.split(/_/).map(&:capitalize).join
      eval( "#{out_class}.new(*argv)" )
    end
  end # class Conversion
end # module RDL

# Run RDL generator if this file is executed
RDL::Generator.new(ARGV) if $0 == __FILE__
