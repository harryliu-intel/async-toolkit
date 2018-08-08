#
# HTML register documentation generator
#
# Usage:
# % rdlgen.rb html top.rdl top.html
#

class HtmlView < RDL::ViewGen

  class SecurityInfo
    SEC_FILE = ENV['MODEL_ROOT']+"/src/srdl/security.pm"
    def initialize()
      sec_hash = IO.read(SEC_FILE).scan(/%security.*^\t\);/m).shift.
                 sub(/^%/,"").
                 sub(/\(/,"{").
                 sub(/\)/,"}")
      @sec_info = eval(sec_hash)
    end
    def get(tag)
      @sec_info.include?(tag) ? @sec_info[tag].gsub(/\"/,"") : ""
    end
  end

  def initialize(top_rdl_file, view_file)
    @security_info = SecurityInfo.new
    super(top_rdl_file)         # init @top_map
    File.delete view_file if File.exist? view_file # force re-generate
    source(view_file, "html")   # init @view_code from html.template if new
    update(addrmap_html_table,  # insert new code between delimiters
           "auto-generated-addrmap",
           "/auto-generated-addrmap")
    update(regfiles_html_table,
           "auto-generated-regfiles",
           "/auto-generated-regfiles")
    update(registers_html_table,
           "auto-generated-registers",
           "/auto-generated-registers")
    commit(view_file)           # save @view_code to view_file
  end

  private

  def get_security(reg)
    tags = reg.instance_variables.keep_if{|x|x.to_s =~ /Security_/}
    tags.map do |x|
      label = x.to_s.sub(/^@/,"").sub(/_Str/,"").underscore.
              split(/_/).map{|x|x.capitalize}.join(' ')
      value = reg.instance_variable_get(x)
      value = @security_info.get(value) if label !~ /Policy Role\z/
      [label, value]
    end
  end

  def registers_html_table()
    @top_map.instances.map do |am_inst|
      am_defn = am_inst.type_defn
      am_defn.instances.map do |rg_inst|
        rg_defn = rg_inst.type_defn
        ranges  = Array.new
        strides = Array.new
        if am_inst.inst_size > 1
          ranges.push  am_inst.inst_size
          strides.push am_inst.addr_incr
        end
        if rg_inst.inst_size > 1
          ranges.push  rg_inst.inst_size
          strides.push rg_inst.addr_incr
        end
        rf_addr = 0
        if rg_defn.class == RDL::Regfile
          ranges.push rg_defn.instances[0].inst_size
          rf_addr = rg_inst.addr_base
          rg_inst = rg_inst.type_defn.instances[0]
          rg_defn = rg_inst.type_defn
          strides.push(rg_inst.addr_incr)
        end
        Html::Table.new do |tbl|
          tbl.widths   = %w(rg1 rg2 rg3 rg4 rg5 rg6)
          tbl.headings = ["Field Name","Width","Bit","Description","Type",
                          "Default"]
          rg_addr = "0x%X + 0x%X" % [am_inst.addr_base,
                                     rf_addr + rg_inst.addr_base]
          lb_addr = "Address"#[i][j]...
          if !ranges.empty?
            strides.each_with_index do |s,i|
              ix = ('i'.ord + i).chr
              lb_addr << "[#{ix}]"
              rg_addr << " + 0x#{s.to_s(16)}*#{ix} for #{ix}:0..#{ranges[i]-1}"
            end
          end
          irange = ranges.map { |x| "[0..#{x-1}]" }.join
          sec_info = get_security(rg_defn)
          tbl.spanners = [Html.r_span(5, lb_addr        , rg_addr),
                          Html.r_span(5, "Atomicity"    , rg_defn.accesswidth),
                          Html.r_span(5, "Reset Domains", am_defn.resetdomains),
                         ] + sec_info.map{|x|Html.r_span(5,*x)}
          tbl.preface  = "      "
          tbl.preface += Html.h(4, Html.a_href(am_inst.inst_name+"-desc",
                                               rg_inst.inst_name+irange,
                                               rg_inst.inst_name+"-desc"))+"\n"
          tbl.preface += Html.desc(rg_defn.desc)+"\n<p>\n"
          tbl.caption  = rg_inst.inst_name+irange
          rg_defn.fields.each do |fld|
            tbl.rows.push [fld.inst_name, fld.width.to_s,
                           "#{fld.msb}:#{fld.lsb}", Html.desc(fld.desc),
                           fld.accesstype, "0x"+fld.reset.to_s(16)]
          end
        end # Html::Table
      end # each rg_inst
    end # each am_inst
  end

  def regfiles_html_table()
    @top_map.instances.map do |am_inst|
      am_defn = am_inst.type_defn
      Html::Table.new do |tbl|
        tbl.widths   = %w(rf1 rf2 rf3 rf4)
        tbl.headings = ["Name","Brief Description","Atomicity","Address"]
        tbl.preface  = "      "+
          Html.h(2, Html.a_href("register-set-summary",
                                am_inst.inst_name+" Registers",
                                am_inst.inst_name+"-desc"))
        am_defn.instances.each do |rg_inst|
          rg_defn = rg_inst.type_defn
          range  = ""
          range += "[0..#{am_inst.inst_size-1}]" if am_inst.inst_size > 1
          range += "[0..#{rg_inst.inst_size-1}]" if rg_inst.inst_size > 1
          rf_addr = 0
          if rg_defn.class == RDL::Regfile
            range  += "[0..#{rg_defn.instances[0].inst_size-1}]"
            rf_addr = rg_inst.addr_base
            rg_inst = rg_inst.type_defn.instances[0]
            rg_defn = rg_inst.type_defn
          end
          name      = Html.a_href(rg_inst.inst_name+"-desc",
                                  rg_inst.inst_name+range)
          brief     = rg_defn.name
          atomicity = rg_defn.accesswidth.to_s
          address   = "0x%07X" % (am_inst.addr_base+rf_addr+rg_inst.addr_base)
          tbl.rows.push [name, brief, atomicity, address]
        end
      end
    end
  end

  def addrmap_html_table()
    Html::Table.new do |tbl|
      tbl.widths   = %w(am1 am2 am3 am4)
      tbl.headings = ["Block","Brief Description","Base Address","Base Size [bits]"]
      tbl.preface  = "      "+
        Html.h(2,Html.a_name("register-set-summary","Register Maps"))
      @top_map.instances.each do |ii|
        iname = ii.inst_name
        iname+= "[0..#{ii.inst_size-1}]" if ii.inst_size > 1
        block = Html.a_href(ii.inst_name+"-desc", iname, ii.inst_name)
        brief = Html.desc(ii.type_defn.desc)
        baddr = "0x%07X" % ii.addr_base
        bsize = ii.type_defn.addressbits
        tbl.rows.push [block, brief, baddr, bsize]
      end
    end
  end
end # class HtmlView

#------------------------------------------------------------------------------
# HTML table generation helper module

module Html
  class Table < Array
    attr_accessor :widths       # Array of strings
    attr_accessor :headings     # Array of strings
    attr_accessor :spanners     # Array of string, pre-table info
    attr_accessor :rows         # Array of Array of string data
    attr_accessor :preface      # String, table preface/links, etc.
    attr_accessor :caption      # String, table caption (optional)
    #
    # Construct HTML table
    #
    def initialize()
      super()                   # is-an Array
      @spanners = Array.new     # optional key-value info
      @rows = Array.new         # rows of tabular data
      yield(self) if block_given?
      push @preface if !@preface.nil?
      push "      <table>"
      push "         <caption>#{@caption}</caption>" if !@caption.nil?
      push "         <thead>"
      push @spanners
      push "            <tr>"
      @headings.each_with_index do |h,i|
        push "            <th class=\"#{@widths[i]}\">#{h}</th>"
      end
      push "            </tr>"
      push "         </thead>"
      push "         <tbody>"
      @rows.each do |r|
        push "            <tr>"
        r.each do |c|
          push "            <td>#{c}</td>"
        end
        push "            </tr>"
      end
      push "         </tbody>"
      push "      </table>"
    end
  end # class Table
  #
  # HTML helper functions
  #
  def self.a_href(link, link_text, anchor="")
    anchor = " name=\"#{anchor}\"" if !anchor.empty?
    "<a href=\"##{link}\"#{anchor}>#{link_text}</a>"
  end

  def self.a_name(link, link_text)
    "<a name=\"#{link}\">#{link_text}</a>"
  end

  def self.h(level, heading)
    "<h#{level}>#{heading}</h#{level}>"
  end

  def self.r_span(span, key, value)
    ["            <tr>",
     "               <th>#{key}</th>",
     "               <th colspan=\"#{span}\">#{value}</th>",
     "            </tr>"]
  end

  def self.desc(desc)
    term = ""
    html = Array.new
    desc.split(/\n/).map(&:strip).each do |ln|
      case ln
      when /^\w+/
        html.push ln
      when ""
        html.push term if !term.empty?
        term = ""
        html.push "<p>"
      when /^([\[\(][xA-Z0-9]+[\)\]]:?|\*)(.*)/
        type = $1.nil? ? '*' : $1
        item = $2
        if html[-1] !~ /^[<]li[>]/
          html.push case type
                    when '*', 'x'
                      term = "</ul>"
                      "<ul>"
                    when /\d+/, /[A-Z]+/
                      term = "</ol>"
                      "<ol type=\"#{type =~ /\d+/ ? '1':'A'}\">"
                    end
        end
        html.push "<li>#{item}</li>"
      when /^\.(.*)/
        html.push "<pre>#{$1}</pre>"
      when /^\+\s*(\w+):(.*)/
        word = $1
        defn = $2
        if html[-1] !~ /^[<]dt[>]/
          html.push "<dl>"
          term = "</dl>"
        end
        html.push "<dt>#{word}</dt><dd>#{defn}</dd>"
      end # case
    end # each
    html.push term if !term.empty?
    return html.join("\n")
  end
end # module Html
