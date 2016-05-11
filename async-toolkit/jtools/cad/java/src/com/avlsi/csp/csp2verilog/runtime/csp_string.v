// vim:et:sw=4:ts=4:tw=79:

`ifndef CSP_STRING_MAX
// a "normal" screen is 24*80 = 1920 characters big; so this should be a fine
// default value
`define CSP_STRING_MAX 2047
`endif

// CSP_STRING_COUNTER_SIZE should be the number of bits that is large enough to
// hold a number in the range 0..CSP_STRING_MAX; setting to 32-bits should be
// plenty big
`define CSP_STRING_COUNTER_SIZE 32

// No user serviceable part below ---------------------------------------------
`define CSP_STRING_WHOLE `CSP_STRING_MAX * 8 + `CSP_STRING_COUNTER_SIZE:1
`define CSP_STRING_LENGTH `CSP_STRING_MAX * 8 + `CSP_STRING_COUNTER_SIZE:`CSP_STRING_MAX * 8 + 1
`define CSP_STRING_ASCII `CSP_STRING_MAX * 8:1
`define CSP_STRING reg [`CSP_STRING_WHOLE]

function automatic string __csp_to_sv_string (input `CSP_STRING s);
    // local arguments
    string  c;
    integer i;
    integer length;
begin
    __csp_to_sv_string = "";
    length = s[`CSP_STRING_LENGTH];
    for (i = 0; i < length; i++)
    begin
        c = $psprintf("%c", s[(length - i) * 8 -: 8]);
        __csp_to_sv_string = {__csp_to_sv_string, c};
    end
end
endfunction : __csp_to_sv_string


module csp_string;

function [`CSP_STRING_WHOLE] init(input integer size,
                                  input [`CSP_STRING_ASCII] value);
begin
    init = 0;
    // in VCS, $bits("") == 8, correct for that case
    if (size == 8 && value == 0) size = 0;
    if (size / 8 > `CSP_STRING_MAX) begin
`ifdef CSP_STRING_WARNING
        $display("%t:%m: CSP string constant truncated to maximum string size", $time);
`endif
        init[`CSP_STRING_LENGTH] = `CSP_STRING_MAX;
    end
    else begin
        init[`CSP_STRING_LENGTH] = size / 8;
    end
    init[`CSP_STRING_ASCII] = value;
end
endfunction

function [`CSP_STRING_COUNTER_SIZE:1] bits(input [`CSP_STRING_ASCII] value);
integer i;
begin
    i = 0;
    while (i < `CSP_STRING_MAX && value[(i + 1) * 8 -: 8] != 0) i = i + 1;
    bits = i * 8;
end
endfunction

function [`CSP_STRING_WHOLE] inits(input [`CSP_STRING_ASCII] value);
inits = init(bits(value), value);
endfunction

task automatic write;
input s;
`CSP_STRING s;
integer i, l;
begin
    $write("%s", __csp_to_sv_string(s));
end
endtask


function [`CSP_STRING_WHOLE] concat(input [`CSP_STRING_WHOLE] s1,
                                    input [`CSP_STRING_WHOLE] s2);
integer i, l1, l2, l3;
begin
    l1 = s1[`CSP_STRING_LENGTH];
    l2 = s2[`CSP_STRING_LENGTH];
    if (l1 + l2 > `CSP_STRING_MAX) begin
`ifdef CSP_STRING_WARNING
        $display("%t:%m: CSP string concatenation truncated to maximum string size", $time);
`endif
        l2 = `CSP_STRING_MAX - l1;
    end
    l3 = l1 + l2;
    concat = 0;
    concat[`CSP_STRING_LENGTH] = l3;
    for (i = 0; i < l1; i = i + 1) begin
        concat[(l3 - i) * 8 -: 8] = s1[(l1 - i) * 8 -: 8];
    end

    for (i = 0; i < l2; i = i + 1) begin
        concat[(l3 - i - l1) * 8 -: 8] = 
            s2[(s2[`CSP_STRING_LENGTH] - i) * 8 -: 8];
    end
end
endfunction

function [`CSP_STRING_WHOLE] reverse(input [`CSP_STRING_WHOLE] s);
integer i, l;
begin
    l = s[`CSP_STRING_LENGTH];
    for (i = 0; i < l; i = i + 1) begin
        reverse[(i + 1) * 8 -: 8] = s[(l - i) * 8 -: 8];
    end
    reverse[`CSP_STRING_LENGTH] = s[`CSP_STRING_LENGTH];
end
endfunction

function [`CSP_STRING_WHOLE] ord(input [`CSP_STRING_WHOLE] val);
`CSP_STRING rev;
begin
    rev = reverse(val);
    ord = rev[`CSP_STRING_ASCII];
end
endfunction

function [`CSP_STRING_WHOLE] chr(input [`CSP_STRING_ASCII] val);
begin
    chr = reverse(inits(val));
end
endfunction

endmodule

`ifdef CSP_STRING_SELF_TEST

`define CSP_DISPLAY(varname)                                                  \
    repeat (1)                                                                \
    begin                                                                     \
        var string __str;                                                     \
        __str = __csp_to_sv_string(varname);                                  \
        $display("display: varname = %s", __str);                             \
    end

`define CSP_WRITE(varname)                                                    \
    $write("write: varname = "); csp_string.write(varname); $write("\n");

module test2;
`CSP_STRING var1;
`CSP_STRING var2;
`CSP_STRING var3;
`CSP_STRING var4;
initial begin
    $display("Testing csp_string.init...");
    var1 = csp_string.init($bits("01234567890"), "01234567890");
    `CSP_WRITE(var1); `CSP_DISPLAY(var1);
    var2 = csp_string.init($bits("89abcdef"), "89abcdef");
    `CSP_WRITE(var2); `CSP_DISPLAY(var2);
    $display("Testing csp_string.init + csp_string.concat...");
    var3 = csp_string.concat(var1, var2);
    `CSP_WRITE(var3); `CSP_DISPLAY(var3);
    var4 = csp_string.concat(csp_string.init($bits(""), ""),
                             csp_string.init($bits("0123"), "0123"));
    `CSP_WRITE(var4); `CSP_DISPLAY(var4);

    $display("Testing csp_string.inits...");
    var1 = csp_string.inits("01234567890");
    `CSP_WRITE(var1); `CSP_DISPLAY(var1);
    var2 = csp_string.inits("89abcdef");
    `CSP_WRITE(var2); `CSP_DISPLAY(var2);
    $display("Testing csp_string.inits + csp_string.concat...");
    var3 = csp_string.concat(var1, csp_string.inits("<--"));
    `CSP_WRITE(var3); `CSP_DISPLAY(var3);
end
endmodule

`endif  // CSP_STRING_SELF_TEST
