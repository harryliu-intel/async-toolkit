virtual class ReadHexInts #(parameter MEM_MIN,
                            parameter MEM_MAX,
                            parameter MEM_WIDTH,
                            parameter WIDTH);
  static task readHexInts(input string filename,
                          output bit signed [MEM_WIDTH-1:0] result[MEM_MIN:MEM_MAX],
                          input bit signed [WIDTH-1:0] count,
                          output bit signed [WIDTH-1:0] read);
    reg signed [MEM_WIDTH-1:0] temp[MEM_MIN:MEM_MAX];
    bit signed [WIDTH-1:0] finish;
    bit signed [WIDTH-1:0] i;
    temp = '{default:'x};
    finish = (MEM_MIN + count) < MEM_MAX ? (MEM_MIN + count) : MEM_MAX;
    $readmemh(filename, temp, MEM_MIN, finish);
    read = MEM_MAX - MEM_MIN + 1;
    for (bit signed [WIDTH-1:0] i = MEM_MIN; i <= MEM_MAX; ++i) begin
        if (temp[i] === 'x) begin
            read = i - MEM_MIN;
            break;
        end else begin
            result[i] = temp[i];
        end
    end
  endtask
endclass
