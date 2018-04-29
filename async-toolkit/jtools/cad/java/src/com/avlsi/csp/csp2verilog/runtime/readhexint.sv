virtual class ReadHexInts #(parameter MEM_MIN = 0,
                            parameter MEM_MAX = 0,
                            parameter MEM_WIDTH = 0,
                            parameter WIDTH = 0);
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

  static task fread(output bit signed [MEM_WIDTH-1:0] ptr[MEM_MIN:MEM_MAX],
                    input bit signed [WIDTH-1:0] size,
                    input bit signed [WIDTH-1:0] nmemb,
                    input bit signed [WIDTH-1:0] stream,
                    output bit signed [WIDTH-1:0] read);
    bit signed [WIDTH-1:0] idx = MEM_MIN;
    integer fp = stream;
    read = 0;
    for (bit signed [WIDTH-1:0] i = 0; i < nmemb; ++i) begin
      for (bit signed [WIDTH-1:0] j = 0; j < size; ++j) begin
        bit [7:0] c;
        if ($fread(c, fp) == 0) return;
        ptr[idx] = $signed({1'b0, c});
        idx = idx + 1'b1;
      end
      read = read + 1'b1;
    end
  endtask

  static task fwrite(input bit signed [MEM_WIDTH-1:0] ptr[MEM_MIN:MEM_MAX],
                     input bit signed [WIDTH-1:0] size,
                     input bit signed [WIDTH-1:0] nmemb,
                     input bit signed [WIDTH-1:0] stream,
                     output bit signed [WIDTH-1:0] wrote);
    bit signed [WIDTH-1:0] idx = MEM_MIN;
    integer fp = stream;
    wrote = 0;
    for (bit signed [WIDTH-1:0] i = 0; i < nmemb; ++i) begin
      for (bit signed [WIDTH-1:0] j = 0; j < size; ++j) begin
        bit [7:0] c = ptr[idx];
        $fwrite(fp, "%c", c);
        idx = idx + 1'b1;
      end
      wrote = wrote + 1'b1;
    end
	if (wrote > 0) $fflush(fp);
  endtask
endclass
