 //-- NLP Assertions Disable Section
 `ifdef NO_PWR_PINS
    `define NLP_ASSERT_CTL(inst_name, sig_name, assert_name)  \
        initial begin  \
            $display("[NLP-ASSERT-OFF] Turning OFF the assertion(s) ``assert_name under the hierarchy ``inst_name");  \
            $assertoff(0, inst_name);  \
        end        \
        always@(inst_name.sig_name) begin  \
            if (inst_name.sig_name === 1'b1) begin  \
                $display("[NLP-ASSERT-OFF] Turning OFF the assertion(s) ``assert_name under the hierarchy ``inst_name");  \
                $assertoff(0, inst_name);  \
            end        \
            else if (inst_name.sig_name === 1'b0) begin  \
                $display("[NLP-ASSERT-ON] Turning ON the assertion(s) ``assert_name under the hierarchy ``inst_name");  \
                $asserton(0, inst_name);  \
            end        \
        end      
  
   `endif //-- `ifdef NO_PWR_PINS
