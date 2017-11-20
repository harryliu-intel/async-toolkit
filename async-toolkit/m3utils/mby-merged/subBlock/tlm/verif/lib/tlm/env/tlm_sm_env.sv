
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_sm_env.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
  TLM1 Soala SM imp file

  This file implemnet the TLM1 IP saola SM.
 
 This file hold:
 1. IP Address MAP - recommed to follow the common usgae.
 2. Implemenattion of Saola SM API's:
    - do_read
    - do_write
    - do_load
    -  do_dump





*/

/*
 Class: TLM1_sm_env

 TLM1 Saola SM implementation for TLM1 IP


 */

class tlm_sm_env extends slu_sm_env;

    `uvm_component_utils(tlm_sm_env)

    // bAck pointer to the TB top env
    tlm_env env;
  
    function new(string name, uvm_component parent);
        super.new(name, parent);
    endfunction // new
  

    function void connect_phase(uvm_phase phase);
        slu_tb_env env_pointer;
        super.connect_phase(phase);
        // Getting a pointer to tlm_env. This sm_env is only used in tlm cluster
        // so we don't check SLA_TOP.
        env_pointer = slu_tb_env::get_top_tb_env();
        assert ($cast (env,env_pointer ))
        else uvm_report_fatal (get_full_name(), "BAD TOP env");
    endfunction // void


  // IN end of elaburation we set the the IP addree map
    function void end_of_elaboration_phase(uvm_phase phase);
        slu_sm_am_tagmap memmap;
        slu_sm_am_tagmap cfgmap;
        slu_sm_am_tagmap iomap;

        super.end_of_elaboration_phase(phase);

        memmap = am.find_map("memmap");
        cfgmap = am.find_map("cfgmap");
        iomap = am.find_map("iomap");


      // Mem regoin MAP
      // We have "high and low" to devide between 32 address and higher.
      // INTEG : This is enable of 34 bit address MAP
      // DRAM - system memory (DDR)
      // MMIO - IO devicdes Memory space
      
        memmap.add("DRAM_LOW", 36'h0, 36'h8000_0000); // 0 - 2G DDR LOW
        memmap.add("DRAM_HIGH", 36'h1_0000_0000, 36'h1_0000_0000); // 4G - 8G DDR HIGH
        memmap.add("MMIO_LOW",  32'h8000_0000 ,36'h8000_0000 ); // 2G - 4G  MMIO (32 bits)
        memmap.add("MMIO_HIGH", 48'h2_0000_0000, 48'h10_0000_0000); // 8G - 64G - MMIO HIGH
        memmap.print_tagmap();
      // IO MAP
        iomap.add("MMIO", 32'h0 , 'h1_0000);
    endfunction // void

  /*
    Task: TLM1 SM - do_read
    
   TLM1 IP imp of Saola SM API do_read
   
   DRAM_* backdoor access will be read from  IOSF Pronary VC memory model.
   DRAM_* forntdoor access isn't imp. 
   INTEG: if the IP has extrenal interafce that can generate upstream transaction to memory (like PCIE)
          The DRAM_* can be active this interface.
   
   MMIO_* backdoor access  isn't imp 
   INTEG: if the IP has extrenal interafce has memory model. The API to this memort model should be map here.
   MMIO_* frontdoor access are mapped to IOSF promary seq <tlm_iosf_pri_basic_trans> through <read_through_iosf> task
    
   
   
    
    */

    /// do the read action
  virtual task  do_read(input addr_t addr, input addr_t length, output byte_t data[$], input string region, bit backdoor, 
   input string map_name, input string allocated_name = "", input  uvm_object user_object=null);
        case (region)
          "DRAM_LOW", "DRAM_HIGH" : begin
	    if (backdoor) begin
	      //Read the data from IOSF BFM memroy model
	      read_iosf_mem(addr,length,data);
	    end
	    else begin
	      //INTEG:: If the IP has extenal interface that can genertae 
	      //Memory transaction to the system Memory through the IP
	      // this is the place to call this task
	      uvm_report_error (get_name()," TLM1 ENV does not support yet read frontdoor to DDR");
	    end
	  end
	  
          "MMIO_LOW", "MMIO_HIGH" :begin
	    if (backdoor) begin;
	      // INTEG:: If the IP has extenal memory model
	      // here is the place to add backdoor read from it
	        uvm_report_error (get_name()," TLM1 ENV does not support yet read backdoor to MMIO");
	    end
	    else
	      // This task will active IOSF seq to do read from the DUT
	      read_through_iosf(addr,length,data);
	  end
	  
	      
          default : uvm_report_error (get_full_name(), $psprintf("TLM1_SM Do_Read Unsupported memeory region = %s",region));
        endcase // case(region)
    endtask

  
  /*
    Task: TLM1 SM - do_write
    
   TLM1 IP imp of Saola SM API do_write
   
   DRAM_* backdoor access will be load to the IOSF Pronary VC memory model.
   DRAM_* forntdoor access isn't imp. 
   INTEG: if the IP has extrenal interafce that can generate upstream transaction to memory (like PCIE)
          The DRAM_* can be active this interface.
   
   MMIO_* backdoor access  isn't imp 
   INTEG: if the IP has extrenal interafce has memory model. The API to this memort model should be map here.
   MMIO_* frontdoor access are mapped to IOSF promary seq <tlm_iosf_pri_basic_trans> through <write_through_iosf> task
   
    
    */
  virtual task do_write(input addr_t addr, input byte_t data[$], input bit be[$], input string region, bit backdoor, 
                         input string map_name, input string allocated_name = "", input  uvm_object user_object=null);
        case (region)
            "DRAM_LOW","DRAM_HIGH" :  begin
	      if (backdoor) begin
	      //Write  the data to the  IOSF BFM memroy model
		write_iosf_mem(addr,data,be);
	      end
	      else
		//INTEG:: If the IP has extenal interface that can genertae 
		//Memory transaction to the system Memory through the IP
		// this is the place to call this task
		uvm_report_error (get_name()," TLM1 ENV does not support yet write frontdoor to DDR");
	    end	      
          "MMIO_LOW",   "MMIO_HIGH" : begin
	    if (backdoor) begin
	      // INTEG:: If the IP has extenal memory model
	      // here is the place to add backdoor write to it
	     uvm_report_error (get_name()," TLM1 ENV does not support yet write frontdoor to DDR");  
	    end
	    else
	      // This task will active IOSF seq to do write to the DUT
	      write_through_iosf(addr,data);
        end   
	    
            default : uvm_report_error (get_full_name(), "TLM1_SM Do_Write Unsupported memeory region");
          endcase
    endtask // do_write


  /*
    Task: TLM1 SM - do_load
    
   TLM1 IP imp of Saola SM API do_load
   
  
   INTEG: If the IP support load memory from a file. This function must be imp.
    
   
    
    */
  virtual task do_load(input string format, input string filename, input string region, bit backdoor, input addr_t start_addr='0, 
   input string map_name, input string allocated_name = "", input  uvm_object user_object=null ); 
      uvm_report_error (get_full_name(), "TLM1_SM do_load is not implemented");
    endtask
 /*
    Task: TLM1 SM - do_dump
    
   TLM1 IP imp of Saola SM API do_dump
   
  
   INTEG: If the IP support dump  memory to  a file. This function must be imp.
    
   
    
    */
   virtual task do_dump(input string filename, input string region, bit backdoor, input string map_name, 
                        input string allocated_name = "", input  uvm_object user_object=null); 
    endtask

  /*
    Task:  TLM1 SM - read_iosf_mem
    
   TLM1 IP backdoor read from IOSF Primary VC memory model
   
   */
  
  task read_iosf_mem(input sla_pkg::addr_t addr, input sla_pkg::addr_t length, output byte_t data[$]);
    int counter;
    bit [31:0] tmp_data;
    sla_pkg::addr_t inc_addr;
    
    counter = length;
    inc_addr = addr;
    // IOSF BFM retrun data in DW whil the task API is Q of bytes
    while (counter > 0)
      begin
	//read one DW from IOSF Primary VC memory model
        for (int i = 0 ; i < 4 ; i++)
          begin
	    // While the reading length is bigger than 0 add data to the Q.
            if (counter > 0) 
              begin
                data.push_back(tmp_data[(i*8)+:8]);
                counter--;
              end
          end
	//increase address
        inc_addr = inc_addr + 4;
      end
  endtask // read_iosf_mem
  
 /*
    Task:  TLM1 SM - read_write_mem
    
   TLM1 IP backdoor write IOSF Primary VC memory model
   
   */
    task write_iosf_mem (input sla_pkg::addr_t addr, input byte_t data[$], input bit be[$]);
        bit [31:0] tmp_data;
        sla_pkg::addr_t tmp_addr;

        tmp_addr = addr;

      // IOSF BFM write  data in DW whil the task API is Q of bytes
        for (int i=0; i < data.size(); i = i + 4)
        begin
            tmp_data = 0;
            for (int j = 0; j < 4; j++) 
              begin
		// Pack the bytes into DW
                if (j+i < data.size())
                begin
                    tmp_data[(j*8)+:8] = data[j+i];
                    if (be[j+i] == 0)
                        uvm_report_error (get_full_name(), "TLM1 ENV does not support byte enables when doing backdoor writes to memory");


                end
            end
	  //read one DW from IOSF Primary VC memory model
	  
	  //increase address
          tmp_addr = tmp_addr + 4;
        end 
    endtask // write_iosf_mem
  

   /*
    Task: read_through_iosf
    
   TLM1 IP imp front door read access through IOSF primary interface
   
  
    */
  task read_through_iosf(input sla_pkg::addr_t addr, input sla_pkg::addr_t length,output byte_t data[$]);
    // IOSF seq to be used
    bit [3:0] fbe = 0;
    bit [3:0] lbe = 0;
    int added_bytes = 0; 
    sla_pkg::addr_t aligned_len;
    int new_len;

    //  Align Address & DATA  to DW 
    aligned_len = length;
    
     if (addr[1:0] + length < 4)
       new_len = addr[1:0] + length;

     else
       new_len = 4;
     
    //Claculate first Btye enable 
    for (int i = addr[1:0] ;  i<new_len ; i++) begin
	fbe[i] = 1;	
    end
    
    aligned_len = aligned_len + addr[1:0];
    added_bytes =  addr[1:0];

    //Claculate last byte enable
    if (aligned_len > 4) begin
      if (aligned_len % 4 == 0 )
	lbe = 4'b1111;
      else
	for (int i =0 ; i < (aligned_len % 4); i++)
	  lbe[i] = 1;
    end

    // Invoke IOSF seq to do read transaction
  endtask // read_through_iosf

  /*
    Task: write_through_iosf
    
   TLM1 IP imp front door write access through IOSF primary interface
   
  
    */
  task write_through_iosf(input sla_pkg::addr_t addr, input byte_t data[$]);
    // IOSF seq to be used
    byte_t  aligned_data[$];
    bit [3:0] fbe = 0;
    bit [3:0] lbe = 0;
    int align_len;
    bit       pad = (addr[1:0] !=0);
    

    //  Align Address & DATA  to DW and add pad if needed
    if ( addr[1:0]+data.size() < 4)
      align_len =  addr[1:0]+data.size();
    else
      align_len = 4;
    
    //Claculate first Btye enable
    for (int i =addr [1:0] ;  i<align_len; i++) begin
      fbe[i] = 1;
      if (pad)
	aligned_data.push_back(8'h0);
    end

    // Pas
    foreach(data[i])
      aligned_data.push_back(data[i]);
    
    //Claculate last Btye enable
    if (aligned_data.size() > 4) begin
      if (aligned_data.size() % 4 == 0)
	lbe = 4'b1111;
      else
	for (int i =0 ; i < (aligned_data.size() % 4); i++)
	  lbe[i] = 1;
    end
    
    
    
  endtask

endclass : tlm_sm_env
