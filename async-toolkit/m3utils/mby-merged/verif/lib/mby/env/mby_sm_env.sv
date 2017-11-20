
/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_sm_env.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
  MBY Soala SM imp file

  This file implemnet the MBY IP saola SM.
 
 This file hold:
 1. IP Address MAP - recommed to follow the common usgae.
 2. Implemenattion of Saola SM API's:
    - do_read
    - do_write
    - do_load
    -  do_dump





*/

/*
 Class: MBY_sm_env

 MBY Saola SM implementation for MBY IP


 */

class mby_sm_env extends sla_sm_env;

    `ovm_component_utils(mby_sm_env)

    // bAck pointer to the TB top env
    mby_env env;
  
    function new(string name, ovm_component parent);
        super.new(name, parent);
    endfunction // new
  

    function void connect();
        sla_tb_env env_pointer;
        super.connect();
        // Getting a pointer to mby_env. This sm_env is only used in mby cluster
        // so we don't check SLA_TOP.
        env_pointer = sla_tb_env::get_top_tb_env();
        assert ($cast (env,env_pointer ))
        else ovm_report_fatal (get_full_name(), "BAD TOP env");
    endfunction // void


  // IN end of elaburation we set the the IP addree map
    function void end_of_elaboration();
        sla_sm_am_tagmap memmap;
        sla_sm_am_tagmap cfgmap;
        sla_sm_am_tagmap iomap;

        super.end_of_elaboration();

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
    Task: MBY SM - do_read
    
   MBY IP imp of Saola SM API do_read
   
   DRAM_* backdoor access will be read from  IOSF Pronary VC memory model.
   DRAM_* forntdoor access isn't imp. 
   INTEG: if the IP has extrenal interafce that can generate upstream transaction to memory (like PCIE)
          The DRAM_* can be active this interface.
   
   MMIO_* backdoor access  isn't imp 
   INTEG: if the IP has extrenal interafce has memory model. The API to this memort model should be map here.
   MMIO_* frontdoor access are mapped to IOSF promary seq <mby_iosf_pri_basic_trans> through <read_through_iosf> task
    
   
   
    
    */

    /// do the read action
  virtual task  do_read(input addr_t addr, input addr_t length, output byte_t data[$], input string region, bit backdoor, 
   input string map_name, input string allocated_name = "", input  ovm_object user_object=null);
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
	      ovm_report_error (get_name()," MBY ENV does not support yet read frontdoor to DDR");
	    end
	  end
	  
          "MMIO_LOW", "MMIO_HIGH" :begin
	    if (backdoor) begin;
	      // INTEG:: If the IP has extenal memory model
	      // here is the place to add backdoor read from it
	        ovm_report_error (get_name()," MBY ENV does not support yet read backdoor to MMIO");
	    end
	    else
	      // This task will active IOSF seq to do read from the DUT
	      read_through_iosf(addr,length,data);
	  end
	  
	      
          default : ovm_report_error (get_full_name(), $psprintf("MBY_SM Do_Read Unsupported memeory region = %s",region));
        endcase // case(region)
    endtask

  
  /*
    Task: MBY SM - do_write
    
   MBY IP imp of Saola SM API do_write
   
   DRAM_* backdoor access will be load to the IOSF Pronary VC memory model.
   DRAM_* forntdoor access isn't imp. 
   INTEG: if the IP has extrenal interafce that can generate upstream transaction to memory (like PCIE)
          The DRAM_* can be active this interface.
   
   MMIO_* backdoor access  isn't imp 
   INTEG: if the IP has extrenal interafce has memory model. The API to this memort model should be map here.
   MMIO_* frontdoor access are mapped to IOSF promary seq <mby_iosf_pri_basic_trans> through <write_through_iosf> task
   
    
    */
  virtual task do_write(input addr_t addr, input byte_t data[$], input bit be[$], input string region, bit backdoor, 
                         input string map_name, input string allocated_name = "", input  ovm_object user_object=null);
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
		ovm_report_error (get_name()," MBY ENV does not support yet write frontdoor to DDR");
	    end	      
          "MMIO_LOW",   "MMIO_HIGH" : begin
	    if (backdoor) begin
	      // INTEG:: If the IP has extenal memory model
	      // here is the place to add backdoor write to it
	     ovm_report_error (get_name()," MBY ENV does not support yet write frontdoor to DDR");  
	    end
	    else
	      // This task will active IOSF seq to do write to the DUT
	      write_through_iosf(addr,data);
        end   
	    
            default : ovm_report_error (get_full_name(), "MBY_SM Do_Write Unsupported memeory region");
          endcase
    endtask // do_write


  /*
    Task: MBY SM - do_load
    
   MBY IP imp of Saola SM API do_load
   
  
   INTEG: If the IP support load memory from a file. This function must be imp.
    
   
    
    */
  virtual task do_load(input string format, input string filename, input string region, bit backdoor, input addr_t start_addr='0, 
   input string map_name, input string allocated_name = "", input  ovm_object user_object=null ); 
      ovm_report_error (get_full_name(), "MBY_SM do_load is not implemented");
    endtask
 /*
    Task: MBY SM - do_dump
    
   MBY IP imp of Saola SM API do_dump
   
  
   INTEG: If the IP support dump  memory to  a file. This function must be imp.
    
   
    
    */
   virtual task do_dump(input string filename, input string region, bit backdoor, input string map_name, 
                        input string allocated_name = "", input  ovm_object user_object=null); 
    endtask

  /*
    Task:  MBY SM - read_iosf_mem
    
   MBY IP backdoor read from IOSF Primary VC memory model
   
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
// START IOSF_NOT_PRESENT
        tmp_data = env.MBYIosfPriVc.readSlaveMemory(Iosf::MEM , inc_addr);
// END IOSF_NOT_PRESENT
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
    Task:  MBY SM - read_write_mem
    
   MBY IP backdoor write IOSF Primary VC memory model
   
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
                        ovm_report_error (get_full_name(), "MBY ENV does not support byte enables when doing backdoor writes to memory");


                end
            end
	  //read one DW from IOSF Primary VC memory model
// START IOSF_NOT_PRESENT
          env.MBYIosfPriVc.writeSlaveMemory(Iosf::MEM, tmp_addr, tmp_data);
// END IOSF_NOT_PRESENT
	  
	  //increase address
          tmp_addr = tmp_addr + 4;
        end 
    endtask // write_iosf_mem
  

   /*
    Task: read_through_iosf
    
   MBY IP imp front door read access through IOSF primary interface
   
  
    */
  task read_through_iosf(input sla_pkg::addr_t addr, input sla_pkg::addr_t length,output byte_t data[$]);
    // IOSF seq to be used
// START IOSF_NOT_PRESENT
    mby_iosf_pri_basic_trans iosf_seq;
// END IOSF_NOT_PRESENT
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
// START IOSF_NOT_PRESENT
    iosf_seq = new("MBY_SM_SEQ");
    iosf_seq.randomize() with {
			       m_cmd == (addr >= 36'h1_0000_0000 ? Iosf::MRd64 :Iosf::MRd32);
    	      		       m_address == addr;
			       m_data.size() == aligned_len/4 + (aligned_len%4 > 0 );
			       m_first_byte_en == fbe;
			       m_last_byte_en == lbe;
			       waitForCompletion == 1;
                               };
    
    iosf_seq.start(env.MBYIosfPriVc.getSequencer());

    //return data.
    for (int i= added_bytes ; i < length; i++) 
      begin
        data.push_back(iosf_seq.return_data[i/4][(8*(i%4))+:8]);
      end
// END IOSF_NOT_PRESENT
  endtask // read_through_iosf

  /*
    Task: write_through_iosf
    
   MBY IP imp front door write access through IOSF primary interface
   
  
    */
  task write_through_iosf(input sla_pkg::addr_t addr, input byte_t data[$]);
    // IOSF seq to be used
// START IOSF_NOT_PRESENT
    mby_iosf_pri_basic_trans iosf_seq;
    Iosf::data_t      tmp_data []; 
// END IOSF_NOT_PRESENT
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
    
// START IOSF_NOT_PRESENT
    // Pack the DATA to DW
    tmp_data =  new [ data.size()/4 + (data.size()%4 > 0 )];
    for (int i= 0 ; i < aligned_data.size(); i++)
      tmp_data[i/4][(i%4)*8+:8] = aligned_data[i];
// END IOSF_NOT_PRESENT
    
// START IOSF_NOT_PRESENT
    // Invoke IOSF seq to do read transaction
    iosf_seq = new("MBY_SM_SEQ");
    iosf_seq.randomize() with {m_cmd == (addr >= 36'h1_0000_0000 ? Iosf::MWr64 :Iosf::MWr32);
			       m_address == addr;
			       m_data.size() == data.size()/4 + (data.size()%4 > 0 );
			       foreach (m_data[i])
			       m_data[i] == tmp_data[i];
			       m_first_byte_en == fbe;
			       m_last_byte_en ==  lbe;
			       waitForCompletion == 0;
			       };
    
    iosf_seq.start(env.MBYIosfPriVc.getSequencer());
// END IOSF_NOT_PRESENT
    
  endtask

endclass : mby_sm_env
