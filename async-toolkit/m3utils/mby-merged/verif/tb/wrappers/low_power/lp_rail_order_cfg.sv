//=========================================================
//-- Author: Ravindra Ganti
//--         ravindra.k.ganti@intel.com
//--
//=========================================================

class lp_rail_order_cfg;
    //----------------
    //-- Power Rails
    //----------------
    typedef enum {
        VCC_RTC,
        DSW_3P3,  //-- 3.3v
        USB_VBUS,
        VCC_PRIM_3P3,  //-- 3.3v
        VCC_PRIM_1P8,  //-- 1.8v
        VCC_PRIM_0P85,  //-- 0.85v(Vnn)
        VCC_PRIM_1P0,  //-- 1.0v(Vnn)
        VCC_PRIM_1P05,  //-- 1.05v
        VCC_PRIM_1P1  //-- 1.1v
    } PWR_RAILS_TYPE_E;


    rand PWR_RAILS_TYPE_E pwr_rails_e[int];
    string msg;
    string name;

    //-- Set num_pwr_rails
    constraint num_pwr_rails {
        soft pwr_rails_e.size() == 7;
    }

    //-- Default Order Constraint
    //-- VCC_RTC -> DSW_3P3 -> USB_VBUS -> 3.3v | 1.8v -> 0.85v | 1.0v
    constraint default_rail_order {
        pwr_rails_e[0] == VCC_RTC; //-- 1st Rail to be powered up
        pwr_rails_e[1] == DSW_3P3; //-- Redaundant for EBG
        //--------------------------------------------------------------------
        //-- Recommendation: VBUS should be powered after DSW 3.3v is powered.
        //-- If Vbus is powered while DSW 3.3v is off or still ramping, minor
        //-- leakage can exist.
        //--------------------------------------------------------------------
        pwr_rails_e[2] == USB_VBUS; //-- VBUS should be powered after DSW 3.3v is powered. 
        //-- Preferred Ramp Order: 3.3v -> 1.8v and/or VNN -> 1.05v
        //-- 3.3v allowed to ramp first with no limit.
        //-- However, 1.8v can be allowed to ramp before 3.3v.
        pwr_rails_e[3] inside {VCC_PRIM_1P8, VCC_PRIM_3P3};
        pwr_rails_e[5] inside {VCC_PRIM_0P85, VCC_PRIM_1P0};
    }
    
    //-- Rail to Rail Order 
    constraint rail2rail_order {
        //-----------------------------------------------------------------
        //-- PWELL 0.85v should always ramp before or equal to PWELL 1.05v.
        //-----------------------------------------------------------------
        foreach (pwr_rails_e[i])
            ((i==5) && ((pwr_rails_e[i] == VCC_PRIM_0P85) ||
            (pwr_rails_e[i] == VCC_PRIM_1P0))) -> (pwr_rails_e[i+1] == VCC_PRIM_1P05); 

        //-----------------------------------------------------------------
        //-- If 3.3v ramps up first, the next rail to ramp up is 1.8v
        //-----------------------------------------------------------------
        foreach (pwr_rails_e[i])
            ((i==3) && (pwr_rails_e[i] == VCC_PRIM_3P3)) -> (pwr_rails_e[i+1] == VCC_PRIM_1P8); 

        //-----------------------------------------------------------------
        //-- If 1.8v ramps up first, the next rail to ramp up is 3.3v
        //-----------------------------------------------------------------
        foreach (pwr_rails_e[i])
            ((i==3) && (pwr_rails_e[i] == VCC_PRIM_1P8)) -> (pwr_rails_e[i+1] == VCC_PRIM_3P3); 

    }

    //-- Unique rails in the list
    constraint uniq_rails {
        foreach(pwr_rails_e[i])
            foreach(pwr_rails_e[j])
                (i != j) -> (pwr_rails_e[i] != pwr_rails_e[j]);
    }

    function new(string name = "lp_rail_order_cfg", string lp_msg = "LP-INFO");
        this.name = name;
        this.msg = {"[",lp_msg,"]"};
    endfunction: new

    //-- Post Randomize
    function void post_randomize();
        string s_rail_order = "";
        int space_indent = 0;
        $display("\n===================================================================");
        $display("%s Rail to Rail Order sequence generated is as given below:", msg);
        $display("===================================================================\n");
        foreach (pwr_rails_e[i]) begin
           if (s_rail_order == "")
               s_rail_order = {"\n", " -> ", pwr_rails_e[i].name(), "\n"}; 
           else begin
              space_indent++;
              for (int i=0; i< space_indent; i++)
                  s_rail_order = {s_rail_order, "  "};
               s_rail_order = {s_rail_order, " -> ", pwr_rails_e[i].name(), "\n"}; 
           end      
        end      
        s_rail_order = {s_rail_order, "\n"}; 
        $display("%s Rail Order Sequence in Temporal Order: %0s", msg, s_rail_order);
        $display("===================================================================");
    endfunction: post_randomize

endclass: lp_rail_order_cfg


module lp_sequence();
    initial begin
        lp_rail_order_cfg lp_rail_cfg = new();
        for (int i=0; i<10; i++) begin
            lp_rail_cfg.randomize() with {pwr_rails_e.size() == 7;};
        end      
    end      
   
endmodule
