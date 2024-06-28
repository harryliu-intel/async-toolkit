
module oscdecode
  #(parameter NSETTINGS=4,
    parameter NINTERP  =8,
    parameter NSTAGES  =6,
    parameter NTAPS    = NSTAGES - 1,
    parameter NSETTINGS_PER_TAP = NINTERP * NSETTINGS,
    parameter NTOTAL_SPEEDS = NTAPS * NSETTINGS_PER_TAP + 1,
    parameter W        = $clog2(NTOTAL_SPEEDS)
    )
   (input  logic [ W - 1 : 0 ]                            i_speed,

    output logic [ NSTAGES - 1 : 0]                       o_stage_en,
    output logic [ NINTERP - 1 : 0][ NSETTINGS - 1 : 0 ] o_interp_ctrl
    );

   logic                          [ NSTAGES - 1 : 0 ] coarsecode [ NTOTAL_SPEEDS - 1 : 0];
   logic [ NINTERP - 1 : 0][ NSETTINGS - 1 : 0 ]   finecode[ NTOTAL_SPEEDS - 1 : 0]   ;
   
   
   generate
      $info("XXXX NSETTINGS=%d NINTERP=%d NTAPS=%d NSETTINGS_PER_TAP=%d",
            NSETTINGS, NINTERP, NTAPS, NSETTINGS_PER_TAP);
      $info("XXXX NTOTAL_SPEEDS=%d W=%d",
            NTOTAL_SPEEDS, W);
      
      for (genvar s=0; s < NTOTAL_SPEEDS; ++s) begin : gen_speed

         localparam tap_lo       = s / NSETTINGS_PER_TAP;
         localparam speed_in_tap = s % NSETTINGS_PER_TAP;
         localparam tap_hi       = tap_lo + 1;
         localparam code_in_tap  = (tap_lo % 2 == 0) ? speed_in_tap : (NSETTINGS_PER_TAP - speed_in_tap);
         
         $info("XXXXXX s=%d, tap_lo=%d, tap_hi=%d",
               s, tap_lo, tap_hi);
         
         $info("XXXXXX speed_in_tap=%d, code_in_tap=%d", 
               speed_in_tap, code_in_tap);
         

         for (genvar i=0; i < NINTERP; ++i) begin : gen_interp
            localparam c_o_s = code_in_tap / NSETTINGS;
            
            localparam ic = (c_o_s > i) ? NSETTINGS :
                            ((c_o_s == i) ? code_in_tap % NSETTINGS :
                             0);
            $info("XXXXXXXX i=%d, c_o_s=%d, ic=%d",
                  i, c_o_s, ic);

            for (genvar j=0; j < NSETTINGS; ++j) begin : gen_fine
               localparam fc = (ic > j) ? 1 : 0;

               $info("XXXXXXXXXX finecode[%d][%d] = %d", i, j, fc);

               assign finecode[s][i][j] = fc;
               
            end

         end

         for (genvar t=0; t < NSTAGES; ++t) begin : gen_coarse
            localparam cc =
                  (t == tap_lo || 
                   t == tap_hi || 
                   (tap_lo == NTAPS && t == NTAPS - 1)) ? 1 : 0;
            $info("XXXXXXXXXX coarsecode[%d] = %d", t, cc);
            assign coarsecode[s][t] = cc;
         end
      end
   endgenerate

   always_comb 
     begin
        o_stage_en = '0;
        o_interp_ctrl = '0;
        
        o_stage_en = coarsecode[i_speed];
        o_interp_ctrl = finecode[i_speed];
     end

endmodule
    
   
