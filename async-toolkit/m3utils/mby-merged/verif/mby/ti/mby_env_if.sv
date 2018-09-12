/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_env_if.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
 IP interafce 
 
 Interface to connect between the IP DUT and ENV
 
 Can be use for OOO signals.
 
 For ex ( interuppts, power, clocks and resets  ...)

 


*/

interface mby_env_if();
  // Power good
  logic power_good_reset;
  // Primray interface clk & reset
  logic primary_reset;
  logic primary_clock;
  logic enable_primary_clock;
  //  Secondery interface clk & reset
  logic secondary_reset;
  logic secondary_clock;
  logic enable_secondary_clock;
	
  // Dummy interrupt wire for monitoring.
  logic mby_int_wire;

  initial begin
    power_good_reset = 0;
    primary_reset = 0;
    secondary_reset = 0;
  end
  
endinterface