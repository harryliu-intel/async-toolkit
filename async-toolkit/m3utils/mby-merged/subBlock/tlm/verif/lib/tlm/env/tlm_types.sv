/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    tlm_types.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : TLM1 IP
   ----------------------------------------------------------------------
 
   Section: TLM1 typedef file

 include all TLM1 IP typedef

 All typdef are wrapped inside a class to avoid collision with other typedefs.



*/




`ifndef TLM1_TYPES_SV
`define TLM1_TYPES_SV

/*
 Class: TLM1_types

 TLM1 typedef class warper


 */

class TLM1_types;

   //something like that :) 
   typedef enum {Group0,Group1,Group2,Group3} reqType;
   
/*
typedef enum int {
          TLM1_IOSF_PRIMARY,
          TLM1_IOSF_SIDEBAND

      } tlm_bus_types;
*/
endclass
`endif // TLM1_TYPES_SV

