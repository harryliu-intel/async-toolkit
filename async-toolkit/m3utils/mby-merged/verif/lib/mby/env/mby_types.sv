/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:    mby_types.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
 
   Section: MBY typedef file

 include all MBY IP typedef

 All typdef are wrapped inside a class to avoid collision with other typedefs.



*/




`ifndef MBY_TYPES_SV
`define MBY_TYPES_SV

/*
 Class: MBY_types

 MBY typedef class warper


 */

class MBY_types;

   //something like that :) 
   typedef enum {Group0,Group1,Group2,Group3} reqType;
// START IOSF_NOT_PRESENT
//   typedef enum iosfsbm_cm::opcode_t {
//        FUSE_GROUP0_REQ    = 8'hB8,
//        FUSE_GROUP1_REQ   = 8'hB9,
//        FUSE_GROUP2_REQ   = 8'hBA,
//        FUSE_GROUP3_REQ   = 8'hBB
//    } reqOpCode;
// END IOSF_NOT_PRESENT
   
/*
typedef enum int {
          MBY_IOSF_PRIMARY,
          MBY_IOSF_SIDEBAND

      } mby_bus_types;
*/
endclass
`endif // MBY_TYPES_SV

