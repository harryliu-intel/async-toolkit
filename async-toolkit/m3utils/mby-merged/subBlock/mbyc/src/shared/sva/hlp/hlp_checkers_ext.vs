//------------------------------------------------------------------------------
//
//  INTEL CONFIDENTIAL
//
//  Copyright 2016 - 2017 Intel Corporation All Rights Reserved.
//
//  The source code contained or described herein and all documents related
//  to the source code ("Material") are owned by Intel Corporation or its
//  suppliers or licensors. Title to the Material remains with Intel
//  Corporation or its suppliers and licensors. The Material contains trade
//  secrets and proprietary and confidential information of Intel or its
//  suppliers and licensors. The Material is protected by worldwide copyright
//  and trade secret laws and treaty provisions. No part of the Material may
//  be used, copied, reproduced, modified, published, uploaded, posted,
//  transmitted, distributed, or disclosed in any way without Intel's prior
//  express written permission.
//
//  No license under any patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or
//  delivery of the Materials, either expressly, by implication, inducement,
//  estoppel or otherwise. Any license under such intellectual property rights
//  must be express and approved by Intel in writing.
//
//------------------------------------------------------------------------------

// only include contents of this file once
`ifndef HLP_CHECKERS_EXT_VS
`define HLP_CHECKERS_EXT_VS

  // If none of the SVA assertion-disabling macros are defined, then apply settings.
  `ifndef INTEL_LINTRA_BUG
  `ifndef HLP_SYNTHESIS
  `ifndef INTEL_SVA_OFF

    `include "intel_checkers.vs"
    `include "hlp_checkers_ext_macro.vs"

    `define HLP_INCLUDE_SVA_MON(filename) \
      `include filename

  // Otherwise, define an empty HLP_INCLUDE_SVA_* macro for each non-SVA case.
  `else // INTEL_SVA_OFF
    `define HLP_INCLUDE_SVA_MON(filename)
  `endif // INTEL_SVA_OFF
  `else // HLP_SYNTHESIS
    `define HLP_INCLUDE_SVA_MON(filename)
  `endif // HLP_SYNTHESIS
  `else // INTEL_LINTRA_BUG
    `define HLP_INCLUDE_SVA_MON(filename)
  `endif // INTEL_LINTRA_BUG


  // If none of the coverage-disabling macros are defined, then apply settings.
  `ifndef INTEL_LINTRA_BUG
  `ifndef HLP_SYNTHESIS
  `ifndef INTEL_SVA_OFF
  `ifndef HLP_COV_OFF

    `define HLP_INCLUDE_SVA_COV(filename) \
      `include filename

    `define HLP_PLUSARGS_COV_DISABLE \
      $test$plusargs("HLP_RTL_COV_DISABLE")

    `define HLP_PLUSARGS_COV_GRP_NEW(grp, suffix=, args=() ) \
       ``grp`` ``grp``suffix``_inst = new``args`` ; \
       initial begin \
         if ( $test$plusargs("HLP_RTL_COV_DISABLE") ) begin \
           ``grp``suffix``_inst.stop() ; \
         end \
       end

  // Otherwise, define an empty HLP_INCLUDE_SVA_* macro for each non-SVA case.
  `else // HLP_COV_OFF
    `define HLP_INCLUDE_SVA_COV(filename)
    `define HLP_PLUSARGS_COV_DISABLE 1
    `define HLP_PLUSARGS_COV_GRP_NEW(grp)
  `endif // HLP_COV_OFF
  `else // INTEL_SVA_OFF
    `define HLP_INCLUDE_SVA_COV(filename)
    `define HLP_PLUSARGS_COV_DISABLE 1
    `define HLP_PLUSARGS_COV_GRP_NEW(grp)
  `endif // INTEL_SVA_OFF
  `else // HLP_SYNTHESIS
    `define HLP_INCLUDE_SVA_COV(filename)
    `define HLP_PLUSARGS_COV_DISABLE 1
    `define HLP_PLUSARGS_COV_GRP_NEW(grp)
  `endif // HLP_SYNTHESIS
  `else // INTEL_LINTRA_BUG
    `define HLP_INCLUDE_SVA_COV(filename)
    `define HLP_PLUSARGS_COV_DISABLE 1
    `define HLP_PLUSARGS_COV_GRP_NEW(grp)
  `endif // INTEL_LINTRA_BUG


  `ifdef HLP_XPROP_CHECK
     `define HLP_INCLUDE_SVA_XPROP(filename) `include filename
  `else
     `define HLP_INCLUDE_SVA_XPROP(filename)
  `endif // HLP_XPROP_CHECK


  `ifdef HLP_JG_SPS_CHECK
     `define HLP_INCLUDE_SVA_SPS(filename) `include filename
  `else
     `define HLP_INCLUDE_SVA_SPS(filename)
  `endif // HLP_JG_SPS_CHECK


  `ifdef HLP_SPS_COV_ON
     `define HLP_INCLUDE_SPS_COV(filename) `include filename
  `else
     `define HLP_INCLUDE_SPS_COV(filename)
  `endif // HLP_SPS_COV_ON

  `ifndef HLP_SYNTHESIS
  `ifdef INTEL_INST_ON
     `include "quickcov_common.vh"
     `include "quickcov_cover.vh"
     `define HLP_PLUSARGS_QC_DISABLE \
       $test$plusargs("HLP_RTL_QC_DISABLE")

     `define HLP_INCLUDE_QUICKCOV(qc_module) `QUICKCOV_INST(qc_module)
     `define HLP_INCLUDE_QUICKCOV_EXT(qc_module,_params,_inst,_args) `QUICKCOV_INST_EXT(qc_module,_params,_inst,_args)
  `else // INTEL_INST_ON
     `define HLP_PLUSARGS_QC_DISABLE 0
     `define HLP_INCLUDE_QUICKCOV(qc_module)
     `define HLP_INCLUDE_QUICKCOV_EXT(qc_module,_params,_inst,_args)
  `endif // INTEL_INST_ON
  `else // HLP_SYNTHESIS
     `define HLP_PLUSARGS_QC_DISABLE 0
     `define HLP_INCLUDE_QUICKCOV(qc_module)
     `define HLP_INCLUDE_QUICKCOV_EXT(qc_module,_params,_inst,_args)
  `endif // HLP_SYNTHESIS

  // If none of the debug-disabling macros are defined, then apply settings.
  `ifndef INTEL_LINTRA_BUG
  `ifndef HLP_SYNTHESIS

    `define HLP_INCLUDE_DEBUG(filename) `include filename

  // Otherwise, define an empty HLP_INCLUDE_DEBUG* macro for each non-debug case.
  `else // HLP_SYNTHESIS
    `define HLP_INCLUDE_DEBUG(filename)
  `endif // HLP_SYNTHESIS
  `else // INTEL_LINTRA_BUG
    `define HLP_INCLUDE_DEBUG(filename)
  `endif // INTEL_LINTRA_BUG

`endif // HLP_CHECKERS_EXT_VS
