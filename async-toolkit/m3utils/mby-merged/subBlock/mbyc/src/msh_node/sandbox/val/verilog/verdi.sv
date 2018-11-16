///
///  INTEL CONFIDENTIAL
///
///  Copyright 2017 Intel Corporation All Rights Reserved.
///
///  The source code contained or described herein and all documents related
///  to the source code ("Material") are owned by Intel Corporation or its
///  suppliers or licensors. Title to the Material remains with Intel
///  Corporation or its suppliers and licensors. The Material contains trade
///  secrets and proprietary and confidential information of Intel or its
///  suppliers and licensors. The Material is protected by worldwide copyright
///  and trade secret laws and treaty provisions. No part of the Material may
///  be used, copied, reproduced, modified, published, uploaded, posted,
///  transmitted, distributed, or disclosed in any way without Intel's prior
///  express written permission.
///
///  No license under any patent, copyright, trade secret or other intellectual
///  property right is granted to or conferred upon you by disclosure or
///  delivery of the Materials, either expressly, by implication, inducement,
///  estoppel or otherwise. Any license under such intellectual property rights
///  must be express and approved by Intel in writing.
///
// ---------------------------------------------------------------------------------------------------------------------
// -- Author : Jim McCormick <jim.mccormick@intel.com>
// -- Project Name : ??? 
// -- Description  : This module generates an FSDB file. 
// ---------------------------------------------------------------------------------------------------------------------

module verdi;

// "initial" procedures are executed at the start of simulation
initial begin

    if($test$plusargs("tracestart")) begin

        // +tracestart= found on build command line
        integer tracestart = 0;                             // default time to start tracing
        $value$plusargs("tracestart=%d", tracestart);       // use value following +tracestart= as trace start time 
        $display("%10.3f %m INFO  Scheduling dumping at time=%0d", $realtime, tracestart);

        #tracestart;                                        // delay tracestart cycles

        $fsdbDumpvars(0,"+all");                            // start dumping everything

    end else begin

        $display("%10.3f %m INFO  To enable dumping, use TRACESTART=%%d", $realtime);

    end

end // initial

endmodule
