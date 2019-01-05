//------------------------------------------------------------------------------
//
// INTEL CONFIDENTIAL
//
// Copyright 2018 Intel Corporation All Rights Reserved.
//
// The source code contained or described herein and all documents related to
// the source code ("Material") are owned by Intel Corporation or its suppliers
// or licensors. Title to the Material remains with Intel Corporation or its
// suppliers and licensors. The Material contains trade secrets and proprietary
// and confidential information of Intel or its suppliers and licensors. The
// Material is protected by worldwide copyright and trade secret laws and
// treaty provisions. No part of the Material may be used, copied, reproduced,
// modified, published, uploaded, posted, transmitted, distributed, or
// disclosed in any way without Intel's prior express written permission.
//
// No license under any patent, copyright, trade secret or other intellectual
// property right is granted to or conferred upon you by disclosure or delivery
// of the Materials, either expressly, by implication, inducement, estoppel or
// otherwise. Any license under such intellectual property rights must be
// express and approved by Intel in writing.
//
//------------------------------------------------------------------------------
//   Author        : Akshay Kotian
//   Project       : Madison Bay
//   Description   : Top module to start/stop the connection to white model server.
//------------------------------------------------------------------------------

module mby_wm_top();

    import mby_wm_dpi_pkg::* ;

    initial begin
        //Temporary plus arg to enable connection to WM until the WM development is complete.
        if ($test$plusargs("WHITE_MODEL_EN")) begin
            string model_server = "scala";

            //Plus arg to choose between "scala" or "M3" WM server.
            //Connects to Scala WM server by default
            if ($value$plusargs("WHITE_MODEL_SERVER=%s", model_server)) begin
                $display("mby_wm_dpi_pkg/mby_wm_top.sv:: Using %s WM Server",model_server);
            end
            if(wm_server_start(model_server)) begin
                $error("mby_wm_dpi_pkg/mby_wm_top.sv:: Error while connecting to the WM");
            end
            else
                $display("mby_wm_dpi_pkg/mby_wm_top.sv:: Connected to %s WM Server",model_server);
        end

    end

    final begin
        //Stop the White model server if it was started at the beginning of the test.
        if ($test$plusargs("WHITE_MODEL_EN")) begin
            wm_server_stop();
            $display("mby_wm_dpi_pkg/mby_wm_top.sv:: Disconnected from WM Server");
        end
    end

endmodule
