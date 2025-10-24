// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0


//config mby;
//  design mby_tb_lib.mby_tb;
//endconfig

// this configuration should be regarded as a list of overrides for emulation on top of 'config soc_rtl;'
config mby_emu;
  //moved to DUT.cfg: design mby_rtl_lib.mby_top;
  default liblist mby_rtl_lib sip_shared_lib;
endconfig
