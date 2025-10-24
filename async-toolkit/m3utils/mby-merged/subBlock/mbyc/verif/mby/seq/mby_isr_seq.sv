// Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information.
// SPDX-License-Identifier: Apache-2.0

/* ----------------------------------------------------------------------


   ----------------------------------------------------------------------
   file:     mby_isr_seq.sv
   Date Created  : 25/7/2016
   Author        : dbenita
   Project       : MBY IP
   ----------------------------------------------------------------------
   Section: MBY Config seq

  This file contain all of the MBY configuration sequences.
*/

/*
 Class: mby_isr_seq
 
 This sequence set up the MBY config space:
 */
class mby_isr_seq extends mby_env_base_seq;
  `uvm_object_utils(mby_isr_seq) 
  `uvm_declare_p_sequencer(slu_sequencer)

    task body();
      uvm_report_warning (get_name(), "INTEG - mby_isr_seq should be imp ");

    endtask // body
endclass // mby_isr_seq
