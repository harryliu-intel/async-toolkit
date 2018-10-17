/**
Section: Assertion Control
 File - SOCVAL_ROOT/acb/acb.vh
 Primary Contact - Barry Schneider (barry.c.schneider@intel.com)
 Creation Date - 8/12/2011
 Copyright (C) - 2011 by Intel Corporation. This information is the
  confidential and proprietary property of Intel Corporation and the possession
  or use of this file requires a written license from Intel Corporation.
 Classification - Intel Confidential

Description:
 Macros for the assertion control methodology.  These should be used in IP
 specific instrumentation code to disable SVA's and unique/priority checking
 when reset is asserted or power is lost.

 The <ACB_RESET_MACRO_ON(id,reset)> macro should be used to start the code and
 start the turnAssertionsOn_``id task, the <ACB_RESET_MACRO_OFF(id)> macro
 begins the turnAssertionsOff_``id task, and the <ACB_RESET_MACRO_END> macro
 ends the block.  Multiple blocks can be created by using a different id with
 the series of macros.

Details:
 The <ACB_RESET_MACRO_ON(id,reset)> macro takes the path to the reset/power gate
 signal as an argument.  It calls the turnAssertionsOn_``id() task when the
 reset signal equals 0, and calls the turnAssertionsOff_``id() task when the
 reset signal does not equal 0.

 The <ACB_RESET_MACRO_ON(id,reset)> macro also starts the turnAssertionsOn_``id()
 task, and thus should be followed by any commands to be executed to turn
 assertions on.  This can include the <ACB_TURN_ON_HIER(hier)> macro, to call
 the $asserton(0,hier) and $uniq_prior_checkon(0,hier) system tasks.

 The <ACB_RESET_MACRO_OFF(id)> macro then starts the turnAssertionsOff_``id()
 task, which should be followed by any commands to be executed to turn
 assertions off.  This can include the <ACB_TURN_OFF_HIER(hier)> macro, to call
 the $assertoff(0,hier) and $uniq_prior_checkoff(0,hier) system tasks.

 Finally, the <ACB_RESET_MACRO_END> macro will end       the block, by ending the
 turnAssertionsOff_``id() task.

Example:
 (start code)
  `ACB_RESET_MACRO_ON(ssa, ~`SOC.cck_rstnclb_zcznfwh) //starts turnAssertionsOn()
     `ACB_TURN_ON_HIER(`SSA)
     $asserton(0, ssacte_tb);
     `SSACTE.acbSetAssertionDefaults();
  `ACB_RESET_MACRO_OFF(ssa) //starts turnAssertionsOff()
     `ACB_TURN_OFF_HIER(`SSA)
     $assertoff(0, ssacte_tb);
  `ACB_RESET_MACRO_END
 (end       code)

*/


/// Macro: ACB_RESET_MACRO_ON(id,reset)
/// Initial macro, of three, for the assertion control methodology.
///
/// Arguments:
///  id    - An identifier name, eg ssa
///  reset - The reset signal to use, eg ~`SOC.cck_rstnclb_zcznfwh
///
/// Description:
///  This first calls the turnAssertionsOff_``id() task, to turn off assertions
///  during the initial reset at time 0.  It then defines an always block that
///  will call turnAssertionsOff_``id() when the reset signal does not equal 0,
///  and will call turnAssertionsOn_``id() when the reset signal equals 0,
///  unless the SVA_OFF plusarg is set.
///
///  The acb_reset_``id``_on and acb_reset_``id``_off events are emitted after
///  the IP functions have been called.
///
///  This also starts the turnAssertionsOn_``id() task, which should be
///  populated with any commands to be executed to turn assertions on.  This can
///  include the <ACB_TURN_ON_HIER(hier)> macro, to call the $asserton(0,hier)
///  and $uniq_prior_checkon(0,hier) system tasks.
`define ACB_RESET_MACRO_ON(id,reset) \
   `ACB_RESET_MAIN(id, reset) \
   `ACB_TURN_ON_BEGIN(id)

/// Macro: ACB_RESET_MACRO_OFF(id)
/// Second macro, of three, for the assertion control methodology.  This
/// continues with the same 'id' argument as the <ACB_RESET_MACRO_ON(id,reset)>
/// macro.  This ends the turnAssertionsOn_``id() task, and starts the
/// turnAssertionsOff_``id() task.  Like the on task, the off task should be
/// populated with any commands to be executed to turn assertions off.  This can
/// include the <ACB_TURN_OFF_HIER(hier)> macro, to call the $assertoff(0,hier)
/// and $uniq_prior_checkoff(0,hier) system tasks.
`define ACB_RESET_MACRO_OFF(id) \
   `ACB_TURN_ON_END \
   `ACB_TURN_OFF_BEGIN(id)

/// Macro: ACB_RESET_MACRO_END
/// Final macro for the assertion control methodology.  This currently just ends
/// the turnAssertionsOff_``id() task.
`define ACB_RESET_MACRO_END \
   `ACB_TURN_OFF_END


/// Macro: ACB_TURN_ON_HIER(hier)
/// Turn on assertions for the hierarchy given in the hier argument.  This can
/// either be a relative path, or an absolute path using path macros.  This
/// calls the $asserton(0,hier) and $uniq_prior_checkon(0,hier) system tasks.
`define ACB_TURN_ON_HIER(hier) \
      $asserton(0, hier); \
      $uniq_prior_checkon(0, hier);

/// Macro: ACB_TURN_OFF_HIER(hier)
/// Turn off assertions for the hierarchy given in the hier argument.  This can
/// either be a relative path, or an absolute path using path macros.  This
/// calls the $assertoff(0,hier) and $uniq_prior_checkoff(0,hier) system tasks.
`define ACB_TURN_OFF_HIER(hier) \
      $assertoff(0, hier); \
      $uniq_prior_checkoff(0, hier);


/// Main macro, containing the initial turnAssertionsOff_``id() call, and the
/// always block that watches reset and will call turnAssertionsOff_``id() when
/// the reset signal does not equal 0, and will call turnAssertionsOn_``id()
/// when the reset signal does equal 0, unless the SVA_OFF plusarg is set.
/// The acb_reset_``id``_on and acb_reset_``id``_off events are emitted after
/// the IP functions have been called.
`define ACB_RESET_MAIN(id, _reset) \
   /*"local" copy of the reset signal, in case it's an expression*/ \
   logic acb_reset_``id; \
   event acb_reset_``id``_on; \
   event acb_reset_``id``_off; \
   assign acb_reset_``id = _reset; \
   /*turn off assertions at time 0*/ \
   initial turnAssertionsOff_``id(); \
   /*turn on and off assertions based on reset edges*/ \
   always @(acb_reset_``id) begin \
      if (acb_reset_``id !== 1'b0) begin \
         turnAssertionsOff_``id(); \
         -> acb_reset_``id``_off; \
      end       /*if acb_reset*/ \
      else if ((acb_reset_``id === 1'b0) && (!$test$plusargs("SVA_OFF"))) begin \
         turnAssertionsOn_``id(); \
         -> acb_reset_``id``_on; \
      end       /*if !acb_reset*/ \
   end       //always @reset


/// Define the turnAssertionsOn_``id() task, given the identifier argument
/// named 'id'.
`define ACB_TURN_ON_BEGIN(id) \
   task turnAssertionsOn_``id(); \
      $display("ACB: assert-on fired (id) at %0t", $realtime);

/// End the turnAssertionsOn_``id() task.
`define ACB_TURN_ON_END \
   endtask //turnAssertionsOn


/// Define the turnAssertionsOff_``id() task, given the identifier argument
/// named 'id'.
`define ACB_TURN_OFF_BEGIN(id) \
   task turnAssertionsOff_``id(); \
      $display("ACB: assert-off fired (id) at %0t", $realtime);

/// End the turnAssertionsOff_``id() task.
`define ACB_TURN_OFF_END \
   endtask //turnAssertionsOff
