/*****************************************************
 * Copyright 2005 Fulcrum Microsystems               *
 * Copyright 1999 California Institute of Technology *
 * All rights reserved.  Written by Andrew Lines.    *
 *****************************************************/

/*** global options data structure ***/
typedef struct _options
  {
  /*** analog parameters ***/
  double timestep,
         timeratio, /* fraction of minium RC delay to set timestep to */
         poststep,
         timemax,
         maxerr,
         partition_mode,
         coupling_cutoff,
         checkpoint_interval, /* system time to do checkpoints (or -1) */
         vmin, /* minimum clamping voltage */
         vmax; /* maximum clamping voltage */
  /*** for mixedmode simulations ***/
  double trueV,
         falseV,
         lo_thresholdV,
         hi_thresholdV,
         prs_tau,
         digital_time_unit,
         d2a_shape, /* choose shape of D2A waveforms (0=exponential, 1=hybrid) */
         d2a_saturation, /* saturation threshold for hybrid d2a_shape */
         a2d_ewma_tau, /* used to smooth V before a2d translation */
         settle_time, /* used to transition from digital to mixed simulation */
         scale_after_delay, /* scales digital after delays */
         scale_after_ps_delay; /* scales digital after_ps delays differently */
  /*** for alint ***/
  double VTN,
         VTP,
         leakage_enabled,
         small_leak_voltage,
         large_leak_voltage,
         max_bump_fanin_aggressors,
         max_delay_fanin_aggressors,
         stable_dVdt,
         bump_event_interval,
         delay_event_interval,
         scenario_timemax,
         relevant_cap_ratio,
         alint_voltage_margin,
         thresh_scenario_timemax, // max duration of alint thresh_up/dn scenarios
         delay_slow_inverse; // assume output INVERSE is infinite C for slow_up/dn
  } OPTIONS;

OPTIONS default_options();

extern OPTIONS options;
