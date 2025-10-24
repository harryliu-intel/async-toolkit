(* Copyright (c) 2025 Intel Corporation.  All rights reserved.  See the file COPYRIGHT for more information. *)
(* SPDX-License-Identifier: Apache-2.0 *)

INTERFACE TimingCheckers;
IMPORT TransitionSeq;
IMPORT CheckDir;

(* 
Pulse latch timing:

             ------                  ------
            |      |                |      |
phi         |      |                |      |
            |      |                |      |
   ---------        ----------------        ------
                   .                .      .
                   .                .      .
                   .                .      .
                   .                .      .
                   .     -------------------------------
                   .    |           .      . 
dat                .    |           .      .
                   .    |           .      .
   ----------------.----            .      .
                   <----><----------><----->
                     t_h      t_s      t_p

and we have t_cyc = t_h + t_s + t_p

Now if the edge is within the clock pulse:


             ------                  ------
            |      |                |      |
phi         |      |                |      |
            |      |                |      |
   ---------        ----------------        ------
                   .                .      .
                   .                .      .
                   .                .      .
                   .                .      .
                   .                .   ----------------
                   .                .  |   . 
dat                .                .  |   .
                   .                .  |   .
   ----------------.----------------.--    .
                                    <-><--->
                                   -t_s -t_h
                                    <------>
                                      t_p

Now we have t_s and t_h negative, and t_p + t_s + t_h = 0.

One measurement left, the "glitch width":

             ------                  ------
            |      |                |      |
phi         |      |                |      |
            |      |                |      |
   ---------        ----------------        ------
                   .                .      .
                   .                .      .
                   .                .      .
                   .                .      .
                   .     XXXXXX-------------------------
                   .    |XXXXXX|    .      . 
dat                .    |XXXXXX|    .      .
                   .    |XXXXXX|    .      .
   ----------------.---- XXXXXX     .      .
                   <----><-----><---><----->
                     t_h   t_g   t_s   t_p

and we have t_cyc = t_h + t_s + t_p + t_g

*)

TYPE Checker = PROCEDURE(clkIdx   : CARDINAL;
                         data     : TransitionSeq.T;
                         clk      : TransitionSeq.T;
                         dataDir  : CheckDir.T) : LONGREAL;

     T       = Checker;

CONST
  Fail = LAST(LONGREAL); (* returned if measurement fails *)

PROCEDURE MeasureSetup(clkIdx   : CARDINAL;
                       data     : TransitionSeq.T;
                       clk      : TransitionSeq.T;
                       dataDir  : CheckDir.T) : LONGREAL;

PROCEDURE MeasurePulsemargin(clkIdx   : CARDINAL;
                             data     : TransitionSeq.T;
                             clk      : TransitionSeq.T;
                             dataDir  : CheckDir.T) : LONGREAL;

PROCEDURE MeasureHold(clkIdx   : CARDINAL;
                      data     : TransitionSeq.T;
                      clk      : TransitionSeq.T;
                      dataDir  : CheckDir.T) : LONGREAL;
  
PROCEDURE MeasurePulsewidth(clkIdx   : CARDINAL;
                            data     : TransitionSeq.T;
                            clk      : TransitionSeq.T;
                            dataDir  : CheckDir.T) : LONGREAL;
  
PROCEDURE MeasureGlitchwidth(clkIdx   : CARDINAL;
                             data     : TransitionSeq.T;
                             clk      : TransitionSeq.T;
                             dataDir  : CheckDir.T) : LONGREAL;

END TimingCheckers.
