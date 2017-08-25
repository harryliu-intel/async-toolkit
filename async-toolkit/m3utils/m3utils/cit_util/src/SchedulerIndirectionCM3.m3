(* $Id: SchedulerIndirectionCM3.m3,v 1.1 2011/04/12 01:10:59 mika Exp $ *)

MODULE SchedulerIndirectionCM3 EXPORTS SchedulerIndirection;
IMPORT Scheduler;

PROCEDURE DisableSwitching() = 
  BEGIN Scheduler.DisableSwitching() END DisableSwitching;

PROCEDURE EnableSwitching() = 
  BEGIN Scheduler.EnableSwitching() END EnableSwitching;

BEGIN END SchedulerIndirectionCM3.
