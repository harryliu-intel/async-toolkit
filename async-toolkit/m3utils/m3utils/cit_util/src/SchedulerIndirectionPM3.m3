(* $Id: SchedulerIndirectionPM3.m3,v 1.1 2011/04/12 01:10:59 mika Exp $ *)

MODULE SchedulerIndirectionPM3 EXPORTS SchedulerIndirection;
IMPORT SchedulerPosix AS Scheduler;

PROCEDURE DisableSwitching() = 
  BEGIN Scheduler.DisableSwitching() END DisableSwitching;

PROCEDURE EnableSwitching() = 
  BEGIN Scheduler.EnableSwitching() END EnableSwitching;

BEGIN END SchedulerIndirectionPM3.
