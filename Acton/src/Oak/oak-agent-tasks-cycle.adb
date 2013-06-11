with Oak.Core;
with Oak.Scheduler;

with Ada.Cyclic_Tasks; use Ada.Cyclic_Tasks;

package body Oak.Agent.Tasks.Cycle is

   subtype Event_Based is Behaviour range Normal .. Aperiodic;
   pragma Unreferenced (Event_Based);
   subtype Time_Based is Behaviour range Sporadic .. Periodic;

   --------------------------
   -- Setup_Cyclic_Section --
   --------------------------

   --  Task state on entry: Setup_Cycle.
   --  Task selected on exit: T.

   procedure Setup_Cyclic_Section (T : in Task_Handler) is
   begin
      T.State          := Runnable;
      T.Next_Run_Cycle := Core.Global_Start_Time + T.Phase;
   end Setup_Cyclic_Section;

   ---------------
   -- New_Cycle --
   ---------------

   procedure New_Cycle (T : in out Task_Handler) is
   begin
      T.State := Sleeping;

      if T.Cycle_Behaviour in Time_Based then
         T.Wake_Time      := T.Next_Run_Cycle;
         T.Next_Run_Cycle := T.Next_Run_Cycle + T.Cycle_Period;
      end if;

      Scheduler.Inform_Scheduler_Agent_Task_Has_Yielded (T);

   end New_Cycle;

end Oak.Agent.Tasks.Cycle;
