with Oak.Core;
with Oak.Scheduler;

with Ada.Cyclic_Tasks; use Ada.Cyclic_Tasks;

package body Oak.Agent.Tasks.Cycle is

   subtype Event_Based is Behaviour range Aperiodic .. Sporadic;
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
      T.Event_Raised   := False;
   end Setup_Cyclic_Section;

   ---------------
   -- New_Cycle --
   ---------------

   procedure New_Cycle (T : in out Task_Handler) is
   begin

      --  This exit state only applies for tasks' whose timing behaviour is
      --  normal. It's not covered by a conditional statement since the
      --  following statements will overwrite the value if needed and the
      --  simple assignment has a smaller overhead than a corresponding if
      --  statement.

      T.State := Runnable;

      --  Time based behaviours (sporadic and periodic) are checked first since
      --  sporadic tasks are first held in a waiting state for a event trigger
      --  to occur before transitioning to a sleep state to wait for its next
      --  time release point. Thus event based behaviours are checked last
      --  so its modification to the task's state is the one that is applied.

      if T.Cycle_Behaviour in Time_Based then
         T.State          := Sleeping;
         T.Wake_Time      := T.Next_Run_Cycle;
         T.Next_Run_Cycle := T.Next_Run_Cycle + T.Cycle_Period;
      end if;

      if T.Cycle_Behaviour in Event_Based then
         if T.Event_Raised then
            T.State       := Runnable;
            T.Event_Raised := False;
         else
            T.State := Waiting_For_Event;
         end if;
      end if;

      Scheduler.Inform_Scheduler_Agent_Task_Has_Changed_State (T);

   end New_Cycle;

   ------------------
   -- Release_Task --
   ------------------

   procedure Release_Task
     (Task_To_Release, Releasing_Task : in Task_Handler;
      Next_Task                       : out Task_Handler)
   is
   begin
      Releasing_Task.State := Runnable;

      if Task_To_Release.State = Waiting_For_Event then
         Task_To_Release.State := Sleeping;

         Next_Task := Task_To_Release;
         Scheduler.Inform_Scheduler_Agent_Task_Has_Changed_State
           (Next_Task);
      else
         Task_To_Release.Event_Raised := True;
         Next_Task := Releasing_Task;
      end if;

   end Release_Task;

end Oak.Agent.Tasks.Cycle;
