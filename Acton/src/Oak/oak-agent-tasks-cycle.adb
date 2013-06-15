with Oak.Core;
with Oak.Scheduler;

with Ada.Cyclic_Tasks; use Ada.Cyclic_Tasks;

package body Oak.Agent.Tasks.Cycle is

   subtype Event_Based is Behaviour range Aperiodic .. Sporadic;
   subtype Time_Based is Behaviour range Sporadic .. Periodic;

   function Next_Release_Time (Sporadic_Task : in Task_Handler)
     return Time;

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

      T.Execution_Cycles := T.Execution_Cycles + 1;
      if T.Current_Execution_Time > T.Max_Execution_Time then
         T.Max_Execution_Time := T.Current_Execution_Time;
      end if;
      T.Current_Execution_Time := Time_Span_Zero;
      T.Remaining_Budget       := T.Execution_Budget;

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
            T.Event_Raised   := False;
            T.Next_Run_Cycle := Clock + T.Cycle_Period;
         else
            T.State := Waiting_For_Event;

            --  Temp arrangement

            Scheduler.Remove_Task_From_Scheduler (T);
            Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
              (Scheduler_Info => Core.Scheduler_Info  (Core.Oak_Instance).all,
               Chosen_Task    => T);
            return;
         end if;
      end if;

      --  Update Deadline
      if T.Cycle_Behaviour in Time_Based then
         T.Next_Deadline := T.Wake_Time + T.Relative_Deadline;
      else
         T.Next_Deadline := Clock + T.Relative_Deadline;
      end if;

      Scheduler.Inform_Scheduler_Agent_Task_Has_Changed_State (T);

   end New_Cycle;

   --------------------------------
   -- Next_Sporadic_Release_Time --
   --------------------------------

   function Next_Release_Time (Sporadic_Task : in Task_Handler)
     return Time is
      Current_Time : constant Time := Clock;
   begin
      if Current_Time > Sporadic_Task.Wake_Time then
         return Current_Time + Sporadic_Task.Cycle_Period;
      else
         return Sporadic_Task.Wake_Time + Sporadic_Task.Cycle_Period;
      end if;
   end Next_Release_Time;

   ------------------
   -- Release_Task --
   ------------------

   procedure Release_Task
     (Task_To_Release, Releasing_Task : in Task_Handler;
      Next_Task                       : out Task_Handler)
   is
      Release_Time : Time;
   begin
      Releasing_Task.State := Runnable;

      if Task_To_Release.State = Waiting_For_Event then
         Task_To_Release.State := Sleeping;

         Release_Time                   := Next_Release_Time (Task_To_Release);
         Task_To_Release.Next_Run_Cycle := Release_Time;
         Task_To_Release.Next_Deadline  :=
           Release_Time + Task_To_Release.Relative_Deadline;

         Next_Task := Task_To_Release;
         Scheduler.Add_Task_To_Scheduler
           (Scheduler_Info => Core.Scheduler_Info  (Core.Oak_Instance).all,
            T => Task_To_Release);
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Core.Scheduler_Info  (Core.Oak_Instance).all,
            Chosen_Task    => Next_Task);
      else
         Task_To_Release.Event_Raised := True;
         Next_Task := Releasing_Task;
      end if;

   end Release_Task;

end Oak.Agent.Tasks.Cycle;
