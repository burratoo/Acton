with Oak.Core;
with Oak.Scheduler;

with Ada.Cyclic_Tasks; use Ada.Cyclic_Tasks;

package body Oak.Agent.Tasks.Cycle is

   subtype Event_Based is Behaviour range Aperiodic .. Sporadic;
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
         T.State :=
           (if T.Cycle_Behaviour = Periodic then Sleeping
                else Sleeping_And_Waiting);
         T.Wake_Time      := T.Next_Run_Cycle;
         T.Next_Run_Cycle := T.Next_Run_Cycle + T.Cycle_Period;
      end if;

      if T.Cycle_Behaviour in Aperiodic then
         if T.Event_Raised then
            T.Event_Raised   := False;
         else
            T.State := Waiting_For_Event;

            --  Temp arrangement

            Scheduler.Remove_Task_From_Scheduler (T);
            T.Deadline_Timer.Remove_Timer;
            Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
              (Scheduler_Info => Core.Scheduler_Info  (Core.Oak_Instance).all,
               Chosen_Task    => T);
            return;
         end if;
      end if;

      --  Update Deadline

      case T.Cycle_Behaviour is
         when Periodic =>
            T.Set_Next_Deadline_For_Task (Using => Wake_Up_Time);
         when Sporadic =>
            T.Deadline_Timer.Remove_Timer;
         when Normal | Aperiodic =>
            T.Set_Next_Deadline_For_Task (Using => Clock_Time);
      end case;

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
         Task_Released (Task_To_Release);

         Scheduler.Add_Task_To_Scheduler
           (Scheduler_Info => Core.Scheduler_Info  (Core.Oak_Instance).all,
            T              => Task_To_Release);
         Scheduler.Check_With_Scheduler_Agents_On_Which_Task_To_Run_Next
           (Scheduler_Info => Core.Scheduler_Info  (Core.Oak_Instance).all,
            Chosen_Task    => Next_Task);

      else
         Task_To_Release.Event_Raised := True;
         Next_Task := Releasing_Task;
      end if;

   end Release_Task;

   procedure Task_Released
     (Released_Task : access Task_Agent'Class) is
   begin
      Released_Task.State := Running;
      Released_Task.Wake_Time := Clock;
      Released_Task.Next_Run_Cycle := Released_Task.Wake_Time +
        Released_Task.Cycle_Period;
      Released_Task.Set_Next_Deadline_For_Task (Using => Wake_Up_Time);
   end Task_Released;

end Oak.Agent.Tasks.Cycle;
