with Oak.Agent.Queue;
with Oak.Message;   use Oak.Message;
with Oak.Scheduler; use Oak.Scheduler;
with Oak.States;    use Oak.States;
with Oak.Timers;    use Oak.Timers;
with Oak.Core;
with Oak.Core_Support_Package.Task_Support;
use Oak.Core_Support_Package.Task_Support;

package body Acton.Scheduler_Agents.Priority_Server is
   procedure Initialise_Scheduler_Agent
     (Agent : in out Priority_Server) is
   begin
      raise Program_Error;
   end Initialise_Scheduler_Agent;

   procedure Initialise_Scheduler_Agent
     (Agent             : in out Priority_Server;
      Budget            : in Time_Span;
      Priority          : in Any_Priority;
      Period            : in Time_Span;
      Phase             : in Time_Span;
      Relative_Deadline : in Time_Span;
      CPU               : in CPU_Range) is
      pragma Unreferenced (CPU);
   begin
      Agent.Set_Priority_Range (From => Priority, To => Priority);

      Initialise_Scheduler_Agent
        (Agent           => Agent'Unchecked_Access,
         Name            => Agent_Name,
         Call_Stack_Size => Stack_Size,
         Run_Loop        => Run_Loop'Address);

      Agent.Period            := Period;
      Agent.Phase             := Phase;
      Agent.Relative_Deadline := Relative_Deadline;
      Agent.Execution_Budget  := Budget;

      Agent.Set_Wake_Time (Oak.Core.Global_Start_Time + Phase);
      Agent.Next_Wake_Time := Agent.Wake_Time + Period;
      Agent.Budget_Exhausted_Time := Agent.Wake_Time + Budget;
      Agent.Scheduler_Timer.Update_Timer (Time_Last);

      Agent.Set_Scheduler_Agent
        (Find_Scheduler_For_System_Priority (Priority => Priority, CPU => 0));
   end Initialise_Scheduler_Agent;

   procedure Run_Loop (Self : in out Priority_Server) is
      Run_Reason     : Agent_State;
      Runnable_Queue : access Oak_Agent'Class renames Self.Runnable_Queue;
      Sleeping_Queue : access Oak_Agent'Class renames Self.Sleeping_Queue;
      Wake_Time      : Time;

      Scheduler_Error1 : exception;

      procedure Task_Yielded;
      procedure Add_Task;
      procedure Remove_Task;

      procedure Insert_Into_Sleeping_Queue
        (T     : access Oak_Agent'Class);

      procedure Add_Task_To_End_Of_Runnable_Queue
        (Task_To_Add : access Oak_Agent'Class);
      procedure Move_Woken_Tasks;

      ------------------
      -- Task_Yielded --
      ------------------

      procedure Task_Yielded is
         Yielded_Task : constant access Oak_Agent'Class :=
                          Self.Agent_Message.Agent_That_Changed;
      begin
         case Yielded_Task.State is
            when Sleep =>
               Queue.Remove_Agent
                 (Queue => Runnable_Queue,
                  Agent => Yielded_Task);
               Insert_Into_Sleeping_Queue
                 (T     => Yielded_Task);

            when Activation_Pending    |
                 Activation_Complete   |
                 Activation_Successful =>
               null;
            when Change_Cycle_Period | Change_Relative_Deadline =>
               null;
            when Runnable =>
               if Runnable_Queue = Yielded_Task then
                  Queue.Move_Head_To_Tail (Runnable_Queue);
               end if;

            when others =>
               raise Scheduler_Error1;
         end case;
      end Task_Yielded;

      --------------
      -- Add_Task --
      --------------

      procedure Add_Task is
         Task_To_Add  : constant access Oak_Agent'Class :=
                          Self.Agent_Message.Agent_To_Add;
         Current_Time : constant Time := Clock;
      begin
         if Task_To_Add.Wake_Time < Current_Time then
            Add_Task_To_End_Of_Runnable_Queue (Task_To_Add => Task_To_Add);
         else
            Insert_Into_Sleeping_Queue (T => Task_To_Add);
         end if;
      end Add_Task;

      -----------------
      -- Remove_Task --
      -----------------

      procedure Remove_Task is
         Task_To_Remove  : constant access Oak_Agent'Class :=
                             Self.Agent_Message.Agent_To_Remove;
      begin
         case Task_To_Remove.State is
         --  Probably best to move this case to the others.

            when Runnable | Entering_PO | Entering_Atomic_Action |
                 Waiting_For_Event =>
               Queue.Remove_Agent
                 (Queue => Runnable_Queue,
                  Agent => Task_To_Remove);

            when Sleeping =>
               Queue.Remove_Agent
                 (Queue => Sleeping_Queue,
                  Agent => Task_To_Remove);
            when others =>
               raise Scheduler_Error1;
         end case;
      end Remove_Task;

      procedure Insert_Into_Sleeping_Queue
        (T     : access Oak_Agent'Class)
      is
         Current        : access Oak_Agent'Class := Sleeping_Queue;
         Task_Wake_Time : constant Time          := T.Wake_Time;
         Queue_End      : Queue.Queue_End_Point  := Queue.Head;
      begin
         if Sleeping_Queue = null then
            Queue.Add_Agent_To_Head
              (Queue => Sleeping_Queue,
               Agent => T);
         else
            while Task_Wake_Time > Current.Wake_Time loop
               Current := Queue.Next_Agent (Current);
               if Current = Sleeping_Queue then
                  Queue_End := Queue.Tail;
                  exit;
               end if;
            end loop;
            Queue.Add_Agent_Before
              (Queue  => Sleeping_Queue,
                Agent => T,
                Before => Current,
                Queue_End => Queue_End);
         end if;
      end Insert_Into_Sleeping_Queue;

      procedure Add_Task_To_End_Of_Runnable_Queue
        (Task_To_Add : access Oak_Agent'Class) is
      begin
         Task_To_Add.Set_State (Runnable);
         Queue.Add_Agent_To_Tail
           (Queue => Runnable_Queue,
            Agent => Task_To_Add);
      end Add_Task_To_End_Of_Runnable_Queue;

      procedure Move_Woken_Tasks is
         Current_Time : constant Time    := Clock;
         T            : access Oak_Agent'Class;
      begin
         while Sleeping_Queue /= null
           and then Current_Time > Sleeping_Queue.Wake_Time
         loop
            T := Sleeping_Queue;
            Queue.Remove_Agent
              (Queue => Sleeping_Queue,
               Agent => T);
            case T.Destination_On_Wake_Up is
               when Run_Queue =>
                  Add_Task_To_End_Of_Runnable_Queue (Task_To_Add => T);
               when Remove =>
                  null;
            end case;
         end loop;
      end Move_Woken_Tasks;

   begin
      loop
         --  Update Budget_Exhausted_Time from scheduler timer. The maximum
         --  value the scheduler timer will possess (excluding Time_Last) is
         --  the time the execution server becomes inactive. Oak is able to
         --  delay the timer due to interference by other tasks if it is placed
         --  in the delayable timers list. TODO: This code belongs in a
         --  defferable server.

--           if Self.Scheduler_Timer.Firing_Time /= Time_Last and then
--             Self.Scheduler_Timer.Firing_Time > Self.Budget_Exhausted_Time
--           then
            --  Budget_Exhausted_Time can be no latter than the next task cycle
--              if Self.Scheduler_Timer.Firing_Time > Self.Next_Wake_Time then
--                 Self.Budget_Exhausted_Time := Self.Wake_Time;
--              else
--              Self.Budget_Exhausted_Time := Self.Scheduler_Timer.Firing_Time;
--              end if;
--           end if;

         Run_Reason := Self.Agent_Message.Message_Type;

         case Run_Reason is
            when Agent_State_Change =>
               Task_Yielded;
            when Adding_Agent =>
               Add_Task;
            when Removing_Agent =>
               Remove_Task;
            when others =>
               null;
         end case;

         Move_Woken_Tasks;

         --  If the agent was sleeping, it stays asleep. The agent can run
         --  while it is technically sleeping so as to add and remove tasks
         --  from it.

         if Self.State = Sleeping then
            Self.Set_Agent_Message
              (Message => (Message_Type => Continue_Sleep));

         --  The clock has passed the agent's budget exhausted time, sleep the
         --  agent and its tasks until the next wake up time.

         elsif Self.Budget_Exhausted_Time < Clock then
            Self.Set_Agent_Message
              (Message => (Message_Type => Sleeping,
                           Wake_Up_At   => Self.Next_Wake_Time));
            Self.Budget_Exhausted_Time := Self.Next_Wake_Time +
              Self.Execution_Budget;
            Self.Next_Wake_Time        := Self.Next_Wake_Time + Self.Period;

         --  The agent is active, its timer is set to the lesser of
         --  the budget exhuasted time and the eariliest wake time of its
         --  tasks in the agent's sleeping queue

         else
            if Sleeping_Queue /= null
              and then Sleeping_Queue.Wake_Time < Self.Budget_Exhausted_Time
            then
               Wake_Time := Sleeping_Queue.Wake_Time;
            else
               Wake_Time := Self.Budget_Exhausted_Time;
            end if;

            --  If there is nothing to run the agent sleeps

            if Runnable_Queue = null then
               Self.Set_Agent_Message
                 (Message => (Message_Type => Sleeping,
                              Wake_Up_At   => Wake_Time));

            --  Otherwise the task returns the chosen task

            else
               Self.Set_Agent_Message
                 (Message => (Message_Type      => Scheduler_Agent_Done,
                              Next_Agent        => Runnable_Queue,
                              Wake_Scheduler_At => Wake_Time));
            end if;
         end if;

         Yield_Processor_To_Kernel;
      end loop;
   end Run_Loop;

end Acton.Scheduler_Agents.Priority_Server;
